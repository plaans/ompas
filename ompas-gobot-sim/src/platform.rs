use crate::default_gobot_sim_path;
use crate::platform_server::PlatformGobotSimService;
use crate::tcp::task_tcp_connection;
use crate::TOKIO_CHANNEL_SIZE;
use async_trait::async_trait;
use ompas_core::ompas::manager::platform::lisp_domain::LispDomain;
use ompas_core::ompas::manager::platform::platform_config::{InnerPlatformConfig, PlatformConfig};
use ompas_core::ompas::manager::platform::PlatformDescriptor;
use ompas_core::ompas_path;
use ompas_interface::platform_interface::platform_interface_server::PlatformInterfaceServer;
use ompas_language::interface::{
    DEFAULT_PLATFORM_SERVICE_IP, DEFAULT_PLATFROM_SERVICE_PORT, LOG_TOPIC_PLATFORM,
};
use ompas_language::process::PROCESS_TOPIC_OMPAS;
use ompas_middleware::logger::LogClient;
use ompas_middleware::{LogLevel, ProcessInterface};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use std::fs::File;
use std::net::SocketAddr;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::sleep;
use tonic::transport::Server;
use xshell::{cmd, Shell};

const DEFAULT_GOBOT_IP: &str = "127.0.0.1";
const DEFAULT_GOBOT_PORT: &str = "10000";
const PROCESS_GOBOT_SIM: &str = "__PROCESS_GOBOT_SIM__";
const PROCESS_SERVER_GRPC: &str = "__PROCESS_SERVER_GRPC__";
const GODOT_DOWNLOAD_URL: &str =
    "https://downloads.tuxfamily.org/godotengine/3.5/Godot_v3.5-stable_x11.64.zip";
const GODOT_ZIP_NAME: &str = "Godot_v3.5-stable_x11.64.zip";
const GODOT_HEADLESS_DOWNLOAD_URL: &str =
    "https://downloads.tuxfamily.org/godotengine/3.5/Godot_v3.5-stable_linux_headless.64.zip";
const GODOT_HEADLESS_ZIP_NAME: &str = "Godot_v3.5-stable_linux_headless.64.zip";

/// Struct used to bind RAE and Godot.
#[derive(Clone)]
pub struct PlatformGobotSim {
    pub service_info: SocketAddr,
    pub godot_tcp_info: SocketAddr,
    pub headless: bool,
    pub domain: LispDomain,
    pub config: String,
    pub log: LogClient,
}
impl Default for PlatformGobotSim {
    fn default() -> Self {
        Self {
            service_info: format!(
                "{}:{}",
                DEFAULT_PLATFORM_SERVICE_IP, DEFAULT_PLATFROM_SERVICE_PORT
            )
            .parse()
            .unwrap(),
            godot_tcp_info: format!("{}:{}", DEFAULT_GOBOT_IP, DEFAULT_GOBOT_PORT)
                .parse()
                .unwrap(),
            headless: false,
            domain: LispDomain::default(),
            config: "".to_string(),
            log: Default::default(),
        }
    }
}

impl PlatformGobotSim {
    pub fn new(domain: LispDomain, headless: bool, log: LogClient) -> Self {
        PlatformGobotSim {
            service_info: format!(
                "{}:{}",
                DEFAULT_PLATFORM_SERVICE_IP, DEFAULT_PLATFROM_SERVICE_PORT
            )
            .parse()
            .unwrap(),
            godot_tcp_info: format!("{}:{}", DEFAULT_GOBOT_IP, DEFAULT_GOBOT_PORT)
                .parse()
                .unwrap(),
            headless,
            domain,
            config: "".to_string(),
            log,
        }
    }

    pub async fn start_platform(&self, config: Option<String>) -> LResult {
        let bin_name = match self.headless {
            true => match self.godot3_headless_path() {
                None => {
                    self.install_godot3_headless();
                    self.godot3_headless_path().unwrap()
                }
                Some(s) => s,
            },
            false => match self.godot3_path() {
                None => {
                    self.install_godot3();
                    self.godot3_path().unwrap()
                }
                Some(s) => s,
            },
        };

        self.init_godot_project();

        let f1 = File::create("gobotsim.log").expect("couldn't create file");
        let f2 = File::create("gobotsim.log").expect("couldn't create file");

        let mut child = match config {
            Some(config) => Command::new(bin_name)
                .args(config.split_whitespace())
                .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                .spawn()
                .expect("failed to execute process"),
            None => match self.config.is_empty() {
                true => Command::new(bin_name)
                    .arg("--path")
                    .arg(default_gobot_sim_path())
                    .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                    .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                    .spawn()
                    .expect("failed to execute process"),
                false => Command::new(bin_name)
                    .args(self.config.split_whitespace())
                    .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                    .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                    .spawn()
                    .expect("failed to execute process"),
            },
        };

        let mut process =
            ProcessInterface::new(PROCESS_GOBOT_SIM, PROCESS_TOPIC_OMPAS, LOG_TOPIC_PLATFORM).await;

        tokio::spawn(async move {
            process.recv().await.expect("error receiving kill message");
            child.kill().expect("could not kill godot");
            process.log("Godot simulator killed", LogLevel::Info);
        });
        Ok(LValue::Nil)
    }

    /// Open the tcp communication on the right address:port
    pub async fn open_com(&self) -> LResult {
        let socket_addr = self.godot_tcp_info;

        let (tx_request, rx_request) = mpsc::unbounded_channel();
        let (tx_response, rx_response) = tokio::sync::broadcast::channel(TOKIO_CHANNEL_SIZE);
        let (tx_update, rx_update) = tokio::sync::broadcast::channel(TOKIO_CHANNEL_SIZE);

        let service = PlatformGobotSimService {
            command_request: tx_request,
            command_response: rx_response,
            state_update: rx_update,
        };
        let server_info: SocketAddr = self.socket().await;
        tokio::spawn(async move {
            let mut process =
                ProcessInterface::new(PROCESS_SERVER_GRPC, PROCESS_TOPIC_OMPAS, LOG_TOPIC_PLATFORM)
                    .await;

            //println!("Serving : {}", server_info);
            let server = Server::builder().add_service(PlatformInterfaceServer::new(service));
            tokio::select! {
                _ = process.recv() => {
                    //process.die().await;
                }
                _ = server.serve(server_info) => {

                }
            }
        });

        sleep(Duration::from_secs(1)).await;

        //Used to verify if tcp sender works
        //tokio::spawn(async move { tx.send(TEST_TCP.to_string()).await });

        //println!("godot launching...");
        //println!("godot launched!");
        tokio::spawn(async move {
            task_tcp_connection(&socket_addr, rx_request, tx_response, tx_update).await
        });
        //println!("com opened with godot");
        Ok(LValue::Nil)
    }

    pub fn godot3_path(&self) -> Option<PathBuf> {
        let sh = Shell::new().unwrap();
        match cmd!(sh, "which godot3.5").quiet().read() {
            Ok(s) => Some(s.into()),
            Err(_) => {
                let ompas_path = ompas_path();
                let path =
                    PathBuf::from(format!("{}/ompas-gobot-sim/gobot-bin/godot3.5", ompas_path));
                if path.is_file() {
                    Some(path)
                } else {
                    None
                }
            }
        }
    }

    pub fn godot3_headless_path(&self) -> Option<PathBuf> {
        let sh = Shell::new().unwrap();
        match cmd!(sh, "which godot3.5-headless").quiet().read() {
            Ok(s) => Some(s.into()),
            Err(_) => {
                let ompas_path = ompas_path();
                let path = PathBuf::from(format!(
                    "{}/ompas-gobot-sim/gobot-bin/godot3.5-headless",
                    ompas_path
                ));
                if path.is_file() {
                    Some(path)
                } else {
                    None
                }
            }
        }
    }

    fn check_dependencies(&self) {
        let sh = Shell::new().unwrap();
        print!("Checking dependencies wget unzip...");
        if cmd!(sh, "which wget").quiet().read().is_err() {
            panic!("Cannot install godot3.5 : missing wget, please install with command \n sudo apt install wget")
        }
        if cmd!(sh, "which unzip").quiet().read().is_err() {
            panic!("Cannot install godot3.5 : missing unzip, please install with command \n sudo apt install unzip")
        }
        println!("Ok!")
    }

    /// Install the executable for godot3 if not yet available on the system.
    fn install_godot3_headless(&self) {
        println!("Installing godot3.5-headless on your computer to run the platform");
        let sh = Shell::new().unwrap();

        self.check_dependencies();
        sh.change_dir(format!("{}/ompas-gobot-sim", ompas_path()));
        sh.create_dir("gobot-bin")
            .expect("Could not create dir gobot-bin");
        sh.change_dir("gobot-bin");

        //Install the headless version
        cmd!(sh, "wget {GODOT_HEADLESS_DOWNLOAD_URL}")
            .run()
            .unwrap();
        cmd!(sh, "unzip {GODOT_HEADLESS_ZIP_NAME}").run().unwrap();
        cmd!(
            sh,
            "mv Godot_v3.5-stable_linux_headless.64 godot3.5-headless"
        )
        .quiet()
        .run()
        .unwrap();
        println!(
            "godot3.5-headless has been installed in {}",
            sh.current_dir().to_str().unwrap()
        )
    }

    fn install_godot3(&self) {
        println!("Installing godot3.5 on your computer to run the platform...");
        let sh = Shell::new().unwrap();

        self.check_dependencies();
        sh.change_dir(format!("{}/ompas-gobot-sim", ompas_path()));
        sh.create_dir("gobot-bin")
            .expect("Could not create dir gobot-bin");
        sh.change_dir("gobot-bin");

        //Install the standard version
        cmd!(sh, "wget {GODOT_DOWNLOAD_URL}").run().unwrap();
        cmd!(sh, "unzip {GODOT_ZIP_NAME}").run().unwrap();
        cmd!(sh, "mv Godot_v3.5-stable_x11.64 godot3.5")
            .run()
            .unwrap();
        println!(
            "godot3.5 has been installed in {}",
            sh.current_dir().to_str().unwrap()
        )
    }

    /// Setup the gobot-sim project in godot3 to launch it
    pub fn init_godot_project(&self) {
        // if .import directory does not exist
        // godot3 -e --quit (imports the project and quits).
        // Needed to generate the .import directory
        let sh = Shell::new().unwrap();
        sh.change_dir(format!("{}/ompas-gobot-sim/gobot-sim/simu", ompas_path()));

        if !sh.path_exists(format!("{}/.import", sh.current_dir().to_str().unwrap())) {
            print!("Init gobot-sim project in godot...");
            let gd_path = self.godot3_path().unwrap();
            let _ = cmd!(sh, "timeout 7 {gd_path} -e").quiet().run();
            println!("Ok!");
        }
    }

    pub fn set_server_info(&mut self, socket_info: SocketAddr) {
        self.service_info = socket_info;
    }

    pub fn get_server_info(&self) -> &SocketAddr {
        &self.service_info
    }
}

#[async_trait]
impl PlatformDescriptor for PlatformGobotSim {
    async fn start(&self, config: PlatformConfig) {
        let config: Option<String> = match config.get_inner::<String>() {
            InnerPlatformConfig::String(s) => Some(s.to_string()),
            InnerPlatformConfig::Any(s) => Some(s.to_string()),
            InnerPlatformConfig::None => None,
        };

        match self.start_platform(config).await {
            Ok(_) => {
                self.log.info("Successfully started platform.");
                match self.open_com().await {
                    Ok(_) => {
                        self.log.info("Successfully open com with platform.");
                    }
                    Err(e) => {
                        self.log
                            .error(format!("Error opening com with platform: {e}."));
                    }
                }
            }
            Err(e) => {
                self.log.error(format!("Error starting platform: {e}"));
            }
        }
    }

    async fn stop(&self) {
        //
    }

    async fn domain(&self) -> LispDomain {
        self.domain.clone()
    }

    async fn module(&self) -> Option<LModule> {
        None
    }

    async fn socket(&self) -> SocketAddr {
        self.service_info
    }
}

#[derive(Clone, Default)]
struct GodotCtx {}

impl From<GodotCtx> for LModule {
    fn from(m: GodotCtx) -> Self {
        LModule::new(m, "GobotSimModule", "")
    }
}
