use crate::platform_server::PlatformGobotSimService;
use crate::tcp::task_tcp_connection;
use crate::PROCESS_TOPIC_GOBOT_SIM;
use crate::{DEFAULT_PATH_PROJECT_GODOT, TOKIO_CHANNEL_SIZE};
use async_trait::async_trait;
use ompas_core::ompas::scheme::exec::platform::lisp_domain::LispDomain;
use ompas_core::ompas::scheme::exec::platform::platform_config::{
    InnerPlatformConfig, PlatformConfig,
};
use ompas_core::ompas::scheme::exec::platform::PlatformDescriptor;
use ompas_interface::platform_interface::platform_interface_server::PlatformInterfaceServer;
use ompas_language::interface::{
    DEFAULT_PLATFORM_SERVICE_IP, DEFAULT_PLATFROM_SERVICE_PORT, LOG_TOPIC_PLATFORM,
    PROCESS_TOPIC_PLATFORM,
};
use ompas_middleware::logger::LogClient;
use ompas_middleware::ProcessInterface;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use std::fs::File;
use std::net::SocketAddr;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::process::{Command, Stdio};
use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::sleep;
use tonic::transport::Server;

const DEFAULT_GOBOT_IP: &str = "127.0.0.1";
const DEFAULT_GOBOT_PORT: &str = "10000";
const PROCESS_GOBOT_SIM: &str = "__PROCESS_GOBOT_SIM__";
const PROCESS_SERVER_GRPC: &str = "__PROCESS_SERVER_GRPC__";

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
        let godot = match self.headless {
            true => "godot3-headless",
            false => "godot3",
        };

        let f1 = File::create("gobotsim.log").expect("couldn't create file");
        let f2 = File::create("gobotsim.log").expect("couldn't create file");

        let mut child = match config {
            Some(config) => Command::new(godot)
                .args(config.split_whitespace())
                .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                .spawn()
                .expect("failed to execute process"),
            None => match self.config.is_empty() {
                true => Command::new(godot)
                    .arg("--path")
                    .arg(DEFAULT_PATH_PROJECT_GODOT)
                    .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                    .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                    .spawn()
                    .expect("failed to execute process"),
                false => Command::new(godot)
                    .args(self.config.split_whitespace())
                    .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                    .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                    .spawn()
                    .expect("failed to execute process"),
            },
        };

        let mut process = ProcessInterface::new(
            PROCESS_GOBOT_SIM,
            PROCESS_TOPIC_GOBOT_SIM,
            LOG_TOPIC_PLATFORM,
        )
        .await;

        tokio::spawn(async move {
            //blocked on the reception of the end signal.
            process.recv().await.expect("error receiving kill message");
            child.kill().expect("could not kill godot");
            //process.die().await;
        });
        Ok(LValue::Nil)
    }

    /// Open the tcp communication on the right address:port
    pub async fn open_com(&self) -> LResult {
        let socket_addr = self.godot_tcp_info;

        let (tx_request, rx_request) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let (tx_response, rx_response) = tokio::sync::broadcast::channel(TOKIO_CHANNEL_SIZE);
        let (tx_update, rx_update) = tokio::sync::broadcast::channel(TOKIO_CHANNEL_SIZE);

        let service = PlatformGobotSimService {
            command_request: tx_request,
            command_response: rx_response,
            state_update: rx_update,
        };
        let server_info: SocketAddr = self.socket().await;
        tokio::spawn(async move {
            let mut process = ProcessInterface::new(
                PROCESS_SERVER_GRPC,
                PROCESS_TOPIC_PLATFORM,
                LOG_TOPIC_PLATFORM,
            )
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
                self.log.info("Successfully started platform.").await;
                match self.open_com().await {
                    Ok(_) => {
                        self.log.info("Successfully open com with platform.").await;
                    }
                    Err(e) => {
                        self.log
                            .error(format!("Error opening com with platform: {e}."))
                            .await;
                    }
                }
            }
            Err(e) => {
                self.log
                    .error(format!("Error starting platform: {e}"))
                    .await;
            }
        }
    }

    async fn stop(&self) {
        self.log
            .info("Process Gobot-Sim killed via subscriptions of its different processes.")
            .await;
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
