use crate::platform_server::PlatformGobotSimService;
use crate::tcp::task_tcp_connection;
use crate::PROCESS_TOPIC_GOBOT_SIM;
use crate::{DEFAULT_PATH_PROJECT_GODOT, TOKIO_CHANNEL_SIZE};
use async_trait::async_trait;
use map_macro::set;
use ompas_middleware::ompas_log::{LogMessage, LOGGER};
use ompas_middleware::{LogLevel, ProcessInterface};
use ompas_rae_core::{LOG_TOPIC_ACTING, PROCESS_TOPIC_ACTING};
use ompas_rae_interface::platform::PlatformModule;
use ompas_rae_interface::platform::{Domain, PlatformDescriptor};
use ompas_rae_interface::platform_interface::platform_interface_server::PlatformInterfaceServer;
use ompas_rae_interface::PROCESS_TOPIC_PLATFORM;
use ompas_rae_interface::{DEFAULT_PLATFORM_SERVICE_IP, DEFAULT_PLATFROM_SERVICE_PORT};
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
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
    pub domain: Domain,
    pub config: String,
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
            domain: Domain::default(),
            config: "".to_string(),
        }
    }
}

impl PlatformGobotSim {
    pub fn new(domain: Domain, headless: bool) -> Self {
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
            domain: domain,
            config: "".to_string(),
        }
    }

    pub async fn start_platform(&self) -> LResult {
        let godot = match self.headless {
            true => "godot3-headless",
            false => "godot3",
        };

        let f1 = File::create("gobotsim.log").expect("couldn't create file");
        let f2 = File::create("gobotsim.log").expect("couldn't create file");

        let mut child = match self.config.is_empty() {
            //default settings
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
        };

        let mut process = ProcessInterface::new(
            PROCESS_GOBOT_SIM.to_string(),
            set! {PROCESS_TOPIC_GOBOT_SIM, PROCESS_TOPIC_PLATFORM},
        )
        .await;

        tokio::spawn(async move {
            //blocked on the reception of the end signal.
            process.recv().await.expect("error receiving kill message");

            child.kill().expect("could not kill godot");
            println!("process godot killed")
        });
        Ok(LValue::Nil)
    }

    /// Open the tcp communication on the right address:port
    pub async fn open_com(&self) -> LResult {
        let socket_addr = self.godot_tcp_info.clone();

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
                set! {PROCESS_TOPIC_PLATFORM, PROCESS_TOPIC_ACTING},
            )
            .await;

            println!("Serving : {}", server_info);
            let server = Server::builder().add_service(PlatformInterfaceServer::new(service));
            tokio::select! {
                _ = process.recv() => {
                    process.die().await;
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
    async fn start(&self) {
        let topic_id = LOGGER.subscribe_to_topic(LOG_TOPIC_ACTING).await;
        match self.start_platform().await {
            Ok(_) => {
                LOGGER
                    .log(LogMessage::new(
                        LogLevel::Debug,
                        "Platform::start",
                        set! {topic_id},
                        "Successfully started platform.",
                    ))
                    .await;
                match self.open_com().await {
                    Ok(_) => {
                        LOGGER
                            .log(LogMessage::new(
                                LogLevel::Debug,
                                "Platform::start",
                                set! {topic_id},
                                "Successfully open com with platform.",
                            ))
                            .await;
                    }
                    Err(e) => {
                        LOGGER
                            .log(LogMessage::new(
                                LogLevel::Debug,
                                "Platform::start",
                                set! {topic_id},
                                format!("Error opening com with platform: {}.", e),
                            ))
                            .await;
                    }
                }
            }
            Err(e) => {
                LOGGER
                    .log(LogMessage::new(
                        LogLevel::Debug,
                        "Platform::start".to_string(),
                        set! {topic_id},
                        format!("Error starting platform: {}", e),
                    ))
                    .await;
            }
        }
    }

    async fn stop(&self) {
        panic!("Not implemented yet")
    }

    async fn domain(&self) -> Domain {
        self.domain.clone()
    }

    async fn module(&self) -> Option<PlatformModule> {
        None
    }

    async fn socket(&self) -> SocketAddr {
        self.service_info.clone()
    }
}

/*/// Initiliaze the godot platform of a the state (contains Arc to structs)
/// and ActionsProgress (contains also Arc to structs)
async fn init(&mut self, interface: RAEInterface) {
    self.interface = interface
}

/// Executes a command on the platform. The command is sent via tcp.
async fn exec_command(&self, args: &[LValue], command_id: usize) -> LResult {
    let _type = {
        if args[0] == "process".into() {
            GodotMessageType::MachineCommand
        } else {
            GodotMessageType::RobotCommand
        }
    };

    let gs = GodotMessageSerde {
        _type,
        data: GodotMessageSerdeData::RobotCommand(SerdeRobotCommand {
            command_info: LValue::from(args).try_into().unwrap(),
            temp_id: command_id,
        }),
    };

    //println!("action status created");

    let command = serde_json::to_string(&gs).unwrap();

    //println!("(in exec-command) : {}", command);

    let sender = match self.get_sender_socket() {
        None => {
            return Err(lruntimeerror!(
                "PlatformGodot::exec_command",
                "ctx godot has no sender to simulation, try first to (open-com-godot)"
            ))
        }
        Some(s) => s.clone(),
    };

    sender
        .send(command)
        .await
        .expect("couldn't send via channel");
    //println!("command sent!");

    Ok(LValue::Nil)
}

/// Sends to godot a cancel command.
async fn cancel_command(&self, args: &[LValue]) -> LResult {
    if args.len() != 1 {
        return Err(wrong_n_args!("PlatformGodot::cancel_command", args, 1));
    }

    let id: usize = if let LValue::Number(n) = &args[0] {
        n.into()
    } else {
        return Err(wrong_type!(
            "PlatformGodot::cancel_command",
            &args[0],
            KindLValue::Number
        ));
    };

    let gs = GodotMessageSerde {
        _type: GodotMessageType::CancelRequest,
        data: GodotMessageSerdeData::ActionId(SerdeActionId { action_id: id }),
    };

    let command = serde_json::to_string(&gs).unwrap();

    //println!("(in exec-command) : {}", command);

    let sender = match self.get_sender_socket() {
        None => {
            return Err(lruntimeerror!(
                "PlatformGodot::cancel_command",
                "ctx godot has no sender to simulation, try first to (open-com-godot)"
            ))
        }
        Some(s) => s.clone(),
    };

    match sender.try_send(command) {
        Ok(_) => Ok(LValue::Nil),
        Err(_) => Err(LRuntimeError::new(
            "gobotsim::cancel_command",
            "tcp channel is closed",
        )),
    }
}

/// Launch the platform godot and open the tcp communication
async fn launch_platform(&mut self, args: &[LValue]) -> LResult {
    let (args_start, args_open) = match args.len() {
        0 => (&args[0..0], &args[0..0]),
        1 => (&args[0..1], &args[0..0]),
        2 => (&args[0..1], &args[1..2]),
        _ => {
            return Err(LRuntimeError::wrong_number_of_args(
                GODOT_LAUNCH_PLATFORM,
                args,
                0..2,
            ))
        }
    };
    self.start_platform(args_start).await?;
    thread::sleep(time::Duration::from_millis(1000));
    self.open_com(args_open).await
}

/// Start the platform (start the godot process and launch the simulation)

/// Function returning the domain of the simulation.
/// The domain is hardcoded.
async fn domain(&self) -> String {
    //GODOT_DOMAIN

    match &self.domain {
        GodotDomain::Lisp(l) => l.clone(),
        GodotDomain::Path(p) => fs::read_to_string(p).expect("could not read domain"),
    }
}

async fn trigger_event(&self, args: &[LValue]) -> LResult {
    let (sender, mut rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let event = Job::new(sender, args.into());
    let sender = self
        .interface
        .command_tx
        .read()
        .await
        .as_ref()
        .unwrap()
        .clone();

    tokio::spawn(async move {
        sender
            .send(event.into())
            .await
            .expect("could not send job to rae");
    });

    Ok(rx.recv().await.unwrap().into())
}

async fn stop_platform(&self) {
    println!("stopping gobot-sim...")
}

fn context_platform(&self) -> CtxPlatform {
    CtxPlatform::new(GodotCtx::default())
}*/

#[derive(Clone, Default)]
struct GodotCtx {}

impl IntoModule for GodotCtx {
    fn into_module(self) -> Module {
        Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: "".to_string(),
        }
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}
