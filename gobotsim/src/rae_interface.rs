use crate::language::GODOT_LAUNCH_PLATFORM;
use crate::mod_godot::{SocketInfo, DEFAULT_PATH_PROJECT_GODOT};
use crate::serde::{
    GodotMessageSerde, GodotMessageSerdeData, GodotMessageType, SerdeActionId, SerdeRobotCommand,
};
use crate::tcp::{task_tcp_connection, TEST_TCP};
use crate::TOKIO_CHANNEL_SIZE;
use async_trait::async_trait;
use core::time;
use im::{HashMap, HashSet};
use ompas_rae_scheme::rae_exec::{CtxPlatform, RAEInterface};
use ompas_rae_structs::exec_context::rae_state::RAEState;
use ompas_rae_structs::refinement::Agenda;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::lerror::LResult;
use sompas_structs::lerror::LRuntimeError::{Anyhow, WrongNumberOfArgument, WrongType};
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::typelvalue::KindLValue;
use sompas_utils::task_handler;
use std::convert::TryInto;
use std::net::SocketAddr;
use std::process::Command;
use std::sync::Arc;
use std::thread;
use std::time::Duration;
use tokio::sync::mpsc::Sender;
use tokio::sync::{mpsc, RwLock};

#[derive(Default, Clone)]
pub struct Instance {
    inner: Arc<RwLock<HashMap<String, HashSet<String>>>>,
}

impl Instance {
    pub async fn add_instance_of(&self, instance: String, _type: String) {
        let mut locked = self.inner.write().await;

        match locked.get_mut(&_type) {
            Some(vec) => {
                vec.insert(instance);
            }
            None => {
                let mut set = HashSet::new();
                set.insert(instance);
                locked.insert(_type, set);
            }
        };
    }

    pub async fn add_type(&self, _type: String) {
        self.inner.write().await.insert(_type, HashSet::default());
    }
}

impl Instance {
    pub async fn is_of_type(&self, instance: String, _type: String) -> LResult {
        //println!("instances : {:?}", self.inner.read().await);

        match self.inner.read().await.get(&_type) {
            Some(vec) => {
                //println!("type exist");
                Ok(vec.contains(&instance).into())
            }
            None => Ok(false.into()),
        }
    }

    pub async fn instance_of(&self, _type: String) -> LResult {
        match self.inner.read().await.get(&_type) {
            Some(set) => Ok(set.clone().iter().cloned().collect::<Vec<String>>().into()),
            None => Ok(LValue::Nil),
        }
    }
}

/// Struct used to bind RAE and Godot.
#[derive(Default, Clone)]
pub struct PlatformGodot {
    pub socket_info: SocketInfo,
    pub sender_socket: Option<Sender<String>>,
    pub state: RAEState,
    pub instance: Instance,
    pub agenda: Agenda,
}

impl PlatformGodot {
    pub fn set_socket_info(&mut self, socket_info: SocketInfo) {
        self.socket_info = socket_info;
    }

    pub fn get_socket_info(&self) -> &SocketInfo {
        &self.socket_info
    }

    pub fn get_sender_socket(&self) -> &Option<Sender<String>> {
        &self.sender_socket
    }
}

#[async_trait]
impl RAEInterface for PlatformGodot {
    /// Initiliaze the godot platform of a the state (contains Arc to structs)
    /// and ActionsProgress (contains also Arc to structs)
    async fn init(&mut self, state: RAEState, agenda: Agenda) {
        self.state = state;
        self.agenda = agenda;
    }

    /// Executes a command on the platform. The command is sent via tcp.
    async fn exec_command(&self, args: &[LValue], command_id: usize) -> LResult {
        let gs = GodotMessageSerde {
            _type: GodotMessageType::RobotCommand,
            data: GodotMessageSerdeData::RobotCommand(SerdeRobotCommand {
                command_info: LValue::List(args.to_vec()).into(),
                temp_id: command_id,
            }),
        };

        //println!("action status created");

        let command = serde_json::to_string(&gs).unwrap();

        //println!("(in exec-command) : {}", command);

        let sender = match self.get_sender_socket() {
            None => {
                return Err(Anyhow(
                    "PlatformGodot::exec_command",
                    "ctx godot has no sender to simulation, try first to (open-com-godot)"
                        .to_string(),
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
            return Err(WrongNumberOfArgument(
                "PlatformGodot::cancel_command",
                args.into(),
                args.len(),
                1..1,
            ));
        }

        let id: usize = if let LValue::Number(n) = &args[0] {
            n.into()
        } else {
            return Err(WrongType(
                "PlatformGodot::cancel_command",
                args[0].clone(),
                (&args[0]).into(),
                KindLValue::Number,
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
                return Err(Anyhow(
                    "PlatformGodot::cancel_command",
                    "ctx godot has no sender to simulation, try first to (open-com-godot)"
                        .to_string(),
                ))
            }
            Some(s) => s.clone(),
        };

        sender
            .send(command)
            .await
            .expect("couldn't send via channel");

        Ok(LValue::Nil)
    }

    /// Launch the platform godot and open the tcp communication
    async fn launch_platform(&mut self, args: &[LValue]) -> LResult {
        let (args_start, args_open) = match args.len() {
            0 => (&args[0..0], &args[0..0]),
            1 => (&args[0..1], &args[0..0]),
            2 => (&args[0..1], &args[1..2]),
            _ => {
                return Err(WrongNumberOfArgument(
                    GODOT_LAUNCH_PLATFORM,
                    args.into(),
                    args.len(),
                    0..2,
                ))
            }
        };
        self.start_platform(args_start).await?;
        thread::sleep(time::Duration::from_millis(1000));
        self.open_com(args_open).await
    }

    /// Start the platform (start the godot process and launch the simulation)
    async fn start_platform(&mut self, args: &[LValue]) -> LResult {
        match args.len() {
            //default settings
            0 => {
                Command::new("gnome-terminal")
                    .args(&["--title", "GODOT 3 Terminal"])
                    .arg("--")
                    .arg("godot3")
                    .arg("--path")
                    .arg(DEFAULT_PATH_PROJECT_GODOT)
                    .spawn()
                    .expect("failed to execute process");
            }
            1 => {
                if let LValue::Symbol(s) = &args[0] {
                    /*println!(
                        "PlatformGodot::start_platfrom: start godot with config {}({} args)",
                        s,
                        s.split_whitespace().count()
                    );*/
                    Command::new("gnome-terminal")
                        .arg("--")
                        .arg("godot3")
                        .args(s.split_whitespace())
                        .spawn()
                        .expect("failed to execute process");
                } else {
                    return Err(WrongType(
                        "PlatformGodot::start_platform",
                        args[0].clone(),
                        (&args[0]).into(),
                        KindLValue::Symbol,
                    ));
                }
            } //path of the project (absolute path)
            _ => {
                return Err(WrongNumberOfArgument(
                    "PlatformGodot::start_platform",
                    args.into(),
                    args.len(),
                    0..1,
                ))
            } //Unexpected number of arguments
        };

        tokio::spawn(async {
            tokio::time::sleep(Duration::from_millis(1000)).await;
            let result = Command::new("pidof")
                .arg("godot3")
                .output()
                .expect("could not run command.");
            let pids = String::from_utf8(result.stdout).expect("could not convert into string");
            //println!("tail pids: {}", pids);
            let logger_pid = if !pids.is_empty() {
                let logger_pid = pids
                    .split_whitespace()
                    .next()
                    .expect("could not get first pid")
                    .to_string();
                //println!("logger pid: {}", logger_pid);
                Some(logger_pid)
            } else {
                None
            };

            //blocked on the reception of the end signal.
            task_handler::subscribe_new_task()
                .recv()
                .await
                .expect("could not receive from task handler");

            if let Some(pid) = logger_pid {
                Command::new("kill")
                    .args(&["-9", pid.as_str()])
                    .spawn()
                    .expect("Command failed.");
            }
            println!("process godot killed")
        });
        Ok(LValue::Nil)
    }

    /// Open the tcp communication on the right address:port
    async fn open_com(&mut self, args: &[LValue]) -> LResult {
        let socket_addr: SocketAddr = match args.len() {
            0 => "127.0.0.1:10000".parse().unwrap(),
            2 => {
                let addr = match &args[0] {
                    LValue::Symbol(s) => s.clone(),
                    lv => {
                        return Err(WrongType(
                            "PlatformGodot::open_com",
                            lv.clone(),
                            lv.into(),
                            KindLValue::Symbol,
                        ))
                    }
                };

                let port: usize = match &args[1] {
                    LValue::Number(n) => n.into(),
                    lv => {
                        return Err(WrongType(
                            "PlatformGodot::open_com",
                            lv.clone(),
                            lv.into(),
                            KindLValue::Usize,
                        ))
                    }
                };

                format!("{}:{}", addr, port).parse().unwrap()
            }
            _ => {
                return Err(WrongNumberOfArgument(
                    "PlatformGodot::open_com",
                    args.into(),
                    args.len(),
                    0..2,
                ))
            }
        };

        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        self.sender_socket = Some(tx.clone());

        //Used to verify if tcp sender works
        tokio::spawn(async move { tx.send(TEST_TCP.to_string()).await });

        //println!("godot launching...");
        //println!("godot launched!");
        let state = self.state.clone();
        let status = self.agenda.clone();
        let instance = self.instance.clone();
        tokio::spawn(async move {
            task_tcp_connection(&socket_addr, rx, state, status, instance).await
        });
        //println!("com opened with godot");
        Ok(LValue::Nil)
    }

    /// Function returning the domain of the simulation.
    /// The domain is hardcoded.
    async fn domain(&self) -> &'static str {
        //GODOT_DOMAIN
        "(read godot_domain/init.lisp)"
    }

    //0 arg: return a map of all instances
    //1 arg: return all instances of a type
    //2 args: check if an instance is of a certain type
    async fn instance(&self, args: &[LValue]) -> LResult {
        match args.len() {
            0 => {
                let map_instances: im::HashMap<String, HashSet<String>> =
                    self.instance.inner.read().await.clone();
                let mut map: im::HashMap<LValue, LValue> = Default::default();
                for (_type, instances) in map_instances {
                    let value = instances.iter().map(LValue::from).collect::<Vec<LValue>>();
                    map.insert(_type.into(), value.into());
                }

                Ok(map.into())
            }
            1 => self.instance.instance_of((&args[0]).try_into()?).await,
            2 => {
                self.instance
                    .is_of_type((&args[0]).try_into()?, (&args[1]).try_into()?)
                    .await
            }
            _ => Err(WrongNumberOfArgument(
                "godot::instance",
                args.into(),
                args.len(),
                1..2,
            )),
        }
    }

    fn context_platform(&self) -> CtxPlatform {
        CtxPlatform::new(GodotCtx::default())
    }
}

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
