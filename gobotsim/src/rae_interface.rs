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
use ompas_rae_structs::job::{Job, JobType};
use ompas_rae_structs::platform::CtxPlatform;
use ompas_rae_structs::platform::RAEPlatform;
use ompas_rae_structs::rae_interface::RAEInterface;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::{lruntimeerror, wrong_n_args, wrong_type};
use std::borrow::Borrow;
use std::convert::TryInto;
use std::fs::File;
use std::net::SocketAddr;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::{fs, thread};
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
    pub headless: bool,
    pub sender_socket: Option<Sender<String>>,
    pub interface: RAEInterface,
    pub instance: Instance,
    pub domain: GodotDomain,
}

#[derive(Clone)]
pub enum GodotDomain {
    Lisp(String),
    Path(PathBuf),
}

impl Default for GodotDomain {
    fn default() -> Self {
        Self::Lisp("".to_string())
    }
}

impl From<String> for GodotDomain {
    fn from(s: String) -> Self {
        Self::Lisp(s)
    }
}

impl From<PathBuf> for GodotDomain {
    fn from(p: PathBuf) -> Self {
        Self::Path(p)
    }
}

impl PlatformGodot {
    pub fn new(domain: GodotDomain, headless: bool) -> Self {
        PlatformGodot {
            socket_info: Default::default(),
            headless,
            sender_socket: None,
            interface: Default::default(),
            instance: Default::default(),
            domain,
        }
    }
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
impl RAEPlatform for PlatformGodot {
    /// Initiliaze the godot platform of a the state (contains Arc to structs)
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
    async fn start_platform(&mut self, args: &[LValue]) -> LResult {
        let godot = match self.headless {
            true => "godot3-headless",
            false => "godot3",
        };

        let f1 = File::create("gobotsim.log").expect("couldn't create file");
        let f2 = File::create("gobotsim.log").expect("couldn't create file");

        let mut child = match args.len() {
            //default settings
            0 => Command::new(godot)
                .arg("--path")
                .arg(DEFAULT_PATH_PROJECT_GODOT)
                .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                .spawn()
                .expect("failed to execute process"),
            1 => {
                if let LValue::Symbol(s) = &args[0] {
                    Command::new(godot)
                        .args(s.split_whitespace())
                        .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                        .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) })
                        .spawn()
                        .expect("failed to execute process")
                } else {
                    return Err(wrong_type!(
                        "PlatformGodot::start_platform",
                        &args[0],
                        KindLValue::Symbol
                    ));
                }
            } //path of the project (absolute path)
            _ => {
                return Err(LRuntimeError::wrong_number_of_args(
                    "PlatformGodot::start_platform",
                    args,
                    0..1,
                ))
            } //Unexpected number of arguments
        };

        let mut killer = self
            .interface
            .killer
            .read()
            .await
            .as_ref()
            .unwrap()
            .subscribe();

        tokio::spawn(async move {
            //blocked on the reception of the end signal.
            killer.recv().await.expect("error receiving kill message");

            child.kill().expect("could not kill godot");
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
                        return Err(wrong_type!(
                            "PlatformGodot::open_com",
                            lv,
                            KindLValue::Symbol
                        ))
                    }
                };

                let port: usize = match &args[1] {
                    LValue::Number(n) => n.into(),
                    lv => {
                        return Err(wrong_type!(
                            "PlatformGodot::open_com",
                            lv,
                            KindLValue::Usize
                        ))
                    }
                };

                format!("{}:{}", addr, port).parse().unwrap()
            }
            _ => {
                return Err(LRuntimeError::wrong_number_of_args(
                    "PlatformGodot::open_com",
                    args,
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
        let state = self.interface.state.clone();
        let agenda = self.interface.agenda.clone();
        let instance = self.instance.clone();
        let killer = self.interface.killer.read().await.clone().unwrap();
        tokio::spawn(async move {
            task_tcp_connection(&socket_addr, rx, state, agenda, instance, killer).await
        });
        //println!("com opened with godot");
        Ok(LValue::Nil)
    }

    /// Function returning the domain of the simulation.
    /// The domain is hardcoded.
    async fn domain(&self) -> String {
        //GODOT_DOMAIN

        match &self.domain {
            GodotDomain::Lisp(l) => l.clone(),
            GodotDomain::Path(p) => fs::read_to_string(p).expect("could not read domain"),
        }
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
            1 => {
                self.instance
                    .instance_of(args[0].borrow().try_into()?)
                    .await
            }
            2 => {
                self.instance
                    .is_of_type(args[0].borrow().try_into()?, args[1].borrow().try_into()?)
                    .await
            }
            _ => Err(LRuntimeError::wrong_number_of_args(
                "godot::instance",
                args,
                1..2,
            )),
        }
    }

    async fn trigger_event(&self, args: &[LValue]) -> LResult {
        let (sender, mut rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        let event = Job::new(sender, args.into(), JobType::Event);
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
