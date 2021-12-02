use crate::mod_godot::{SocketInfo, DEFAULT_PATH_PROJECT_GODOT};
use crate::serde::{
    GodotMessageSerde, GodotMessageSerdeData, GodotMessageType, SerdeActionId, SerdeRobotCommand,
};
use crate::tcp::{task_tcp_connection, TEST_TCP};
use crate::TOKIO_CHANNEL_SIZE;
use async_trait::async_trait;
use core::time;
use im::{HashMap, HashSet};
use ompas_acting::rae::context::actions_progress::{ActionsProgress, Status};
use ompas_acting::rae::context::rae_state::{RAEState, StateType, KEY_DYNAMIC, KEY_STATIC};
use ompas_acting::rae::module::mod_rae_exec::{
    CtxPlatform, RAEInterface, RAE_GET_STATE_VARIBALE, RAE_LAUNCH_PLATFORM,
};
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::*;
use ompas_utils::task_handler;
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
        match self.inner.read().await.get(&_type) {
            Some(vec) => Ok(vec.contains(&instance).into()),
            None => Ok(false.into()),
        }
    }

    pub async fn instance_of(&self, _type: String) -> LResult {
        match self.inner.read().await.get(&_type) {
            Some(set) => Ok(set.clone().iter().cloned().collect::<Vec<String>>().into()),
            None => Ok(Nil),
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
    pub status: ActionsProgress,
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
    async fn init(&mut self, state: RAEState, status: ActionsProgress) {
        self.state = state;
        self.status = status;
    }

    /// Executes a command on the platform. The command is sent via tcp.
    async fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError> {
        let gs = GodotMessageSerde {
            _type: GodotMessageType::RobotCommand,
            data: GodotMessageSerdeData::RobotCommand(SerdeRobotCommand {
                command_info: LValue::List(args.to_vec()).into(),
                temp_id: command_id,
            }),
        };

        self.status
            .status
            .write()
            .await
            .insert(command_id, Status::Pending);

        //println!("action status created");

        let command = serde_json::to_string(&gs).unwrap();

        //println!("(in exec-command) : {}", command);

        let sender = match self.get_sender_socket() {
            None => {
                return Err(SpecialError(
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
    async fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError> {
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
                NameTypeLValue::Number,
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
                return Err(SpecialError(
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

    /// Returns the state of godot. Arg is either 'static' or 'dynamic'
    async fn get_state(&self, args: &[LValue]) -> Result<LValue, LError> {
        let _type = match args.len() {
            0 => None,
            1 => match &args[0] {
                LValue::Symbol(s) => match s.as_str() {
                    KEY_STATIC => Some(StateType::Static),
                    KEY_DYNAMIC => Some(StateType::Dynamic),
                    _ => {
                        let result1 = Err(SpecialError(
                            "PlatformGodot::get_state",
                            format!("Expected keywords {} or {}", KEY_STATIC, KEY_DYNAMIC),
                        ));
                        return result1;
                    }
                },
                lv => {
                    return Err(WrongType(
                        "PlatformGodot::get_state",
                        lv.clone(),
                        lv.into(),
                        NameTypeLValue::Symbol,
                    ))
                }
            },
            _ => {
                return Err(WrongNumberOfArgument(
                    "PlatformGodot::get_state",
                    args.into(),
                    args.len(),
                    0..1,
                ))
            }
        };

        //println!("state type: {:?}", _type);

        //let handle = tokio::runtime::Handle::current();
        //let state = self.state.clone();

        let result = self.state.get_state(_type).await;
        Ok(result.into_map())
    }

    /// Returns the value of a state variable.
    /// args contains the key of the state variable.
    async fn get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError> {
        if args.is_empty() {
            return Err(WrongNumberOfArgument(
                RAE_GET_STATE_VARIBALE,
                args.into(),
                0,
                1..std::usize::MAX,
            ));
        }
        let key: LValueS = if args.len() > 1 {
            LValue::from(args).into()
        } else {
            args[0].clone().into()
        };

        //println!("key: {}", key);
        let c_state = self.state.clone();
        let state = c_state.get_state(None).await;

        //println!("state: {:?}", state);

        let value = state.inner.get(&key).unwrap_or(&LValueS::Bool(false));
        //println!("value: {}", value);

        Ok(value.into())
    }

    /// Return the status of all the actions.
    async fn get_status(&self, _: &[LValue]) -> Result<LValue, LError> {
        let status = self.status.status.clone();

        let status = status.read().await;

        let mut string = "Action(s) Status\n".to_string();

        for e in status.iter() {
            string.push_str(format!("- {}: {}\n", e.0, e.1).as_str())
        }

        Ok(LValue::String(string))
    }

    /// Launch the platform godot and open the tcp communication
    async fn launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError> {
        let (args_start, args_open) = match args.len() {
            0 => (&args[0..0], &args[0..0]),
            1 => (&args[0..1], &args[0..0]),
            2 => (&args[0..1], &args[1..2]),
            _ => {
                return Err(WrongNumberOfArgument(
                    RAE_LAUNCH_PLATFORM,
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
    async fn start_platform(&self, args: &[LValue]) -> Result<LValue, LError> {
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
                        NameTypeLValue::Symbol,
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
            tokio::time::sleep(Duration::from_millis(100)).await;
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
    async fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError> {
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
                            NameTypeLValue::Symbol,
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
                            NameTypeLValue::Usize,
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
        let status = self.status.clone();
        let instance = self.instance.clone();
        tokio::spawn(async move {
            task_tcp_connection(&socket_addr, rx, state, status, instance).await
        });
        //println!("com opened with godot");
        Ok(LValue::Nil)
    }

    /// Return the status of a specific action.
    async fn get_action_status(&self, action_id: &usize) -> Status {
        self.status.get_action_status(action_id).await
    }

    async fn set_status(&self, _: usize, _: Status) {
        todo!()
    }

    /// Function returning the domain of the simulation.
    /// The domain is hardcoded.
    async fn domain(&self) -> &'static str {
        //GODOT_DOMAIN
        "(read godot_domain/init.lisp)"
    }

    //1 arg: return all instances of a type
    //2 arg: check if an instance is of a certain type
    async fn instance(&self, args: &[LValue]) -> LResult {
        match args.len() {
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

impl GetModule for GodotCtx {
    fn get_module(self) -> Module {
        Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: "".to_string(),
        }
    }
}
