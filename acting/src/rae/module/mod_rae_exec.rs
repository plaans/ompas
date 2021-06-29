use crate::rae::context::{
    ActionId, ActionsProgress, Agenda, SelectOption, Status, RAE_STATE_FUNCTION_LIST,
};
use crate::rae::module::domain::*;
use crate::rae::state::{
    ActionStatus, RAEState, StateType, KEY_DYNAMIC, KEY_INNER_WORLD, KEY_STATIC,
};
use ompas_lisp::core::LEnv;
use ompas_lisp::functions::union_map;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::*;
use ompas_lisp::structs::*;
use ompas_modules::doc::{Documentation, LHelp};
use std::any::Any;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard};
use std::thread;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::Mutex;

/*
LANGUAGE
 */

//manage facts
pub const RAE_ASSERT: &str = "assert";
pub const RAE_ASSERT_SHORT: &str = "+>";
pub const RAE_RETRACT: &str = "retract";
pub const RAE_RETRACT_SHORT: &str = "->";
pub const RAE_AWAIT: &str = "rae-await";

//RAE Interface with a platform
pub const RAE_EXEC_COMMAND: &str = "rae-exec-command";
pub const RAE_GET_STATE: &str = "rae-get-state";
pub const RAE_GET_STATE_VARIBALE: &str = "rae-get-state-variable";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_OPEN_COM_PLATFORM: &str = "rae-open-com-platform";
pub const RAE_START_PLATFORM: &str = "rae-start-platform";
pub const RAE_GET_STATUS: &str = "rae-get-status";
pub const RAE_CANCEL_COMMAND: &str = "rae-cancel-command";

///Context that will contains primitives for the RAE executive
pub struct CtxRaeExec {
    //pub stream: JobStream,
    pub actions_progress: ActionsProgress,
    pub state: RAEState,
    pub platform_interface: Box<dyn RAEInterface>,
}

impl Default for CtxRaeExec {
    fn default() -> Self {
        Self {
            actions_progress: Default::default(),
            state: Default::default(),
            platform_interface: Box::new(()),
        }
    }
}

impl GetModule for CtxRaeExec {
    fn get_module(self) -> Module {
        let init: InitLisp = vec![
            MACRO_GENERATE_ACTION,
            MACRO_GENERATE_METHOD,
            MACRO_GENERATE_TASK,
            MACRO_GENERATE_STATE_FUNCTION,
        ]
        .into();
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: init,
            label: "",
        };

        module.add_fn_prelude(RAE_GET_STATE, get_state);
        module.add_fn_prelude(RAE_GET_STATE_VARIBALE, get_state_variable);
        module.add_fn_prelude(RAE_EXEC_COMMAND, exec_command);
        module.add_mut_fn_prelude(RAE_LAUNCH_PLATFORM, launch_platform);
        module.add_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_fn_prelude(RAE_CANCEL_COMMAND, cancel_command);
        //Manage facts:
        module.add_fn_prelude(RAE_ASSERT, assert_fact);
        module.add_fn_prelude(RAE_ASSERT_SHORT, assert_fact);
        module.add_fn_prelude(RAE_RETRACT, retract_fact);
        module.add_fn_prelude(RAE_RETRACT_SHORT, retract_fact);
        module.add_fn_prelude(RAE_AWAIT, fn_await);
        module.add_mut_fn_prelude(RAE_OPEN_COM_PLATFORM, open_com);
        module.add_mut_fn_prelude(RAE_START_PLATFORM, start_platform);
        module
    }
}

impl CtxRaeExec {
    pub fn get_execution_status(&self, action_id: &ActionId) -> Option<Status> {
        self.actions_progress.get_status(&action_id)
    }

    pub fn add_platform(&mut self, platform: Box<dyn RAEInterface>) {
        self.platform_interface = platform;
    }
}

impl Documentation for CtxRaeExec {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}

pub struct JobStream {
    sender: Sender<Job>,
    receiver: Receiver<Job>,
}

impl JobStream {
    pub fn new(sender: Sender<Job>, receiver: Receiver<Job>) -> Self {
        Self { sender, receiver }
    }

    pub fn get_sender(&self) -> Sender<Job> {
        self.sender.clone()
    }

    pub fn get_ref_receiver(&mut self) -> &mut Receiver<Job> {
        &mut self.receiver
    }
}

#[derive(Debug, Clone)]
pub struct Job {
    pub _type: JobType,
    pub core: LValue,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} : {}", self._type, self.core)
    }
}

impl Job {
    pub fn new(value: LValue, _type: JobType) -> Self {
        Self { _type, core: value }
    }
}

#[derive(Debug, Clone)]
pub enum JobType {
    Task,
    Event,
}

impl Display for JobType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            JobType::Task => write!(f, "task"),
            JobType::Event => write!(f, "event"),
        }
    }
}
pub type JobId = usize;

pub trait RAEInterface: Any + Send + Sync {
    ///Execute a command on the platform
    ///TODO: Store the command id in the args?
    ///

    fn init(&mut self, state: RAEState, status: ActionsProgress);

    fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError>;

    fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get the whole state of the platform
    fn get_state(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get a specific state variable
    fn get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Return the status of all the actions
    fn get_status(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Launch the platform (such as the simulation in godot)
    fn launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    fn start_platform(&self, args: &[LValue]) -> Result<LValue, LError>;

    fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    fn get_action_status(&self, action_id: &usize) -> Status;
    fn set_status(&self, action_id: usize, status: Status);

    fn domain(&self) -> &'static str;
}

impl RAEInterface for () {
    fn init(&mut self, _: RAEState, _: ActionsProgress) {
        todo!()
    }

    fn exec_command(&self, _args: &[LValue], _: usize) -> Result<LValue, LError> {
        todo!()
    }

    fn cancel_command(&self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_state(&self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_state_variable(&self, _args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_status(&self, _args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn launch_platform(&mut self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn start_platform(&self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn open_com(&mut self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_action_status(&self, _action_id: &usize) -> Status {
        todo!()
    }

    fn set_status(&self, _action_id: usize, _status: Status) {
        todo!()
    }

    fn domain(&self) -> &'static str {
        todo!()
    }
}

pub fn exec_command(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    let command_id = ctx.actions_progress.get_new_id();
    //let debug: LValue = args.into();
    //println!("exec command {}: {}", command_id, debug);
    ctx.platform_interface.exec_command(args, command_id)?;
    Ok(command_id.into())
}

///Retract a fact to state
pub fn retract_fact(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }
    let key = args[0].clone().into();
    let value = args[1].clone().into();
    ctx.state.retract_fact(key, value)
}

///Add a fact to fact state
pub fn assert_fact(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }
    let key = args[0].clone().into();
    let value = args[1].clone().into();
    ctx.state.add_fact(key, value);

    Ok(Nil)
}

///Monitor the status of an action that has been triggered
/// Return true if the action is a success, false otherwise
pub fn fn_await(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    let action_id = args[0].clone();
    //println!("await on action (id={})", action_id);

    if let LValue::Number(LNumber::Usize(action_id)) = action_id {
        let mut receiver = ctx.actions_progress.declare_new_watcher(&action_id);
        let action_progress = ctx.actions_progress.clone();
        let handle = tokio::runtime::Handle::current();
        thread::spawn(move || {
            handle.block_on(async move {
                loop {
                    //println!("waiting on status:");
                    match receiver.recv().await.unwrap() {
                        true => {
                            //println!("status updated!");
                            match action_progress.status.read().unwrap().get(&action_id) {
                                Some(s) => match s {
                                    Status::Pending => {
                                        //println!("not triggered");
                                    }
                                    Status::Running => {
                                        //println!("running");
                                    }
                                    Status::Failure => {
                                        println!("command is a failure");
                                        return Ok(false.into());
                                    }
                                    Status::Done => {
                                        println!("command is a success");
                                        return Ok(true.into());
                                    }
                                },
                                None => {
                                    panic!("no action status")
                                }
                            };
                        }
                        false => panic!("sync to watch action status should not be false"),
                    }
                }
            })
        })
        .join()
        .unwrap()
    } else {
        Err(WrongType(
            action_id.clone(),
            action_id.into(),
            NameTypeLValue::Usize,
        ))
    }
}

pub fn launch_platform(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRaeExec,
) -> Result<LValue, LError> {
    match &ctx.actions_progress.sync.sender {
        None => Err(SpecialError(
            "sender to actions status watcher missing.".to_string(),
        )),
        Some(_) => ctx.platform_interface.launch_platform(args),
    }
}

pub fn start_platform(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRaeExec,
) -> Result<LValue, LError> {
    ctx.platform_interface.start_platform(args)
}

pub fn open_com(args: &[LValue], _env: &LEnv, ctx: &mut CtxRaeExec) -> Result<LValue, LError> {
    match &ctx.actions_progress.sync.sender {
        None => Err(SpecialError(
            "sender to actions status watcher missing.".to_string(),
        )),
        Some(_) => ctx.platform_interface.open_com(args),
    }
}

pub fn get_state(args: &[LValue], env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    let _type = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(sym) = &args[0] {
                match sym.as_str() {
                    KEY_STATIC => Some(StateType::Static),
                    KEY_DYNAMIC => Some(StateType::Dynamic),
                    KEY_INNER_WORLD => Some(StateType::InnerWorld),
                    _ => {
                        return Err(SpecialError(format!(
                            "was expecting keys {}, {}, {}",
                            KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                        )))
                    }
                }
            } else {
                return Err(WrongType(
                    args[0].clone(),
                    (&args[0]).into(),
                    NameTypeLValue::Symbol,
                ));
            }
        }
        _ => return Err(WrongNumberOfArgument(args.into(), args.len(), 0..1)),
    };

    let platform_state = ctx.platform_interface.get_state(args).unwrap();
    let state = ctx.state.get_state(_type).into_map();
    union_map(&[platform_state, state], env, &())
}

pub fn get_state_variable(
    args: &[LValue],
    _env: &LEnv,
    ctx: &CtxRaeExec,
) -> Result<LValue, LError> {
    ctx.platform_interface.get_state_variable(args)
}

pub fn get_status(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    ctx.platform_interface.get_status(args)
}

pub fn cancel_command(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    ctx.platform_interface.cancel_command(args)
}
