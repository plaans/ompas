use crate::rae::context::actions_progress::{ActionId, ActionsProgress, Status};
use crate::rae::context::agenda::Agenda;
use crate::rae::context::rae_env::RAE_TASK_METHODS_MAP;
use crate::rae::context::rae_state::*;
use crate::rae::context::ressource_access::wait_on::add_waiter;
use crate::rae::module::domain::*;
use crate::rae::select_methods::sort_greedy;
use log::{error, info, warn};
use ompas_lisp::core::LEnv;
use ompas_lisp::functions::{cons, union_map};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::*;
use ompas_lisp::structs::*;
use ompas_utils::blocking_async;
use std::any::Any;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, PoisonError, RwLock, RwLockReadGuard};
use std::thread;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::Mutex;

/*
LANGUAGE
 */

const MOD_RAE_EXEC: &str = "mod-rae-exec";

//manage facts
pub const RAE_ASSERT: &str = "assert";
pub const RAE_ASSERT_SHORT: &str = "+>";
pub const RAE_RETRACT: &str = "retract";
pub const RAE_RETRACT_SHORT: &str = "->";
pub const RAE_AWAIT: &str = "rae-await";
pub const WAIT_ON: &str = "wait-on";

//RAE Interface with a platform
pub const RAE_EXEC_COMMAND: &str = "rae-exec-command";
pub const RAE_GET_STATE: &str = "rae-get-state";
pub const RAE_GET_STATE_VARIBALE: &str = "rae-get-state-variable";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_OPEN_COM_PLATFORM: &str = "rae-open-com-platform";
pub const RAE_START_PLATFORM: &str = "rae-start-platform";
pub const RAE_GET_STATUS: &str = "rae-get-status";
pub const RAE_CANCEL_COMMAND: &str = "rae-cancel-command";
pub const RAE_GET_INSTANTIATED_METHODS: &str = "rae-get-instantiated-methods";
pub const RAE_GET_BEST_METHOD: &str = "rae-get-best-method";
pub const RAE_LOG: &str = "rae-log";
pub const RAE_SELECT: &str = "rae-select";
pub const RAE_GET_NEXT_METHOD: &str = "rae-get-next-method";
pub const RAE_SET_SUCCESS_FOR_TASK: &str = "rae-set-success-for-task";

//TODO: write doc

///Context that will contains primitives for the RAE executive
pub struct CtxRaeExec {
    //pub stream: JobStream,
    pub actions_progress: ActionsProgress,
    pub state: RAEState,
    pub platform_interface: Box<dyn RAEInterface>,
    pub agenda: Agenda,
}

impl Default for CtxRaeExec {
    fn default() -> Self {
        Self {
            actions_progress: Default::default(),
            state: Default::default(),
            platform_interface: Box::new(()),
            agenda: Default::default(),
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
            MACRO_GENERATE_TASK_SIMPLE,
            MACRO_GENERATE_METHOD_PARAMETERS,
            MACRO_ENUMERATE_PARAMS,
            LAMBDA_MUTEX_LOCK,
            LAMBDA_MUTEX_IS_LOCKED,
            LAMBDA_MUTEX_RELEASE,
            LAMBDA_PROGRESS,
            LAMBDA_RETRY,
            LAMBDA_GET_METHODS,
            LAMBDA_GET_PARAMETERS_GENERATOR,
            LAMBDA_GET_SCORE_GENERATOR,
        ]
        .into();
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: init,
            label: MOD_RAE_EXEC,
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
        module.add_fn_prelude(RAE_GET_INSTANTIATED_METHODS, get_instantiated_methods);
        module.add_fn_prelude(RAE_GET_BEST_METHOD, get_best_method);
        module.add_fn_prelude(WAIT_ON, wait_on);
        module.add_fn_prelude(RAE_LOG, log);
        module.add_fn_prelude(RAE_SELECT, select);
        module.add_fn_prelude(RAE_SET_SUCCESS_FOR_TASK, set_success_for_task);
        module.add_fn_prelude(RAE_GET_NEXT_METHOD, get_next_method);
        module
    }
}

impl CtxRaeExec {
    pub async fn get_execution_status(&self, action_id: &ActionId) -> Option<Status> {
        self.actions_progress.get_status(action_id).await
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

/// Trait that a platform needs to implement to be able to be used as execution platform in RAE.
pub trait RAEInterface: Any + Send + Sync {
    /// Initial what needs to be.
    fn init(&mut self, state: RAEState, status: ActionsProgress);

    /// Executes a command on the platform
    fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError>;

    fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get the whole state of the platform
    fn get_state(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get a specific state variable
    fn get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Return the status of all the actions
    fn get_status(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Launch the platform (such as the simulation in godot) and open communication
    fn launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    /// Start the platform process
    fn start_platform(&self, args: &[LValue]) -> Result<LValue, LError>;

    /// Open communication with the platform
    fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    /// Returns the status of a given action
    fn get_action_status(&self, action_id: &usize) -> Status;

    /// Set the status of a given action
    fn set_status(&self, action_id: usize, status: Status);

    /// Returns the RAE domain of the platform.
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
    let debug: LValue = args.into();
    info!("exec command {}: {}", command_id, debug);
    ctx.platform_interface.exec_command(args, command_id)?;
    Ok(command_id.into())
}

///Retract a fact to state
pub fn retract_fact(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAE_RETRACT,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    let key = args[0].clone().into();
    let value = args[1].clone().into();
    let c_state = ctx.state.clone();
    blocking_async!(c_state.retract_fact(key, value).await).expect("todo!")
}

///Add a fact to fact state
pub fn assert_fact(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAE_ASSERT,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    let key = args[0].clone().into();
    let value = args[1].clone().into();
    let c_state = ctx.state.clone();
    blocking_async!(c_state.add_fact(key, value).await).expect("todo!");

    Ok(True)
}

///Monitor the status of an action that has been triggered
/// Return true if the action is a success, false otherwise
pub fn fn_await(args: &[LValue], _env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_AWAIT,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let action_id = args[0].clone();
    //println!("await on action (id={})", action_id);

    if let LValue::Number(LNumber::Usize(action_id)) = action_id {
        let mut receiver = ctx.actions_progress.declare_new_watcher(&action_id);
        let action_progress = ctx.actions_progress.clone();
        let handle = tokio::runtime::Handle::current();
        info!("waiting on action {}", action_id);
        thread::spawn(move || {
            handle.block_on(async move {
                loop {
                    //println!("waiting on status:");
                    match receiver.recv().await.unwrap() {
                        true => {
                            //println!("status updated!");
                            match action_progress.status.read().await.get(&action_id) {
                                Some(s) => match s {
                                    Status::Pending => {
                                        //println!("not triggered");
                                    }
                                    Status::Running => {
                                        //println!("running");
                                    }
                                    Status::Failure => {
                                        warn!("Command {} is a failure.", action_id);
                                        return Ok(false.into());
                                    }
                                    Status::Done => {
                                        info!("Command {} is a success.", action_id);
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
            RAE_AWAIT,
            action_id.clone(),
            action_id.into(),
            NameTypeLValue::Usize,
        ))
    }
}

//Return the labels of the methods

fn get_instantiated_methods(
    args: &[LValue],
    env: &LEnv,
    _ctx: &CtxRaeExec,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_GET_INSTANTIATED_METHODS,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }
    let task_name = &args[0];
    let task_args: LValue = (&args[1..]).into();
    //log::send(format!("searching methods for {}\n", task_name));
    let task_method_map = env.get_symbol(RAE_TASK_METHODS_MAP).unwrap();
    //log::send(format!("method_map: {}\n", task_method_map));
    let methods = if let LValue::Map(map) = task_method_map {
        let methods = match map.get(task_name) {
            None => {
                return Err(SpecialError(
                    RAE_GET_INSTANTIATED_METHODS,
                    format!("no methods for {}", task_name),
                ))
            }
            Some(methods) => {
                //Got here the list of the symbol of the methods
                let mut instantiated_method = vec![];
                if let LValue::List(methods) = methods {
                    for method in methods {
                        //Handle here the case where it is needed to generate all instantiation of methods where several parameters are possible.
                        instantiated_method
                            .push(cons(&[method.clone(), task_args.clone()], env, &()).unwrap());
                    }
                    instantiated_method.into()
                } else if let LValue::Nil = methods {
                    LValue::Nil
                } else {
                    panic!("The list of methods should be a LValue::List or Nil and nothing else")
                }
            }
        };
        methods
    } else {
        panic!("this should be a LValue::Map")
    };

    //log::send(format!("{}", methods));
    Ok(methods)
}

fn get_best_method(args: &[LValue], env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    /*ompas_utils::log::send(format!("env in get_best_method :\n {}", env));
    let task_methods_map = env.get_symbol(RAE_TASK_METHODS_MAP);
    ompas_utils::log::send(format!(
        "In get-best-method, task_methods_map: {:?}\n",
        task_methods_map
    ));*/

    let methods = get_instantiated_methods(args, env, ctx)?;
    let task_args = &args[1..];
    //log::send(format!("methods for {}: {}\n", LValue::from(args), methods));
    let best_method = if let LValue::List(methods) = methods {
        if methods.is_empty() {
            return Err(SpecialError(
                RAE_GET_BEST_METHOD,
                "task has no applicable method".to_string(),
            ));
        }
        methods[0].clone()
    } else {
        return Err(WrongType(
            RAE_GET_BEST_METHOD,
            methods.clone(),
            methods.into(),
            NameTypeLValue::List,
        ));
    };

    let method_instance = cons(&[best_method, task_args.into()], env, &())?;
    //log::send(format!("instance of the method: {}\n", method_instance));

    Ok(method_instance)
}

pub fn launch_platform(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRaeExec,
) -> Result<LValue, LError> {
    match &ctx.actions_progress.sync.sender {
        None => Err(SpecialError(
            RAE_LAUNCH_PLATFORM,
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
            RAE_START_PLATFORM,
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
                        return Err(SpecialError(
                            RAE_GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                            ),
                        ))
                    }
                }
            } else {
                return Err(WrongType(
                    RAE_GET_STATE,
                    args[0].clone(),
                    (&args[0]).into(),
                    NameTypeLValue::Symbol,
                ));
            }
        }
        _ => {
            return Err(WrongNumberOfArgument(
                RAE_GET_STATE,
                args.into(),
                args.len(),
                0..1,
            ))
        }
    };

    let platform_state = ctx.platform_interface.get_state(args).unwrap();
    let c_state = ctx.state.clone();
    let state = blocking_async!(c_state.get_state(_type).await.into_map()).expect("todo!");
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

pub fn wait_on(args: &[LValue], _env: &LEnv, _: &CtxRaeExec) -> Result<LValue, LError> {
    //info!("wait on function");
    //println!("wait on function with {} args", args.len());
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            WAIT_ON,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    //println!("New wait on {}", args[0]);
    let mut rx = add_waiter(args[0].clone());
    //println!("receiver ok");
    blocking_async!({
        if let false = rx.recv().await.expect("could not receive msg from waiters") {
            unreachable!("should not receive false from waiters")
        }
        //println!("end of wait on");
    })
    .expect("todo!");
    //println!("end wait on");
    Ok(LValue::True)
}

fn log(args: &[LValue], _env: &LEnv, _: &CtxRaeExec) -> Result<LValue, LError> {
    info!("{}", LValue::from(args));
    Ok(LValue::True)
}

//Takes an instantiated task to refine and return the best applicable method and a task_id.
fn select(args: &[LValue], env: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    let task: LValue = args.into();

    info!("Add task {} to agenda", task);
    let task_id = ctx.agenda.add_task(task);
    let methods = get_instantiated_methods(args, env, ctx)?;

    //Here we choose different way to choose the best method.
    //TODO: Implement a way to configure select

    let methods: Vec<LValue> = sort_greedy(methods)?.try_into()?;
    let mut stack = ctx.agenda.get_stack(task_id).unwrap();
    stack.set_current_method(methods[0].clone());
    stack.set_applicable_methods(methods[1..].into());

    ctx.agenda.update_stack(stack)?;

    //info!("agenda: {}", ctx.agenda);

    Ok(vec![methods[0].clone(), task_id.into()].into())

    /*
    Steps:
    - Create a new entry in the agenda
    - Generate all instances of applicable methods
    - Select the best method
    - Store the stack
    - Return (task_id, best_method)
     */
}

fn get_next_method(args: &[LValue], _: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_GET_NEXT_METHOD,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::Number(LNumber::Usize(task_id)) = &args[0] {
        let next_method = ctx.agenda.get_next_applicable_method(task_id);
        Ok(next_method)
    } else {
        Err(WrongType(
            RAE_GET_NEXT_METHOD,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::Usize,
        ))
    }
}

fn set_success_for_task(args: &[LValue], _: &LEnv, ctx: &CtxRaeExec) -> Result<LValue, LError> {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_SET_SUCCESS_FOR_TASK,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::Number(LNumber::Usize(task_id)) = &args[0] {
        ctx.agenda.remove_task(task_id)?;
        Ok(LValue::True)
    } else {
        Err(WrongType(
            RAE_SET_SUCCESS_FOR_TASK,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::Usize,
        ))
    }
}
