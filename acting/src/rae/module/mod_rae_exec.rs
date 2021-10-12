use crate::rae::context::actions_progress::{ActionId, ActionsProgress, Status};
use crate::rae::context::agenda::Agenda;
use crate::rae::context::mutex;
use crate::rae::context::mutex::MutexResponse;
use crate::rae::context::rae_env::RAE_TASK_METHODS_MAP;
use crate::rae::context::rae_state::*;
use crate::rae::context::ressource_access::wait_on::add_waiter;
use crate::rae::select_methods::sort_greedy;
use ::macro_rules_attribute::macro_rules_attribute;
use async_trait::async_trait;
use log::{error, info, warn};
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::functions::{cons, union_map};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::*;
use ompas_lisp::structs::*;
use ompas_utils::dyn_async;
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
pub const WAIT_ON: &str = "check";
pub const LOCK: &str = "lock";
pub const RELEASE: &str = "release";
pub const IS_LOCKED: &str = "locked?";
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
pub const RAE_SELECT: &str = "rae-select";
pub const RAE_GET_NEXT_METHOD: &str = "rae-get-next-method";
pub const RAE_SET_SUCCESS_FOR_TASK: &str = "rae-set-success-for-task";

pub const MACRO_MUTEX_LOCK_AND_DO: &str = "(defmacro mutex::lock-and-do 
    (lambda (r b)
        `(begin
            (lock ,r)
            ,b
            (release ,r))))";
pub const MACRO_WAIT_ON: &str = "(defmacro wait-on (lambda (expr)
    `(if (not (eval ,expr))
        (check ,expr))))";
pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

/*pub const LAMBDA_MUTEX_LOCK: &str = "(define mutex::lock (lambda (__symbol__)
                                        (begin
                                            (wait-on `(not (mutex::locked? ,__symbol__)))
                                            (assert `(locked ,__symbol__) true))))";

pub const LAMBDA_MUTEX_IS_LOCKED: &str = "(define mutex::locked? (lambda (__symbol__)
                                        (rae-get-state-variable `(locked ,__symbol__))))";

pub const LAMBDA_MUTEX_RELEASE: &str = "(define mutex::release (lambda (__symbol__)
                                        (retract `(locked ,__symbol__) true)))";*/

pub const LAMBDA_PROGRESS: &str = "
(define progress (lambda args
    (let* ((result (eval (cons select (cons `(quote ,(car args)) (cdr args)))))
            (first_m (car result))
            (task_id (cadr result)))
            
            (if (null? first_m)
                nil
                (if (eval first_m)
                    (rae-set-success-for-task task_id)
                    (retry task_id))))))";

pub const LAMBDA_SELECT: &str = "
(define select
  (lambda args
    (rae-select args (generate-applicable-instances (rae-get-state) args ))))";

pub const LAMBDA_RETRY: &str = "
(define retry (lambda (task_id)
    (let ((new_method (rae-get-next-method task_id)))
        (begin 
            (print \"Retrying task \" task_id)
            (if (null? new_method) ; if there is no method applicable
            nil
            (if (eval new_method)
                (rae-set-success-for-task task_id)
                (rae-retry task_id)))))))";

//Access part of the environment

pub const LAMBDA_GET_METHODS: &str = "\
(define get-methods\
    (lambda (label)\
        (get rae-task-methods-map label)))";

pub const LAMBDA_GET_METHOD_GENERATOR: &str = "\
(define get-method-generator
       (lambda (label)
            (get rae-method-generator-map label)))";

pub const LAMBDA_GENERATE_INSTANCES: &str = "
(define generate-instances (lambda args
    (let* ((label (car args))
            (i_params (cdr args))
            (methods (get-methods label)))

            (begin
                (define __generate__
                    (lambda (methods)
                        (if (null? methods)
                            nil
                            (append
                                (eval
                                    (append (list (get-method-generator (car methods)))
                                        i_params))
                                (__generate__ (cdr methods))))))
                (__generate__ methods)))))";

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
            MACRO_MUTEX_LOCK_AND_DO,
            MACRO_WAIT_ON,
            //LAMBDA_MUTEX_LOCK,
            //LAMBDA_MUTEX_IS_LOCKED,
            //LAMBDA_MUTEX_RELEASE,
            LAMBDA_PROGRESS,
            LAMBDA_SELECT,
            LAMBDA_RETRY,
            LAMBDA_GET_METHODS,
            LAMBDA_GET_METHOD_GENERATOR,
        ]
        .into();
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: init,
            label: MOD_RAE_EXEC.to_string(),
        };

        module.add_async_fn_prelude(RAE_GET_STATE, get_state);
        module.add_async_fn_prelude(RAE_GET_STATE_VARIBALE, get_state_variable);
        module.add_async_fn_prelude(RAE_EXEC_COMMAND, exec_command);
        module.add_async_mut_fn_prelude(RAE_LAUNCH_PLATFORM, launch_platform);
        module.add_async_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_async_fn_prelude(RAE_CANCEL_COMMAND, cancel_command);
        //Manage facts:
        module.add_async_fn_prelude(RAE_ASSERT, assert_fact);
        module.add_async_fn_prelude(RAE_ASSERT_SHORT, assert_fact);
        module.add_async_fn_prelude(RAE_RETRACT, retract_fact);
        module.add_async_fn_prelude(RAE_RETRACT_SHORT, retract_fact);
        //module.add_async_fn_prelude(RAE_AWAIT, fn_await);
        module.add_async_mut_fn_prelude(RAE_OPEN_COM_PLATFORM, open_com);
        module.add_async_mut_fn_prelude(RAE_START_PLATFORM, start_platform);
        module.add_fn_prelude(RAE_GET_INSTANTIATED_METHODS, get_instantiated_methods);
        module.add_fn_prelude(RAE_GET_BEST_METHOD, get_best_method);
        module.add_async_fn_prelude(WAIT_ON, wait_on);
        module.add_async_fn_prelude(RAE_SELECT, select);
        module.add_async_fn_prelude(RAE_SET_SUCCESS_FOR_TASK, set_success_for_task);
        module.add_async_fn_prelude(RAE_GET_NEXT_METHOD, get_next_method);

        //mutex
        module.add_async_fn_prelude(LOCK, lock);
        module.add_async_fn_prelude(RELEASE, release);
        module.add_async_fn_prelude(IS_LOCKED, is_locked);
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
#[async_trait]
pub trait RAEInterface: Any + Send + Sync {
    /// Initial what needs to be.
    async fn init(&mut self, state: RAEState, status: ActionsProgress);

    /// Executes a command on the platform
    async fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError>;

    async fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get the whole state of the platform
    async fn get_state(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get a specific state variable
    async fn get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Return the status of all the actions
    async fn get_status(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Launch the platform (such as the simulation in godot) and open communication
    async fn launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    /// Start the platform process
    async fn start_platform(&self, args: &[LValue]) -> Result<LValue, LError>;

    /// Open communication with the platform
    async fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    /// Returns the status of a given action
    async fn get_action_status(&self, action_id: &usize) -> Status;

    /// Set the status of a given action
    async fn set_status(&self, action_id: usize, status: Status);

    /// Returns the RAE domain of the platform.
    async fn domain(&self) -> &'static str;
}

#[async_trait]
impl RAEInterface for () {
    async fn init(&mut self, _: RAEState, _: ActionsProgress) {}

    async fn exec_command(&self, _args: &[LValue], _: usize) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn cancel_command(&self, _: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn get_state(&self, _: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn get_state_variable(&self, _args: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn get_status(&self, _args: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn launch_platform(&mut self, _: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn start_platform(&self, _: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn open_com(&mut self, _: &[LValue]) -> Result<LValue, LError> {
        Ok(Nil)
    }

    async fn get_action_status(&self, _action_id: &usize) -> Status {
        Status::Pending
    }

    async fn set_status(&self, _action_id: usize, _status: Status) {}

    async fn domain(&self) -> &'static str {
        ""
    }
}
#[macro_rules_attribute(dyn_async!)]
async fn exec_command<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    let command_id = ctx.actions_progress.get_new_id();
    let debug: LValue = args.into();
    info!("exec command {}: {}", command_id, debug);
    ctx.platform_interface
        .exec_command(args, command_id)
        .await?;

    //println!("await on action (id={})", action_id);

    let mut receiver = ctx.actions_progress.declare_new_watcher(&command_id).await;
    info!("waiting on action {}", command_id);

    let mut action_status = ctx
        .actions_progress
        .status
        .read()
        .await
        .get(&command_id)
        .unwrap()
        .clone();

    loop {
        //println!("waiting on status:");
        match action_status {
            Status::Pending => {
                //println!("not triggered");
            }
            Status::Running => {
                //println!("running");
            }
            Status::Failure => {
                warn!("Command {} is a failure.", command_id);
                return Ok(false.into());
            }
            Status::Done => {
                info!("Command {} is a success.", command_id);
                return Ok(true.into());
            }
        }
        action_status = if let true = receiver.recv().await.unwrap() {
            ctx.actions_progress
                .status
                .read()
                .await
                .get(&command_id)
                .unwrap()
                .clone()
        } else {
            unreachable!("the signal for an update should always be true")
        }
    }
}

///Retract a fact to state
#[macro_rules_attribute(dyn_async!)]
async fn retract_fact<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAE_RETRACT,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    let key = (&args[0]).into();
    let value = (&args[1]).into();
    ctx.state.retract_fact(key, value).await
}

///Add a fact to fact state
#[macro_rules_attribute(dyn_async!)]
async fn assert_fact<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAE_ASSERT,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    let key = (&args[0]).into();
    let value = (&args[1]).into();
    ctx.state.add_fact(key, value).await;

    Ok(True)
}

///Monitor the status of an action that has been triggered
/// Return true if the action is a success, false otherwise
#[macro_rules_attribute(dyn_async!)]
async fn fn_await<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
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
        let mut receiver = ctx.actions_progress.declare_new_watcher(&action_id).await;
        info!("waiting on action {}", action_id);

        loop {
            //println!("waiting on status:");
            match receiver.recv().await.unwrap() {
                true => {
                    //println!("status updated!");
                    match ctx.actions_progress.status.read().await.get(&action_id) {
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
#[macro_rules_attribute(dyn_async!)]
async fn launch_platform<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a mut CtxRaeExec,
) -> Result<LValue, LError> {
    match &ctx.actions_progress.sync.sender {
        None => Err(SpecialError(
            RAE_LAUNCH_PLATFORM,
            "sender to actions status watcher missing.".to_string(),
        )),
        Some(_) => ctx.platform_interface.launch_platform(args).await,
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn start_platform<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a mut CtxRaeExec,
) -> Result<LValue, LError> {
    ctx.platform_interface.start_platform(args).await
}
#[macro_rules_attribute(dyn_async!)]
async fn open_com<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a mut CtxRaeExec,
) -> Result<LValue, LError> {
    match &ctx.actions_progress.sync.sender {
        None => Err(SpecialError(
            RAE_START_PLATFORM,
            "sender to actions status watcher missing.".to_string(),
        )),
        Some(_) => ctx.platform_interface.open_com(args).await,
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn get_state<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
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

    let platform_state = ctx.platform_interface.get_state(args).await.unwrap();
    let state = ctx.state.get_state(_type).await.into_map();
    union_map(&[platform_state, state], env, &())
}

#[macro_rules_attribute(dyn_async!)]
async fn get_state_variable<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    ctx.platform_interface.get_state_variable(args).await
}

#[macro_rules_attribute(dyn_async!)]
async fn get_status<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    ctx.platform_interface.get_status(args).await
}

#[macro_rules_attribute(dyn_async!)]
async fn cancel_command<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    ctx.platform_interface.cancel_command(args).await
}

#[macro_rules_attribute(dyn_async!)]
async fn wait_on<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    _: &'a CtxRaeExec,
) -> Result<LValue, LError> {
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
    let mut rx = add_waiter(args[0].clone()).await;
    //println!("receiver ok");

    if let false = rx.recv().await.expect("could not receive msg from waiters") {
        unreachable!("should not receive false from waiters")
    }

    //println!("end wait on");
    Ok(LValue::True)
}

//Takes an instantiated task to refine and return the best applicable method and a task_id.
//TODO: Implement a way to configure select
#[macro_rules_attribute(dyn_async!)]
async fn select<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    let task = args[0].clone();

    if let LValue::List(_) = &args[1] {
        let methods = args[1].clone();
        info!("Add task {} to agenda", task);
        let task_id = ctx.agenda.add_task(task).await;
        info!("methods with their score found: {}", args[1]);
        let methods: Vec<LValue> = sort_greedy(methods)?.try_into()?; //Handle the case where there is no methods
        info!("sorted_methods: {}", LValue::from(&methods));
        let mut stack = ctx.agenda.get_stack(task_id).await.unwrap();
        stack.set_current_method(methods[0].clone());
        stack.set_applicable_methods(methods[1..].into());

        ctx.agenda.update_stack(stack).await?;

        //info!("agenda: {}", ctx.agenda);

        Ok(vec![methods[0].clone(), task_id.into()].into())
    } else {
        return Err(SpecialError(
            "rae-select",
            format!("Task {} has no applicable method", task),
        ));
    }
    /*
    Steps:
    - Create a new entry in the agenda
    - Generate all instances of applicable methods
    - Select the best method
    - Store the stack
    - Return (task_id, best_method)
     */
}
#[macro_rules_attribute(dyn_async!)]
async fn get_next_method<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_GET_NEXT_METHOD,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::Number(LNumber::Usize(task_id)) = &args[0] {
        let next_method = ctx.agenda.get_next_applicable_method(task_id).await;
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

#[macro_rules_attribute(dyn_async!)]
async fn set_success_for_task<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
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
        ctx.agenda.remove_task(task_id).await?;
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

/*
MUTEXES
 */

#[macro_rules_attribute(dyn_async!)]
async fn lock<'a>(args: &'a [LValue], _: &'a LEnv, _: &'a CtxRaeExec) -> Result<LValue, LError> {
    match mutex::lock(args[0].clone()).await {
        MutexResponse::Ok => Ok(Nil),
        MutexResponse::Wait(mut rx) => {
            rx.recv().await;
            Ok(True)
        }
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn release<'a>(args: &'a [LValue], _: &'a LEnv, _: &'a CtxRaeExec) -> Result<LValue, LError> {
    mutex::release(args[0].clone()).await;
    Ok(True)
}

#[macro_rules_attribute(dyn_async!)]
async fn is_locked<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    _: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    Ok(mutex::is_locked(args[0].clone()).await.into())
}
