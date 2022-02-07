pub mod platform;
pub mod rae_mutex;
pub mod simu;

use crate::context::actions_progress::{ActionId, ActionsProgress, Status};
use crate::context::agenda::Agenda;
use crate::context::rae_env::RAE_TASK_METHODS_MAP;
use crate::context::rae_state::*;
use crate::context::ressource_access::wait_on::add_waiter;
use crate::module::rae_exec::platform::*;
use crate::module::rae_exec::rae_mutex::*;
use crate::module::rae_exec::simu::*;
use ::macro_rules_attribute::macro_rules_attribute;
use async_trait::async_trait;
use log::info;
use ompas_lisp::core::root_module::list::cons;
use ompas_lisp::core::root_module::map::{get_map, remove_key_value_map, set_map};
use ompas_lisp::core::structs::contextcollection::Context;
use ompas_lisp::core::structs::documentation::Documentation;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::lvalue::LValue::{Nil, True};
use ompas_lisp::core::structs::lvalues::LValueS;
use ompas_lisp::core::structs::module::{InitLisp, IntoModule, Module};
use ompas_lisp::core::structs::purefonction::PureFonctionCollection;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_utils::dyn_async;
use std::any::Any;

use crate::supervisor::select_methods::sort_greedy;
use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::string::String;
use std::sync::Arc;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::RwLock;

/*
LANGUAGE
 */

pub const MOD_RAE_EXEC: &str = "mod-rae-exec";

//manage facts
pub const RAE_ASSERT: &str = "assert";
pub const RAE_ASSERT_SHORT: &str = "+>";
pub const RAE_RETRACT: &str = "retract";
pub const RAE_RETRACT_SHORT: &str = "->";
pub const RAE_AWAIT: &str = "rae-await";
pub const MONITOR: &str = "monitor";
pub const LOCK: &str = "lock";
pub const RELEASE: &str = "release";
pub const IS_LOCKED: &str = "locked?";
pub const LOCKED: &str = "locked";
pub const LOCKED_LIST: &str = "locked-list";
//RAE Interface with a platform
pub const RAE_GET_STATE: &str = "rae-get-state";
pub const RAE_GET_FACTS: &str = "rae-get-facts";
pub const RAE_GET_STATE_VARIBALE: &str = "rae-get-state-variable";

pub const RAE_GET_STATUS: &str = "rae-get-status";
pub const RAE_CANCEL_COMMAND: &str = "rae-cancel-command";
pub const RAE_GET_INSTANTIATED_METHODS: &str = "rae-get-instantiated-methods";
pub const RAE_GET_BEST_METHOD: &str = "rae-get-best-method";
pub const RAE_SELECT: &str = "rae-select";
pub const RAE_GET_NEXT_METHOD: &str = "rae-get-next-method";
pub const RAE_SET_SUCCESS_FOR_TASK: &str = "rae-set-success-for-task";

const SUCCESS: &str = "success";
const FAILURE: &str = "failure";
const IS_SUCCESS: &str = "success?";
const IS_FAILURE: &str = "failure?";

pub const MACRO_WAIT_ON: &str = "(defmacro wait-on (lambda (expr)
    `(if (not (eval ,expr))
        (monitor ,expr))))";
pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

pub const LAMBDA_PROGRESS: &str = "
(define progress (lambda task
    (let* ((result (select task))
            (first_m (first result))
            (task_id (second result)))

            (if (null? first_m)
                (err err::no-method-applicable)
                (if (! (err? (enr first_m)))
                    (rae-set-success-for-task task_id)
                    (retry task_id))))))";

pub const LAMBDA_SELECT: &str = "
(define select
  (lambda (task)
    (sim_block
    (rae-select task (generate_applicable_instances task)))))))";

pub const LAMBDA_RETRY: &str = "
(define retry (lambda (task_id)
    (let ((new_method (rae-get-next-method task_id)))
        (begin 
            (print \"Retrying task \" task_id)
            (if (null? new_method)
                (err err::no-applicable-method)
                (if (! (err? (enr new_method)))
                    (rae-set-success-for-task task_id)
                    (rae-retry task_id)))))))";

//Access part of the environment

pub const LAMBDA_GET_METHODS: &str = "\
(define get-methods\
    (lambda (label)\
        (get rae-task-methods-map label)))";

pub const DEFINE_RAE_MODE: &str = "(define rae-mode EXEC-MODE)";
pub const SYMBOL_EXEC_MODE: &str = "exec-mode";
pub const SYMBOL_SIMU_MODE: &str = "simu-mode";
pub const SYMBOL_RAE_MODE: &str = "rae-mode";

pub const DEFINE_RAE_PLATFORM: &str = "(define rae-platform some)";
pub const DEFINE_NO_RAE_PLATFORM: &str = "(define rae-platform nil)";

pub struct Platform {
    inner: Arc<RwLock<dyn RAEInterface>>,
}

impl Platform {
    pub fn new(platform: impl RAEInterface) -> Self {
        Self {
            inner: Arc::new(RwLock::new(platform)),
        }
    }

    pub fn get_ref(&self) -> Arc<RwLock<dyn RAEInterface>> {
        self.inner.clone()
    }

    /// Initial what needs to be.
    pub async fn init(&self, state: RAEState, status: ActionsProgress) {
        self.inner.write().await.init(state, status).await;
    }

    ///Launch the platform (such as the simulation in godot) and open communication
    pub async fn launch_platform(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.launch_platform(args).await
    }

    /// Start the platform process
    pub async fn start_platform(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.start_platform(args).await
    }

    /// Open communication with the platform
    pub async fn open_com(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.open_com(args).await
    }

    /// Executes a command on the platform
    pub async fn exec_command(&self, args: &[LValue], command_id: usize) -> LResult {
        self.inner.read().await.exec_command(args, command_id).await
    }

    pub async fn cancel_command(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.cancel_command(args).await
    }

    ///Get the whole state of the platform
    pub async fn get_state(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.get_state(args).await
    }

    ///Get a specific state variable
    pub async fn get_state_variable(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.get_state_variable(args).await
    }
    ///Return the status of all the actions
    pub async fn get_status(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.get_status(args).await
    }
    /// Returns the status of a given action
    pub async fn get_action_status(&self, action_id: &usize) -> Status {
        self.inner.read().await.get_action_status(action_id).await
    }

    /// Set the status of a given action
    pub async fn set_status(&self, action_id: usize, status: Status) {
        self.inner.read().await.set_status(action_id, status).await
    }

    /// Returns the RAE domain of the platform.
    pub async fn domain(&self) -> &'static str {
        self.inner.read().await.domain().await
    }

    pub async fn instance(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.instance(args).await
    }
}

///Context that will contains primitives for the RAE executive
#[derive(Default)]
pub struct CtxRaeExec {
    //pub stream: JobStream,
    pub actions_progress: ActionsProgress,
    pub state: RAEState,
    pub platform_interface: Option<Platform>,
    pub agenda: Agenda,
}

impl IntoModule for CtxRaeExec {
    fn into_module(self) -> Module {
        let init: InitLisp = vec![
            MACRO_MUTEX_LOCK_AND_DO,
            MACRO_WAIT_ON,
            MACRO_SIM_BLOCK,
            //LAMBDA_INSTANCE,
            LAMBDA_PROGRESS,
            LAMBDA_SELECT,
            LAMBDA_RETRY,
            LAMBDA_GET_METHODS,
            //LAMBDA_GET_METHOD_GENERATOR,
            //LAMBDA_ARBITRARY,
            LAMBDA_GET_PRECONDITIONS,
            LAMBDA_GET_SCORE,
            LAMBDA_GET_ACTION_MODEL,
            LAMBDA_EVAL_PRE_CONDITIONS,
            LAMBDA_COMPUTE_SCORE,
            LAMBDA_GENERATE_APPLICABLE_INSTANCES,
            LAMBDA_R_GENERATE_INSTANCES,
            LAMBDA_R_TEST_METHOD,
            DEFINE_RAE_MODE,
            LAMBDA_IS_APPLICABLE,
        ]
        .into();
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: init,
            label: MOD_RAE_EXEC.to_string(),
        };

        module.add_async_fn_prelude(RAE_GET_STATE, get_state);
        module.add_async_fn_prelude(RAE_GET_FACTS, get_facts);
        module.add_async_fn_prelude(RAE_GET_STATE_VARIBALE, get_state_variable);
        module.add_async_fn_prelude(RAE_EXEC_COMMAND, exec_command);
        module.add_async_fn_prelude(RAE_LAUNCH_PLATFORM, launch_platform);
        module.add_async_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_async_fn_prelude(RAE_CANCEL_COMMAND, cancel_command);
        module.add_async_fn_prelude(RAE_INSTANCE, instance);
        module.add_fn_prelude(RAE_IS_PLATFORM_DEFINED, is_platform_defined);
        //Manage facts:
        module.add_async_fn_prelude(RAE_ASSERT, assert_fact);
        module.add_async_fn_prelude(RAE_ASSERT_SHORT, assert_fact);
        module.add_async_fn_prelude(RAE_RETRACT, retract_fact);
        module.add_async_fn_prelude(RAE_RETRACT_SHORT, retract_fact);
        module.add_async_fn_prelude(RAE_OPEN_COM_PLATFORM, open_com);
        module.add_async_fn_prelude(RAE_START_PLATFORM, start_platform);
        module.add_fn_prelude(RAE_GET_INSTANTIATED_METHODS, get_instantiated_methods);
        module.add_fn_prelude(RAE_GET_BEST_METHOD, get_best_method);
        module.add_async_fn_prelude(MONITOR, monitor);
        module.add_async_fn_prelude(RAE_SELECT, select);
        module.add_async_fn_prelude(RAE_SET_SUCCESS_FOR_TASK, set_success_for_task);
        module.add_async_fn_prelude(RAE_GET_NEXT_METHOD, get_next_method);

        //mutex
        module.add_async_fn_prelude(LOCK, lock);
        module.add_async_fn_prelude(RELEASE, release);
        module.add_async_fn_prelude(IS_LOCKED, is_locked);
        module.add_async_fn_prelude(LOCKED_LIST, get_list_locked);

        //success and failure
        module.add_fn_prelude(SUCCESS, success);
        module.add_fn_prelude(FAILURE, failure);
        module.add_fn_prelude(IS_SUCCESS, is_success);
        module.add_fn_prelude(IS_FAILURE, is_failure);
        module
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

impl CtxRaeExec {
    pub async fn get_execution_status(&self, action_id: &ActionId) -> Option<Status> {
        self.actions_progress.get_status(action_id).await
    }

    pub fn add_platform(&mut self, platform: Option<Platform>) {
        self.platform_interface = platform;
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
    async fn start_platform(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    /// Open communication with the platform
    async fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    /// Returns the status of a given action
    async fn get_action_status(&self, action_id: &usize) -> Status;

    /// Set the status of a given action
    async fn set_status(&self, action_id: usize, status: Status);

    /// Returns the RAE domain of the platform.
    async fn domain(&self) -> &'static str;

    async fn instance(&self, args: &[LValue]) -> LResult;

    fn context_platform(&self) -> CtxPlatform;
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

    async fn start_platform(&mut self, _: &[LValue]) -> Result<LValue, LError> {
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

    async fn instance(&self, _args: &[LValue]) -> LResult {
        Ok(Nil)
    }

    fn context_platform(&self) -> CtxPlatform {
        todo!()
    }
}

pub struct CtxPlatform {
    module: Module,
}

impl CtxPlatform {
    pub fn new(ctx: impl IntoModule) -> Self {
        Self {
            module: ctx.into_module(),
        }
    }
}

impl IntoModule for CtxPlatform {
    fn into_module(self) -> Module {
        self.module
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

///Retract a fact to state
#[macro_rules_attribute(dyn_async!)]
async fn retract_fact<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
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
        SYMBOL_SIMU_MODE => {
            /*
            (defmacro retract
             (lambda args
                 `(define state (remove-key-value-map state (quote ,args)))))
              */
            let state = match env.get_symbol(STATE) {
                Some(lv) => lv,
                None => {
                    return Err(SpecialError(
                        RAE_RETRACT,
                        "state not defined in env".to_string(),
                    ))
                }
            };

            remove_key_value_map(&[state, args.into()], env)
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

///Add a fact to fact state
#[macro_rules_attribute(dyn_async!)]
async fn assert_fact<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
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
        SYMBOL_SIMU_MODE => {
            /*
            (defmacro assert
                (lambda args
                    `(define state (set-map state (quote ,args)))))
             */
            let state = match env.get_symbol(STATE) {
                Some(lv) => lv,
                None => {
                    return Err(SpecialError(
                        RAE_ASSERT,
                        "state not defined in env.".to_string(),
                    ))
                }
            };

            set_map(&[state, args.into()], env)
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

//Return the labels of the methods

fn get_instantiated_methods(args: &[LValue], env: &LEnv) -> LResult {
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
                            .push(cons(&[method.clone(), task_args.clone()], env).unwrap());
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

fn get_best_method(args: &[LValue], env: &LEnv) -> LResult {
    /*ompas_utils::log::send(format!("env in get_best_method :\n {}", env));
    let task_methods_map = env.get_symbol(RAE_TASK_METHODS_MAP);
    ompas_utils::log::send(format!(
        "In get-best-method, task_methods_map: {:?}\n",
        task_methods_map
    ));*/

    let methods = get_instantiated_methods(args, env)?;
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
            TypeLValue::List,
        ));
    };

    let method_instance = cons(&[best_method, task_args.into()], env)?;
    //log::send(format!("instance of the method: {}\n", method_instance));

    Ok(method_instance)
}

#[macro_rules_attribute(dyn_async!)]
async fn get_facts<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            let mut state: im::HashMap<LValue, LValue> = get_state(&[], env).await?.try_into()?;
            let locked: Vec<LValue> = get_list_locked(&[], env).await?.try_into()?;

            for e in locked {
                state.insert(vec![LOCKED.into(), e].into(), True);
            }
            Ok(state.into())
        }
        SYMBOL_SIMU_MODE => Ok(env
            .get_symbol(STATE)
            .expect("state should be defined in simu mode")),
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn get_state<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

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
                    TypeLValue::Symbol,
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

    let state = ctx.state.get_state(_type).await.into_map();
    Ok(state)
}

#[macro_rules_attribute(dyn_async!)]
async fn get_state_variable<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_GET_STATE_VARIBALE,
            args.into(),
            0,
            1..std::usize::MAX,
        ));
    }

    let rae_mode: String = env
        .get_symbol(SYMBOL_RAE_MODE)
        .expect("{} not defined")
        .try_into()?;

    let platform_defined = ctx.platform_interface.is_some();

    if rae_mode == SYMBOL_EXEC_MODE && platform_defined {
        let key: LValueS = if args.len() > 1 {
            LValue::from(args).into()
        } else {
            args[0].clone().into()
        };

        let state = ctx.state.get_state(None).await;

        let value = state.inner.get(&key).unwrap_or(&LValueS::Bool(false));
        //println!("value: {}", value);

        Ok(value.into())
    } else if rae_mode != SYMBOL_RAE_MODE && rae_mode != SYMBOL_SIMU_MODE {
        return Err(SpecialError(
            RAE_GET_STATE_VARIBALE,
            format!(
                "RAE_MODE must have the value {} or {}",
                SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE,
            ),
        ));
    } else {
        get_map(&[env.get_symbol(STATE).unwrap(), args.into()], env)
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn get_status<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let status = ctx.actions_progress.status.read().await;

    let mut string = "Action(s) Status\n".to_string();

    for e in status.iter() {
        string.push_str(format!("- {}: {}\n", e.0, e.1).as_str())
    }

    Ok(LValue::String(string))
}

#[macro_rules_attribute(dyn_async!)]
async fn monitor<'a>(args: &'a [LValue], _: &'a LEnv) -> LResult {
    //info!("wait on function");
    //println!("wait on function with {} args", args.len());
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            MONITOR,
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
async fn select<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let task = args[0].clone();

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

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
async fn get_next_method<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_GET_NEXT_METHOD,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if let LValue::Number(LNumber::Usize(task_id)) = &args[0] {
        let next_method = ctx.agenda.get_next_applicable_method(task_id).await;
        Ok(next_method)
    } else {
        Err(WrongType(
            RAE_GET_NEXT_METHOD,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Usize,
        ))
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn set_success_for_task<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

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
            TypeLValue::Usize,
        ))
    }
}

pub fn success(args: &[LValue], _: &LEnv) -> LResult {
    Ok(vec![LValue::from(SUCCESS), args.into()].into())
}

pub fn failure(args: &[LValue], _: &LEnv) -> LResult {
    Ok(vec![LValue::from(FAILURE), args.into()].into())
}

pub fn is_failure(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            IS_FAILURE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(s) = &list[0] {
            match s.as_str() {
                SUCCESS => Ok(false.into()),
                FAILURE => Ok(true.into()),
                _ => Err(WrongType(
                    IS_FAILURE,
                    list[0].clone(),
                    (&list[0]).into(),
                    TypeLValue::Other("{success,failure}".to_string()),
                )),
            }
        } else {
            Err(WrongType(
                IS_FAILURE,
                list[0].clone(),
                (&list[0]).into(),
                TypeLValue::Other("{success,failure}".to_string()),
            ))
        }
    } else {
        Err(WrongType(
            IS_FAILURE,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}

pub fn is_success(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            IS_SUCCESS,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(s) = &list[0] {
            match s.as_str() {
                SUCCESS => Ok(true.into()),
                FAILURE => Ok(false.into()),
                _ => Err(WrongType(
                    IS_SUCCESS,
                    list[0].clone(),
                    (&list[0]).into(),
                    TypeLValue::Other("{success,failure}".to_string()),
                )),
            }
        } else {
            Err(WrongType(
                IS_SUCCESS,
                list[0].clone(),
                (&list[0]).into(),
                TypeLValue::Other("{success,failure}".to_string()),
            ))
        }
    } else {
        Err(WrongType(
            IS_SUCCESS,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}

/*
let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            todo!()
        }
        SYMBOL_SIMU_MODE => {
            todo!()
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
 */
