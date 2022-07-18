use crate::rae_exec::algorithms::*;
use crate::rae_exec::platform::*;
use crate::rae_exec::rae_mutex::{get_list_locked, is_locked, lock, lock_in_list, release};
use ::macro_rules_attribute::macro_rules_attribute;
use async_trait::async_trait;
use ompas_rae_core::monitor::add_waiter;
use ompas_rae_language::*;
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::context::RAE_TASK_METHODS_MAP;
use ompas_rae_structs::job::Job;
use ompas_rae_structs::state::task_status::TaskStatus;
use ompas_rae_structs::state::world_state::*;
use ompas_rae_structs::TaskId;
use sompas_core::eval;
use sompas_core::modules::list::cons;
use sompas_core::modules::map::{get_map, remove_key_value_map, set_map};
use sompas_macros::{async_scheme_fn, scheme_fn};
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalue::LValue::Nil;
use sompas_structs::lvalues::LValueS;
use sompas_structs::module::{InitLisp, IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::{list, lruntimeerror, wrong_n_args, wrong_type};
use std::any::Any;
use std::borrow::Borrow;
use std::convert::TryInto;
use std::string::String;
use std::sync::Arc;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::RwLock;

pub mod algorithms;
pub mod platform;
pub mod rae_mutex;

/*
LANGUAGE
 */

pub const MOD_RAE_EXEC: &str = "mod-rae-exec";
pub const STATE: &str = "state";

/*pub const MACRO_WAIT_ON: &str = "(defmacro monitor (lambda (expr)
`(if (not (eval ,expr))
    (monitor ,expr))))";*/
pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

pub const DEFINE_RAE_MODE: &str = "(define rae-mode EXEC-MODE)";
pub const SYMBOL_EXEC_MODE: &str = "exec-mode";
pub const SYMBOL_SIMU_MODE: &str = "simu-mode";
pub const SYMBOL_RAE_MODE: &str = "rae-mode";
//pub const DEFINE_PARENT_TASK: &str = "(define parent_task nil)";
pub const PARENT_TASK: &str = "parent_task";

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
    pub async fn init(&self, state: WorldState, agenda: Agenda) {
        self.inner.write().await.init(state, agenda).await;
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
    pub state: WorldState,
    pub platform_interface: Option<Platform>,
    pub agenda: Agenda,
}

impl IntoModule for CtxRaeExec {
    fn into_module(self) -> Module {
        let init: InitLisp = vec![
            MACRO_MUTEX_LOCK_AND_DO,
            MACRO_MUTEX_LOCK_IN_LIST_AND_DO,
            MACRO_SIM_BLOCK,
            LAMBDA_GET_PRECONDITIONS,
            LAMBDA_GET_SCORE,
            LAMBDA_GET_ACTION_MODEL,
            LAMBDA_EVAL_PRE_CONDITIONS,
            LAMBDA_COMPUTE_SCORE,
            DEFINE_RAE_MODE,
            LAMBDA_IS_APPLICABLE,
            DEFINE_ERR_ACTION_FAILURE,
            DEFINE_ERR_NO_APPLICABLE_METHOD,
            LAMBDA_RAE_RETRY,
            LAMBDA_MONITOR,
            MACRO_RUN_MONITORING,
            LAMBDA_RAE_EXEC_TASK, //DEFINE_PARENT_TASK,
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
        //module.add_async_fn_prelude(RAE_GET_STATUS, get_status);
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
        module.add_async_fn_prelude(RAE_WAIT_FOR, wait_for);
        //module.add_async_fn_prelude(RAE_SELECT, select);
        //module.add_async_fn_prelude(RAE_SET_SUCCESS_FOR_TASK, set_success_for_task);
        //module.add_async_fn_prelude(RAE_GET_NEXT_METHOD, get_next_method);

        //progress
        module.add_async_fn_prelude(REFINE, refine);
        module.add_async_fn_prelude(RETRY, retry);
        module.add_async_fn_prelude(RAE_SET_SUCCESS_FOR_TASK, set_success_for_task);
        /*module.add_async_fn_prelude(
            RAE_GENERATE_APPLICABLE_INSTANCES,
            generate_applicable_instances,
        );*/

        //mutex
        module.add_async_fn_prelude(LOCK, lock);
        module.add_async_fn_prelude(RELEASE, release);
        module.add_async_fn_prelude(IS_LOCKED, is_locked);
        module.add_async_fn_prelude(LOCKED_LIST, get_list_locked);
        module.add_async_fn_prelude(LOCK_IN_LIST, lock_in_list);

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
    pub async fn get_execution_status(&self, id: &TaskId) -> TaskStatus {
        self.agenda.get_status(id).await
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

/// Trait that a platform needs to implement to be able to be used as execution platform in RAE.
#[async_trait]
pub trait RAEInterface: Any + Send + Sync {
    /// Initial what needs to be.
    async fn init(&mut self, state: WorldState, agenda: Agenda);

    /// Executes a command on the platform
    async fn exec_command(&self, args: &[LValue], command_id: usize) -> LResult;

    async fn cancel_command(&self, args: &[LValue]) -> LResult;

    ///Launch the platform (such as the simulation in godot) and open communication
    async fn launch_platform(&mut self, args: &[LValue]) -> LResult;

    /// Start the platform process
    async fn start_platform(&mut self, args: &[LValue]) -> LResult;

    /// Open communication with the platform
    async fn open_com(&mut self, args: &[LValue]) -> LResult;

    /// Returns the RAE domain of the platform.
    async fn domain(&self) -> &'static str;

    async fn instance(&self, args: &[LValue]) -> LResult;

    fn context_platform(&self) -> CtxPlatform;
}

#[async_trait]
impl RAEInterface for () {
    async fn init(&mut self, _: WorldState, _: Agenda) {}

    async fn exec_command(&self, _args: &[LValue], _: usize) -> LResult {
        Ok(Nil)
    }

    async fn cancel_command(&self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn launch_platform(&mut self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn start_platform(&mut self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn open_com(&mut self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

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
#[async_scheme_fn]
async fn retract_fact(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            if args.len() != 2 {
                return Err(wrong_n_args!(RAE_RETRACT, args, 2));
            }
            let key = args[0].borrow().into();
            let value = args[1].borrow().into();
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
                    return Err(lruntimeerror!(
                        RAE_RETRACT,
                        "state not defined in env".to_string()
                    ))
                }
            };

            remove_key_value_map(env, &[state, args.into()])
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

///Add a fact to fact state
#[async_scheme_fn]
async fn assert_fact(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            if args.len() != 2 {
                return Err(wrong_n_args!(RAE_ASSERT, args, 2));
            }
            let key = args[0].borrow().into();
            let value = args[1].borrow().into();
            ctx.state.add_fact(key, value).await;

            Ok(LValue::True)
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
                    return Err(lruntimeerror!(
                        RAE_ASSERT,
                        "state not defined in env.".to_string()
                    ))
                }
            };

            set_map(env, &[state, args.into()])
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

//Return the labels of the methods

fn get_instantiated_methods(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_GET_INSTANTIATED_METHODS,
            args,
            1..usize::MAX,
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
                return Err(lruntimeerror!(
                    RAE_GET_INSTANTIATED_METHODS,
                    format!("no methods for {}", task_name)
                ))
            }
            Some(methods) => {
                //Got here the list of the symbol of the methods
                let mut instantiated_method = vec![];
                if let LValue::List(methods) = methods {
                    for method in methods.iter() {
                        //Handle here the case where it is needed to generate all instantiation of methods where several parameters are possible.
                        instantiated_method
                            .push(cons(env, &[method.clone(), task_args.clone()]).unwrap());
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

fn get_best_method(env: &LEnv, args: &[LValue]) -> LResult {
    /*ompas_utils::log::send(format!("env in get_best_method :\n {}", env));
    let task_methods_map = env.get_symbol(RAE_TASK_METHODS_MAP);
    ompas_utils::log::send(format!(
        "In get-best-method, task_methods_map: {:?}\n",
        task_methods_map
    ));*/

    let methods = get_instantiated_methods(env, args)?;
    let task_args = &args[1..];
    //log::send(format!("methods for {}: {}\n", LValue::from(args), methods));
    let best_method = if let LValue::List(methods) = methods {
        if methods.is_empty() {
            return Err(lruntimeerror!(
                RAE_GET_BEST_METHOD,
                "task has no applicable method".to_string()
            ));
        }
        methods[0].clone()
    } else {
        return Err(wrong_type!(RAE_GET_BEST_METHOD, &methods, KindLValue::List));
    };

    let method_instance = cons(env, &[best_method, task_args.into()])?;
    //log::send(format!("instance of the method: {}\n", method_instance));

    Ok(method_instance)
}

#[async_scheme_fn]
async fn get_facts(env: &LEnv) -> LResult {
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            let mut state: im::HashMap<LValue, LValue> = get_state(env, &[]).await?.try_into()?;
            let locked: Vec<LValue> = get_list_locked(env, &[]).await?.try_into()?;

            for e in locked {
                state.insert(vec![LOCKED.into(), e].into(), LValue::True);
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

#[async_scheme_fn]
async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let _type = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(sym) = &args[0] {
                match sym.as_str() {
                    KEY_STATIC => Some(StateType::Static),
                    KEY_DYNAMIC => Some(StateType::Dynamic),
                    KEY_INNER_WORLD => Some(StateType::InnerWorld),
                    KEY_INSTANCE => Some(StateType::Instance),
                    _ => {
                        return Err(lruntimeerror!(
                            RAE_GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD, KEY_INSTANCE
                            )
                        ))
                    }
                }
            } else {
                return Err(wrong_type!(RAE_GET_STATE, &args[0], KindLValue::Symbol));
            }
        }
        _ => {
            return Err(LRuntimeError::wrong_number_of_args(
                RAE_GET_STATE,
                args,
                0..1,
            ))
        }
    };

    let state = ctx.state.get_state(_type).await.into_map();
    Ok(state)
}

#[async_scheme_fn]
async fn get_state_variable(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_GET_STATE_VARIBALE,
            args,
            1..usize::MAX,
        ));
    }

    let rae_mode: String = env
        .get_symbol(SYMBOL_RAE_MODE)
        .expect("{} not defined")
        .try_into()?;

    let platform_defined = ctx.platform_interface.is_some();

    let key: LValue = if args.len() > 1 {
        args.into()
    } else {
        args[0].clone()
    };

    match rae_mode.as_str() {
        SYMBOL_EXEC_MODE => {
            if platform_defined {
                let state = ctx.state.get_state(None).await;

                let value = state
                    .inner
                    .get(&key.into())
                    .unwrap_or(&LValueS::Bool(false));
                //println!("value: {}", value);

                Ok(value.into())
            } else {
                let facts: LValue = get_facts(env, &[]).await?;
                get_map(env, &[facts, key])
            }
        }
        SYMBOL_SIMU_MODE => {
            let state = env.get_symbol(STATE).unwrap();
            get_map(env, &[state, key])
        }
        _ => Err(lruntimeerror!(
            RAE_GET_STATE_VARIBALE,
            format!(
                "RAE_MODE must have the value {} or {} (value = {}).",
                SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE, rae_mode,
            )
        )),
    }
}

/*#[async_scheme_fn]
async fn get_status<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let mut string = "Action(s) Status\n".to_string();

    for e in status.iter() {
        string.push_str(format!("- {}: {}\n", e.0, e.1).as_str())
    }

    Ok(LValue::String(string))
}*/

#[async_scheme_fn]
async fn wait_for(env: &LEnv, args: &[LValue]) -> LResult {
    //info!("wait on function");
    //println!("wait on function with {} args", args.len());
    /*pub const MACRO_WAIT_ON: &str = "(defmacro monitor (lambda (expr)
    `(if (not (eval ,expr))
        (monitor ,expr))))";*/

    if args.len() != 1 {
        return Err(wrong_n_args!(RAE_MONITOR, args, 1));
    }

    if let LValue::True = eval(&args[0], &mut env.clone(), None).await? {
    } else {
        let mut rx = add_waiter(args[0].clone()).await;
        //println!("receiver ok");

        if let false = rx.recv().await.expect("could not receive msg from waiters") {
            unreachable!("should not receive false from waiters")
        }
        //println!("end wait on");
    }
    Ok(LValue::True)
}
#[scheme_fn]
pub fn success(args: &[LValue]) -> LValue {
    list![LValue::from(SUCCESS), args.into()]
}
#[scheme_fn]
pub fn failure(args: &[LValue]) -> LValue {
    list![LValue::from(FAILURE), args.into()]
}

#[scheme_fn]
pub fn is_failure(list: Vec<LValue>) -> Result<bool, LRuntimeError> {
    if let LValue::Symbol(s) = &list[0] {
        match s.as_str() {
            SUCCESS => Ok(false),
            FAILURE => Ok(true),
            _ => Err(wrong_type!(
                IS_FAILURE,
                &list[0],
                KindLValue::Other("{success,failure}".to_string())
            )),
        }
    } else {
        Err(wrong_type!(IS_FAILURE, &list[0], KindLValue::Symbol))
    }
}

#[scheme_fn]
pub fn is_success(list: Vec<LValue>) -> Result<bool, LRuntimeError> {
    if let LValue::Symbol(s) = &list[0] {
        match s.as_str() {
            SUCCESS => Ok(true),
            FAILURE => Ok(false),
            _ => Err(wrong_type!(
                IS_SUCCESS,
                &list[0],
                KindLValue::Other("{success,failure}".to_string())
            )),
        }
    } else {
        Err(wrong_type!(IS_FAILURE, &list[0], KindLValue::Symbol))
    }
}
