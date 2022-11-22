use crate::contexts::ctx_rae::{CtxRae, CTX_RAE};
use crate::contexts::ctx_state::{CtxState, CTX_STATE};
use crate::contexts::ctx_task::{define_parent_task, DEFINE_PARENT_TASK};
use crate::exec::platform::*;
use crate::exec::rae_resource::{
    acquire, acquire_in_list, get_list_resources, is_locked, new_resource, release,
};
use crate::exec::refinement::{
    refine, retry, set_success_for_task, LAMBDA_COMPUTE_SCORE, LAMBDA_EVAL_PRE_CONDITIONS,
    LAMBDA_GET_ACTION_MODEL, LAMBDA_GET_PRECONDITIONS, LAMBDA_GET_SCORE, LAMBDA_IS_APPLICABLE,
    LAMBDA_RAE_EXEC_TASK, LAMBDA_RAE_RETRY,
};
use ::macro_rules_attribute::macro_rules_attribute;
use futures::FutureExt;
use im::HashMap;
use ompas_rae_language::*;
use ompas_rae_structs::state::world_state::*;
use sompas_core::eval;
use sompas_core::modules::list::cons;
use sompas_core::modules::map::get_map;
use sompas_macros::{async_scheme_fn, scheme_fn};
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::interrupted;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use sompas_structs::module::{InitLisp, IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::{list, lruntimeerror, wrong_type};
use std::borrow::Borrow;
use std::convert::TryInto;
use std::string::String;

pub mod platform;
pub mod rae_resource;
pub mod refinement;

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

///Context that will contains primitives for the RAE executive
#[derive(Default)]
pub struct CtxRaeExec {}

impl IntoModule for CtxRaeExec {
    fn into_module(self) -> Module {
        let init: InitLisp = vec![
            //MACRO_RESOURCE_ACQUIRE_AND_DO,
            //MACRO_MUTEX_LOCK_IN_LIST_AND_DO,
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
        module.add_async_fn_prelude(RAE_READ_STATE, read_state);
        module.add_async_fn_prelude(RAE_EXEC_COMMAND, exec_command);
        module.add_async_fn_prelude(RAE_LAUNCH_PLATFORM, launch_platform);
        //module.add_async_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_async_fn_prelude(RAE_CANCEL_COMMAND, cancel_command);
        module.add_async_fn_prelude(RAE_INSTANCE, instance);
        module.add_async_fn_prelude(RAE_INSTANCES, instances);
        module.add_fn_prelude(RAE_IS_PLATFORM_DEFINED, is_platform_defined);
        //Manage facts:
        module.add_async_fn_prelude(RAE_ASSERT, assert_fact);
        module.add_async_fn_prelude(RAE_ASSERT_SHORT, assert_fact);
        module.add_async_fn_prelude(RAE_RETRACT, retract_fact);
        module.add_async_fn_prelude(RAE_RETRACT_SHORT, retract_fact);
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
        module.add_async_fn_prelude(ACQUIRE, acquire);
        module.add_async_fn_prelude(RELEASE, release);
        module.add_async_fn_prelude(NEW_RESOURCE, new_resource);
        module.add_async_fn_prelude(IS_LOCKED, is_locked);
        module.add_async_fn_prelude(ACQUIRE_LIST, get_list_resources);
        module.add_async_fn_prelude(ACQUIRE_IN_LIST, acquire_in_list);

        //Manage hierarchy
        module.add_mut_fn_prelude(DEFINE_PARENT_TASK, define_parent_task);

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

///Retract a fact to state
#[async_scheme_fn]
async fn retract_fact(env: &LEnv, key: LValueS, value: LValueS) -> Result<(), LRuntimeError> {
    let ctx_state = env.get_context::<CtxState>(CTX_STATE)?;

    ctx_state.state.retract_fact(key, value).await
}

///Add a fact to fact state
#[async_scheme_fn]
async fn assert_fact(env: &LEnv, key: LValueS, value: LValueS) -> Result<(), LRuntimeError> {
    let ctx_state = env.get_context::<CtxState>(CTX_STATE)?;
    ctx_state.state.add_fact(key, value).await;
    Ok(())
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
    let mut state: im::HashMap<LValue, LValue> = get_state(env, &[]).await?.try_into()?;
    let locked: Vec<LValue> = get_list_resources(env, &[]).await?.try_into()?;

    for e in locked {
        state.insert(vec![LOCKED.into(), e].into(), LValue::True);
    }

    Ok(state.into())
}

#[async_scheme_fn]
async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx_state = env.get_context::<CtxState>(CTX_STATE)?;

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

    let state = ctx_state.state.get_state(_type).await.into_map();
    Ok(state)
}

#[async_scheme_fn]
async fn read_state(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_READ_STATE,
            args,
            1..usize::MAX,
        ));
    }

    let key: LValue = if args.len() > 1 {
        args.into()
    } else {
        args[0].clone()
    };

    let facts: LValue = get_facts(env, &[]).await?;
    get_map(env, &[facts, key])
}

#[async_scheme_fn]
async fn wait_for(env: &LEnv, lv: LValue) -> Result<LAsyncHandler, LRuntimeError> {
    let (tx, mut rx) = new_interruption_handler();
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;
    let monitors = ctx.monitors.clone();

    let mut env = env.clone();
    let f: LFuture = (Box::pin(async move {
        //println!("wait-for: {}", lv);
        if let LValue::True = eval(&lv, &mut env, None).await.unwrap() {
            //println!("wait-for: {} already true", lv);
            Ok(LValue::Nil)
        } else {
            let handler = monitors.add_waiter(lv.clone()).await;
            let id = *handler.id();
            //println!("wait-for: waiting on {}", lv);
            tokio::select! {
                _ = rx.recv() => {
                        //println!("wait-for: waiter no longer needed");
                        monitors.remove_waiter(id).await;
                        Ok(interrupted!())
                }
                _ = handler.recv() => {
                    //println!("success for waiter");
                    Ok(LValue::Nil)
                }
            }
        }
    }) as FutureResult)
        .shared();

    tokio::spawn(f.clone());

    Ok(LAsyncHandler::new(f, tx))
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

//0 arg: return a map of all instances
//1 arg: return all instances of a type
//2 args: check if an instance is of a certain type
#[async_scheme_fn]
pub async fn instance(env: &LEnv, object: String, r#type: String) -> LResult {
    let state = &env.get_context::<CtxState>(CTX_STATE)?.state;

    /*match args.len() {
        0 => Ok(state.get_state(Some(StateType::Instance)).await.into_map()),
        1 => Ok(state.instances(args[0].borrow().try_into()?).await),
        2 => Ok(state
            .instance(args[0].borrow().try_into()?, args[1].borrow().try_into()?)
            .await),
        _ => Err(LRuntimeError::wrong_number_of_args(
            "godot::instance",
            args,
            1..2,
        )),
    }*/

    Ok(state.instance(object, r#type).await)
}

#[async_scheme_fn]
pub async fn instances(env: &LEnv, r#type: String) -> LResult {
    let state = &env.get_context::<CtxState>(CTX_STATE)?.state;
    Ok(state.instances(r#type).await)
}
