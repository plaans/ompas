use crate::exec::refinement::*;
use ::macro_rules_attribute::macro_rules_attribute;
use futures::FutureExt;
use ompas_rae_language::exec::ARBITRARY;
use sompas_core::eval;
use sompas_core::modules::list::car;
use sompas_macros::{async_scheme_fn, scheme_fn};
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::list;
use sompas_structs::lmodule::LModule;
use sompas_structs::lprimitives::LPrimitives;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use std::string::String;

pub mod mode;
pub mod platform;
pub mod refinement;
pub mod resource;
pub mod state;
pub mod task;

/*
LANGUAGE
 */

pub const MOD_RAE_EXEC: &str = "mod-rae-exec";

pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

///Context that will contains primitives for the RAE executive
#[derive(Default)]
pub struct ModExec {}

impl From<ModExec> for LModule {
    fn from(m: ModExec) -> Self {
        todo!()
    }
}

/*//Return the labels of the methods
fn get_instantiated_methods(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            GET_INSTANTIATED_METHODS,
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
}*/

/*fn get_best_method(env: &LEnv, args: &[LValue]) -> LResult {
    let methods = get_instantiated_methods(env, args)?;
    let task_args = &args[1..];
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

    Ok(method_instance)
}*/

#[async_scheme_fn]
pub async fn arbitrary(env: &LEnv, args: &[LValue]) -> LResult {
    match args.len() {
        1 => car(env, &[args[0].clone()]),
        2 => {
            eval(
                &vec![
                    args[1].clone(),
                    vec![LPrimitives::Quote.into(), args[0].clone()].into(),
                ]
                .into(),
                &mut env.clone(),
                None,
            )
            .await
        }
        _ => Err(LRuntimeError::wrong_number_of_args(ARBITRARY, args, 1..2)),
    }
}
