use crate::rae_exec::*;
use log::info;
use ompas_rae_language::IS_LOCKED;
use ompas_rae_structs::mutex::Wait;
use ompas_rae_structs::resource::{AcquireResponse, Capacity, ResourceHandler};
use sompas_core::modules::map::get_map;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_utils::dyn_async;
use sompas_utils::other::generic_race;
use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};

pub const MACRO_RESOURCE_ACQUIRE_AND_DO: &str = "(defmacro resource::acquire-and-do
    (lambda (r p b)
        `(begin
            (acquire ,r ,p)
            ,b
            (release ,r))))";

pub const MACRO_MUTEX_LOCK_IN_LIST_AND_DO: &str = "(defmacro mutex::lock-in-list-and-do
    (lambda (l p b)
        `(begin
            (define r (lock-in-list ,l ,p))
            ,b
            (release r))))";

#[async_scheme_fn]
pub async fn new_resource(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE, args, 1..2))?
        .try_into()?;

    let capacity: Option<Capacity> = match args.get(1) {
        None => None,
        Some(lv) => Some(Capacity::Some(lv.try_into()?)),
    };

    ctx.resources.new_resource(label, capacity).await;
    Ok(())
}

///Lock a resource
/// Waits on the resource until its his turn in the queue list
#[async_scheme_fn]
pub async fn acquire(env: &LEnv, args: &[LValue]) -> Result<LAsyncHandler, LRuntimeError> {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let resources = ctx.resources.clone();

    let (tx, mut rx) = new_interruption_handler();

    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE, args, 1..2))?
        .try_into()?;

    let capacity = match args.get(1) {
        None => Capacity::All,
        Some(lv) => Capacity::Some(lv.try_into()?),
    };

    let others = vec![];
    //let others = args[2..].to_vec();

    let f: LFuture = (Box::pin(async move {
        info!("Acquiring {} with {} capacity", label, capacity);
        let rh: ResourceHandler = match resources.acquire(label.clone(), capacity, others).await? {
            AcquireResponse::Ok(rh) => rh,
            AcquireResponse::Wait(mut wait) => {
                info!("waiting on resource {}", label);

                tokio::select! {
                    _ = rx.recv() => {
                        return Ok(LValue::Err(LValue::Nil.into()))
                    }
                    rh = wait.recv() => {
                        info!("resource {} unlocked!!!", label);
                        rh
                    }
                }
            }
        };

        let rc = resources.clone();
        let (tx, mut rx) = new_interruption_handler();

        let f: LFuture = (Box::pin(async move {
            rx.recv().await;
            rc.release(rh).await.map(|_| LValue::Nil)
        }) as FutureResult)
            .shared();

        let f2 = f.clone();

        tokio::spawn(f);

        info!("{} acquired with {} capacity", label, capacity);

        Ok(LAsyncHandler::new(f2, tx).into())
    }) as FutureResult)
        .shared();

    let f2 = f.clone();

    tokio::spawn(f);

    Ok(LAsyncHandler::new(f2, tx))
}

/// Release the resource
#[async_scheme_fn]
pub async fn release(mut h: LAsyncHandler) -> LResult {
    h.interrupt().await
}

#[macro_rules_attribute(dyn_async!)]
async fn check_receiver<'a>(mut wait: (String, Wait)) -> String {
    wait.1.rx.recv().await;
    wait.0
}

#[macro_rules_attribute(dyn_async!)]
async fn check_acquire<'a>(
    arg: (LEnv, String, Option<LValue>),
) -> Result<(String, LAsyncHandler), LRuntimeError> {
    let (env, label, capacity) = arg;

    let mut args: Vec<LValue> = vec![label.clone().into()];
    if let Some(c) = capacity {
        args.push(c)
    }
    let r = acquire(&env, args.as_slice()).await?;
    if let LValue::Handler(h) = r {
        Ok((label, h))
    } else {
        panic!()
    }
}
/// Ask to lock a resource in a list
/// Returns the resource that has been locked.
#[async_scheme_fn]
pub async fn acquire_in_list(env: &LEnv, args: &[LValue]) -> Result<LAsyncHandler, LRuntimeError> {
    let (tx, mut rx) = new_interruption_handler();
    let mut resources: Vec<LValue> = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE_IN_LIST, args, 1..2))?
        .try_into()?;
    let capacity = args.get(1).cloned();

    let env = env.clone();

    let f: LFuture = (Box::pin(async move {
        let mut resources: Vec<String> = resources.drain(..).map(|lv| lv.to_string()).collect();

        let receivers: Vec<(LEnv, String, Option<LValue>)> = resources
            .iter_mut()
            .map(|r| (env.clone(), r.clone(), capacity.clone()))
            .collect();

        tokio::select! {
            _ = rx.recv() =>  {
                Ok(LValue::Err(LValue::Nil.into()))
            }
            r = generic_race(receivers, check_acquire) => {
                let r = r?;
                //Retunrs a list with the label of the resource that has been acquired, along the handler
                Ok(list![r.0.into(), r.1.into()])
            }
        }
    }) as FutureResult)
        .shared();

    let f2 = f.clone();

    tokio::spawn(f2);

    Ok(LAsyncHandler::new(f, tx))
}

#[async_scheme_fn]
pub async fn is_locked(env: &LEnv, args: &[LValue]) -> LResult {
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    match mode.as_str() {
        SYMBOL_EXEC_MODE => Ok(ctx
            .resources
            .is_locked(args[0].borrow().try_into()?)
            .await?
            .into()),
        SYMBOL_SIMU_MODE => {
            let state = match env.get_symbol("state") {
                Some(lv) => lv,
                None => {
                    return Err(lruntimeerror!(
                        IS_LOCKED,
                        "state should be defined in simu mode".to_string()
                    ))
                }
            };
            get_map(env, &[state, args.into()])
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[async_scheme_fn]
pub async fn get_list_resources(env: &LEnv) -> Result<Vec<String>, LRuntimeError> {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    Ok(ctx.resources.get_list_resources().await)
}
