use crate::contexts::ctx_rae::{CtxRae, CTX_RAE};
use crate::exec::*;
use ompas_rae_language::IS_LOCKED;
use ompas_rae_structs::mutex::Wait;
use ompas_rae_structs::resource::{AcquireResponse, Capacity, ResourceHandler};
use ompas_utils::dyn_async;
use ompas_utils::other::generic_race;
use rand::{thread_rng, Rng};
use sompas_core::modules::map::get_map;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
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

pub const PRIORITY: &str = ":priority";

#[async_scheme_fn]
pub async fn new_resource(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;

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
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;

    let resources = ctx.resources.clone();

    let (tx, mut rx) = new_interruption_handler();

    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE, args, 1..2))?
        .try_into()?;

    let mut priority: usize = 0;
    let mut capacity = Capacity::All;
    //init of capacity and
    match args.len() {
        1 => {}
        2 => {
            if let LValue::List(l) = &args[1] {
                if l.get(0).unwrap() == &PRIORITY.into() && l.len() == 2 {
                    priority = l.get(1).unwrap().try_into()?;
                } else {
                    Err(LRuntimeError::new("", ""))?
                }
            } else {
                capacity = Capacity::Some(args[1].borrow().try_into()?);
            }
        }
        3 => {
            capacity = Capacity::Some(args[1].borrow().try_into()?);
            if let LValue::List(l) = &args[1] {
                if l.get(0).unwrap() == &PRIORITY.into() && l.len() == 2 {
                    priority = l.get(1).unwrap().try_into()?;
                } else {
                    Err(LRuntimeError::new("", ""))?
                }
            } else {
                Err(LRuntimeError::new("", ""))?
            }
        }
        _ => {}
    }

    let f: LFuture = (Box::pin(async move {
        info!(
            "Acquiring {}; capacity = {}; priority = {}",
            label, capacity, priority
        );
        let rh: ResourceHandler = match resources.acquire(label.clone(), capacity, priority).await?
        {
            AcquireResponse::Ok(rh) => rh,
            AcquireResponse::Wait(mut wait) => {
                info!("Waiting on resource {}", label);

                tokio::select! {
                    _ = rx.recv() => {
                        info!("Acquisition of {} cancelled.", label);
                        resources.remove_waiter(wait).await;
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
            info!("Releasing {}", rh.get_label());
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
async fn check_acquire<'a>(arg: (LEnv, Vec<LValue>, usize)) -> LResult {
    let (env, args, d) = arg;
    let label = args[0].clone();
    let r = acquire(&env, args.as_slice()).await?;
    let h_await: LAsyncHandler = r.try_into().unwrap();
    let h: LValue = h_await.get_future().await?;
    info!("{} unlocked {}", d, label);
    Ok(list![label, h])
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
    let d: usize = thread_rng().gen();
    info!("Acquire element from {}, id: {} ", args[0], d);
    //let capacity = args.get(1).cloned();
    let rest = if args.len() > 1 {
        args[1..].to_vec()
    } else {
        vec![]
    };

    let env = env.clone();

    let f: LFuture = (Box::pin(async move {
        let receivers: Vec<(LEnv, Vec<LValue>, usize)> = resources
            .iter_mut()
            .map(|r| {
                let mut vec = vec![r.clone()];
                vec.append(&mut rest.clone());
                (env.clone(), vec, d)
            })
            .collect();

        tokio::select! {
            _ = rx.recv() =>  {
                Ok(LValue::Err(LValue::Nil.into()))
            }
            r = generic_race(receivers, check_acquire) => {
                r
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
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;

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
    let ctx = env.get_context::<CtxRae>(CTX_RAE)?;

    Ok(ctx.resources.get_list_resources().await)
}
