use crate::rae_exec::*;
use log::info;
use ompas_rae_language::IS_LOCKED;
use ompas_rae_structs::exec_context::mutex;
use ompas_rae_structs::exec_context::mutex::*;
use sompas_core::modules::map::get_map;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror;
use sompas_structs::lerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_utils::dyn_async;
use sompas_utils::other::generic_race;
use std::convert::{TryFrom, TryInto};

///Lock a resource
/// Waits on the resource until its his turn in the queue list
#[async_scheme_fn]
pub async fn lock(resource: String, priority: i64) {
    info!("rae_exec::lock({})", resource);

    match mutex::lock(resource.clone(), priority as usize).await {
        MutexResponse::Ok => (),
        MutexResponse::Wait(mut wait) => {
            info!("waiting on resource {}", resource);
            wait.rx.recv().await;
            info!("resource {} unlocked!!!", resource);
        }
    }
}

/// Release the resource
#[async_scheme_fn]
pub async fn release(s: String) {
    mutex::release(&s).await;
}

#[macro_rules_attribute(dyn_async!)]
async fn check_receiver<'a>(mut wait: (String, Wait)) -> String {
    wait.1.rx.recv().await;
    wait.0
}

/// Ask to lock a resource in a list
/// Returns the resource that has been locked.
#[async_scheme_fn]
pub async fn lock_in_list(env: &LEnv, mut resources: Vec<LValue>, priority: i64) -> LResult {
    let mut resources: Vec<String> = resources.drain(..).map(|lv| lv.to_string()).collect();
    let mut receivers: Vec<(String, Wait)> = vec![];

    let mut r = None;

    for resource in resources.drain(..) {
        match mutex::lock(resource.clone(), priority as usize).await {
            MutexResponse::Ok => {
                //println!("{} is already available!", resource);
                r = Some(resource);
                break;
            }
            MutexResponse::Wait(rx) => receivers.push((resource, rx)),
        }
    }

    match r {
        Some(r) => Ok(r.into()),
        None => {
            //println!("none are available for the moment...");
            let r = generic_race(receivers, check_receiver).await;
            //println!("{} is now available!", r);
            Ok(r.into())
        }
    }
}

#[async_scheme_fn]
pub async fn is_locked(env: &LEnv, args: &[LValue]) -> LResult {
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => Ok(mutex::is_locked((&args[0]).try_into()?).await.into()),
        SYMBOL_SIMU_MODE => {
            let state = match env.get_symbol("state") {
                Some(lv) => lv,
                None => {
                    return Err(lerror!(
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
pub async fn get_list_locked() -> Vec<LValue> {
    mutex::get_list_locked()
        .await
        .iter()
        .map(|s| s.into())
        .collect::<Vec<LValue>>()
}
