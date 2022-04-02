use crate::context::mutex;
use crate::context::mutex::{MutexResponse, Wait};
use crate::module::rae_exec::{SYMBOL_EXEC_MODE, SYMBOL_RAE_MODE, SYMBOL_SIMU_MODE};
use ::macro_rules_attribute::macro_rules_attribute;
use log::info;
use ompas_lisp::core::root_module::map::get_map;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_utils::dyn_async;
use ompas_utils::other::generic_race;
use std::convert::{TryFrom, TryInto};

pub const MACRO_MUTEX_LOCK_AND_DO: &str = "(defmacro mutex::lock-and-do
    (lambda (r p b)
        `(begin
            (lock ,r ,p)
            ,b
            (release ,r))))";
pub const LOCK: &str = "lock";
pub const RELEASE: &str = "release";
pub const IS_LOCKED: &str = "locked?";
pub const LOCKED: &str = "locked";
pub const LOCKED_LIST: &str = "locked-list";
pub const LOCK_IN_LIST: &str = "lock-in-list";

pub const MACRO_MUTEX_LOCK_IN_LIST_AND_DO: &str = "(defmacro mutex::lock-in-list-and-do
    (lambda (l p b)
        `(begin
            (define r (lock-in-list ,l ,p))
            ,b
            (release r))))";

///Lock a resource
/// Waits on the resource until its his turn in the queue list
#[macro_rules_attribute(dyn_async!)]
pub async fn lock<'a>(args: &'a [LValue], _: &'a LEnv) -> LResult {
    info!("rae_exec::lock({})", LValue::from(args));

    if args.len() != 2 {
        return Err(WrongNumberOfArgument(LOCK, args.into(), args.len(), 2..2));
    }

    let resource = if let LValue::Symbol(s) = args[0].clone() {
        s
    } else {
        return Err(WrongType(
            LOCK,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Symbol,
        ));
    };
    let priority = if let LValue::Number(LNumber::Int(i)) = &args[1] {
        *i as usize
    } else {
        return Err(WrongType(
            LOCK,
            args[1].clone(),
            (&args[1]).into(),
            TypeLValue::Number,
        ));
    };

    match mutex::lock(resource.clone(), priority).await {
        MutexResponse::Ok => Ok(LValue::True),
        MutexResponse::Wait(mut wait) => {
            info!("waiting on resource {}", resource);
            wait.rx.recv().await;
            info!("resource {} unlocked!!!", resource);
            Ok(LValue::True)
        }
    }
}

/// Release the resource
#[macro_rules_attribute(dyn_async!)]
pub async fn release<'a>(args: &'a [LValue], _: &'a LEnv) -> LResult {
    mutex::release((&args[0]).try_into()?).await;
    Ok(LValue::True)
}

#[macro_rules_attribute(dyn_async!)]
async fn check_receiver<'a>(mut wait: (String, Wait)) -> String {
    wait.1.rx.recv().await;
    wait.0
}

/// Ask to lock a resource in a list
/// Returns the resource that has been locked.
#[macro_rules_attribute(dyn_async!)]
pub async fn lock_in_list<'a>(args: &'a [LValue], _: &'a LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            LOCK_IN_LIST,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    //println!("Getting first available in {}", args[0]);

    let mut resources: Vec<LValue> = (&args[0]).try_into()?;

    let mut resources: Vec<String> = resources.drain(..).map(|lv| lv.to_string()).collect();
    let priority: usize = i64::try_from(&args[1])? as usize;
    let mut receivers: Vec<(String, Wait)> = vec![];

    let mut r = None;

    for resource in resources.drain(..) {
        match mutex::lock(resource.clone(), priority).await {
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

#[macro_rules_attribute(dyn_async!)]
pub async fn is_locked<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
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
                    return Err(SpecialError(
                        IS_LOCKED,
                        "state should be defined in simu mode".to_string(),
                    ))
                }
            };
            get_map(&[state, args.into()], env)
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn get_list_locked<'a>(_: &'a [LValue], _: &'a LEnv) -> LResult {
    let locked = mutex::get_list_locked()
        .await
        .iter()
        .map(|s| s.into())
        .collect::<Vec<LValue>>();
    Ok(locked.into())
}
