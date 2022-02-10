use crate::context::mutex;
use crate::context::mutex::MutexResponse;
use crate::module::rae_exec::{SYMBOL_EXEC_MODE, SYMBOL_RAE_MODE, SYMBOL_SIMU_MODE};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::root_module::map::get_map;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_utils::dyn_async;
use std::convert::TryInto;

pub const MACRO_MUTEX_LOCK_AND_DO: &str = "(defmacro mutex::lock-and-do
    (lambda (r p b)
        `(begin
            (lock ,r ,p)
            ,b
            (release ,r))))";
pub const LOCK: &str = "lock";
//pub const RELEASE: &str = "release";
pub const IS_LOCKED: &str = "locked?";

#[macro_rules_attribute(dyn_async!)]
pub async fn lock<'a>(args: &'a [LValue], _: &'a LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(LOCK, args.into(), args.len(), 2..2));
    }

    let ressource = if let LValue::Symbol(s) = args[0].clone() {
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

    match mutex::lock(ressource, priority).await {
        MutexResponse::Ok => Ok(LValue::True),
        MutexResponse::Wait(mut rx) => {
            rx.recv().await;
            Ok(LValue::True)
        }
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn release<'a>(args: &'a [LValue], _: &'a LEnv) -> LResult {
    mutex::release(args[0].clone()).await;
    Ok(LValue::True)
}

#[macro_rules_attribute(dyn_async!)]
pub async fn is_locked<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => Ok(mutex::is_locked(args[0].clone()).await.into()),
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
