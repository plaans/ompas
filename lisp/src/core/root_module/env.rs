use crate::core::root_module::env::language::ENV_GET_MACRO;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError;
use crate::core::structs::lerror::LError::{WrongNumberOfArgument, WrongType};
use crate::core::structs::lvalue::LValue;
use crate::core::structs::typelvalue::TypeLValue;

pub mod language {

    pub const ENV_GET_KEYS: &str = "env.get_keys"; //return a list of keys of the environment
    pub const ENV_GET_MACROS: &str = "env.get_macros";
    pub const ENV_GET_MACRO: &str = "env.get_macro";
}

/// Returns a list of all the keys present in the environment
pub fn env_get_keys(_: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(env
        .keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn env_get_macros(_: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(env
        .macros()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn env_get_macro(args: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            ENV_GET_MACRO,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    if let LValue::Symbol(s) = &args[0] {
        Ok(match env.get_macro(s).cloned() {
            Some(l) => l.into(),
            None => LValue::Nil,
        })
    } else {
        Err(WrongType(
            ENV_GET_MACRO,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Symbol,
        ))
    }
}
