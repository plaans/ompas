use crate::core::language::ERR;
use crate::core::root_module::error::language::*;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::{WrongNumberOfArgument, WrongType};
use crate::core::structs::lerror::LResult;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::typelvalue::TypeLValue;

//LANGUAGE
pub mod language {
    pub const IS_ERR: &str = "err?";
    pub const CHECK: &str = "check";

    pub const DOC_ERR: &str = "Return an LValue (err <expr>)";
    pub const DOC_IS_ERR: &str =
        "Return true if the argument is an lvalue of the form (err <expr>), false otherwise.";
    pub const DOC_CHECK: &str = "todo!";
}

#[derive(Default)]
pub struct CtxError {}

pub fn err(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(ERR, args.into(), args.len(), 1..1));
    }
    Ok(LValue::Err(Box::new(args[0].clone())))
}

pub fn is_err(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(IS_ERR, args.into(), args.len(), 1..1));
    }

    Ok(matches!(args[0], LValue::Err(_)).into())
}

pub fn check(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(CHECK, args.into(), args.len(), 1..1));
    }

    match args[0] {
        LValue::Nil => Ok(LValue::Err(Box::new(LValue::Nil))),
        LValue::True => Ok(LValue::True),
        _ => Err(WrongType(
            CHECK,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Bool,
        )),
    }
}
