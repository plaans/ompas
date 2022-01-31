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
    pub const DOC_ERR: &str = "Return an LValue::Err(LValue)";
    pub const DOC_IS_ERR: &str =
        "Return true if the argument is an lvalue of the form (err <expr>), false otherwise.";

    pub const CHECK: &str = "check";
    pub const DOC_CHECK: &str = "Return an LValue::Err if the LValue if false";
}

pub fn check(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(CHECK, args.into(), args.len(), 1..1));
    }
    match &args[0] {
        LValue::True => Ok(LValue::True),
        LValue::Nil => Ok(LValue::Err(Box::new(LValue::Nil))),
        _ => Err(WrongType(
            CHECK,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Bool,
        )),
    }
}

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

    if let LValue::Err(_) = &args[0] {
        Ok(LValue::True)
    } else {
        Ok(LValue::Nil)
    }
}

#[cfg(test)]
mod tests {
    use crate::core::root_module::basic_math::language::LE;
    use crate::core::root_module::error::{check, err, is_err};
    use crate::core::structs::lenv::LEnv;
    use crate::core::structs::lerror;
    use crate::core::structs::lvalue::LValue;

    #[test]
    pub fn test_err() -> lerror::Result<()> {
        let env = LEnv::default();
        let lv = 5.into();
        let result = err(&[lv], &env)?;

        assert_eq!(LValue::Err(Box::new(5.into())), result);
        Ok(())
    }

    #[test]
    pub fn test_is_err() -> lerror::Result<()> {
        let env = LEnv::default();
        let result = is_err(&[LValue::Err(Box::new(5.into()))], &env)?;
        assert_eq!(result, LValue::True);
        let result = is_err(&[5.into()], &env)?;
        assert_eq!(result, LValue::Nil);
        Ok(())
    }

    #[test]
    pub fn test_check() -> lerror::Result<()> {
        let env = LEnv::default();
        let result = check(&[LValue::Nil], &env)?;
        assert_eq!(result, LValue::Err(Box::new(LValue::Nil)));
        let result = check(&[LValue::True], &env)?;
        assert_eq!(result, LValue::True);
        Ok(())
    }
}
