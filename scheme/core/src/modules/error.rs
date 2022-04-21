use sompas_macros::scheme_fn;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

#[scheme_fn]
pub fn check(b: bool) -> LValue {
    match b {
        true => LValue::True,
        false => LValue::Err(Arc::new(LValue::Nil)),
    }
}

#[scheme_fn]
pub fn err(e: LValue) -> LValue {
    LValue::Err(Arc::new(e))
}

/*lfn! {pub err(args, _){
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(ERR, args.into(), args.len(), 1..1));
    }
    Ok(LValue::Err(Arc::new(args[0].clone())))
}}*/
#[scheme_fn]
pub fn is_err(lv: LValue) -> bool {
    matches!(lv, LValue::Err(_))
}
/*
#[cfg(test)]
mod tests {
    use super::*;
    use sompas_structs::lerror;

    #[test]
    pub fn test_err() -> lerror::Result<()> {
        let env = LEnv::default();
        let lv = 5.into();
        let result = err(&env, &[lv].into())?;

        assert_eq!(LValue::Err(Arc::new(5.into())), result);
        Ok(())
    }

    #[test]
    pub fn test_is_err() -> lerror::Result<()> {
        let env = LEnv::default();
        let result = is_err(&[LValue::Err(Arc::new(5.into()))], &env)?;
        assert_eq!(result, LValue::True);
        let result = is_err(&[5.into()], &env)?;
        assert_eq!(result, LValue::Nil);
        Ok(())
    }

    #[test]
    pub fn test_check() -> lerror::Result<()> {
        let env = LEnv::default();
        let result = check(&[LValue::Nil], &env)?;
        assert_eq!(result, LValue::Err(Arc::new(LValue::Nil)));
        let result = check(&[LValue::True], &env)?;
        assert_eq!(result, LValue::True);
        Ok(())
    }
}
*/
