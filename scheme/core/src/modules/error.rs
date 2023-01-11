use sompas_language::error::*;
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

#[derive(Default)]
pub struct ModError {}

impl From<ModError> for LModule {
    fn from(m: ModError) -> LModule {
        let mut module = LModule::new(m, MOD_ERROR, DOC_MOD_ERROR);
        //module.add_fn(CHECK, check, DOC_CHECK, true);
        //module.add_fn(FN_ERR, fn_err, DOC_FN_ERR, true);
        module.add_lambda(CHECK, LAMBDA_CHECK, DOC_CHECK);
        module.add_macro(DO_T, MACRO_DO_T, DOC_DO_T);
        module.add_fn(IS_ERR, is_err, DOC_IS_ERR, true);
        module.add_fn(IS_INTERRUPTED, is_interrupted, DOC_IS_INTERRUPTED, true);
        module
    }
}

#[scheme_fn]
pub fn check(b: bool) -> LValue {
    match b {
        true => LValue::Nil,
        false => LValue::Err(Arc::new(LValue::Nil)),
    }
}

/*#[scheme_fn]
pub fn fn_err(e: LValue) -> LValue {
    LValue::Err(Arc::new(e))
}*/

#[scheme_fn]
pub fn is_err(lv: LValue) -> bool {
    matches!(lv, LValue::Err(_))
}
#[scheme_fn]
pub fn is_interrupted(lv: LValue) -> bool {
    if let LValue::Err(r) = lv {
        if let LValue::List(l) = r.as_ref() {
            l[0] == INTERRUPTED.into()
        } else {
            false
        }
    } else {
        false
    }
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
