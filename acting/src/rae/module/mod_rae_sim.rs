use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use std::sync::Arc;

//Language
const MOD_RAE_SIM: &str = "rae-sim";
const SUCCESS: &str = "success";
const FAILURE: &str = "failure";
const IS_SUCCESS: &str = "success?";
const IS_FAILURE: &str = "failure?";

pub const MACRO_ASSERT: &str = "(defmacro assert
    (lambda args
        `(set! state (set-map state (quote ,args)))))";

pub const MACRO_RETRACT: &str = "(defmacro retract
    (lambda args
        `(set! state (remove-key-value-map state (quote ,args)))))";

pub struct CtxRaeSim {}

impl GetModule for CtxRaeSim {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![MACRO_ASSERT, MACRO_RETRACT].into(),
            label: MOD_RAE_SIM.to_string(),
        };

        module.add_fn_prelude(SUCCESS, success);
        module.add_fn_prelude(FAILURE, failure);
        module.add_fn_prelude(IS_SUCCESS, is_success);
        module.add_fn_prelude(IS_FAILURE, is_failure);

        module
    }
}

pub fn success(args: &[LValue], _: &LEnv, _: &CtxRaeSim) -> Result<LValue, LError> {
    Ok(vec![LValue::from(SUCCESS), args.into()].into())
}

pub fn failure(args: &[LValue], _: &LEnv, _: &CtxRaeSim) -> Result<LValue, LError> {
    Ok(vec![LValue::from(FAILURE), args.into()].into())
}

pub fn is_failure(args: &[LValue], _: &LEnv, _: &CtxRaeSim) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            IS_FAILURE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(s) = &list[0] {
            match s.as_str() {
                SUCCESS => Ok(false.into()),
                FAILURE => Ok(true.into()),
                _ => Err(WrongType(
                    IS_FAILURE,
                    list[0].clone(),
                    (&list[0]).into(),
                    NameTypeLValue::Other("{success,failure}".to_string()),
                )),
            }
        } else {
            Err(WrongType(
                IS_FAILURE,
                list[0].clone(),
                (&list[0]).into(),
                NameTypeLValue::Other("{success,failure}".to_string()),
            ))
        }
    } else {
        Err(WrongType(
            IS_FAILURE,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}

pub fn is_success(args: &[LValue], _: &LEnv, _: &CtxRaeSim) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            IS_SUCCESS,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(s) = &list[0] {
            match s.as_str() {
                SUCCESS => Ok(true.into()),
                FAILURE => Ok(false.into()),
                _ => Err(WrongType(
                    IS_SUCCESS,
                    list[0].clone(),
                    (&list[0]).into(),
                    NameTypeLValue::Other("{success,failure}".to_string()),
                )),
            }
        } else {
            Err(WrongType(
                IS_SUCCESS,
                list[0].clone(),
                (&list[0]).into(),
                NameTypeLValue::Other("{success,failure}".to_string()),
            ))
        }
    } else {
        Err(WrongType(
            IS_SUCCESS,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}
