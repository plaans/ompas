use crate::core::LEnv;
use crate::modules::doc::{Documentation, LHelp};
use crate::structs::LError::{WrongNumberOfArgument, WrongType};
use crate::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use std::sync::Arc;

//LANGUAGE
const OK: &str = "ok";
const ERR: &str = "err";
const IS_ERR: &str = "err?";
const IS_OK: &str = "ok?";

const DOC_OK: &str = "Return an LValue (ok <expr>)";
const DOC_ERR: &str = "Return an LValue (err <expr>)";
const DOC_IS_OK: &str =
    "Return true if the argument is an lvalue of the form (ok <expr>), false otherwise.";
const DOC_IS_ERR: &str =
    "Return true if the argument is an lvalue of the form (err <expr>), false otherwise.";

const MOD_ERROR: &str = "error";
const DOC_MOD_ERROR: &str =
    "Module that contain functions to define enums ok(lvalue) and err(lvalue)";
const DOC_MOD_ERROR_VERBOSE: &str = "functions:\n\
                                \t-ok\n
                                \t-err\n\
                                \t-ok?\n\
                                \t-err?\n";

#[derive(Default)]
pub struct CtxError {}

impl GetModule for CtxError {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_ERROR.to_string(),
        };

        module.add_fn_prelude(OK, ok);
        module.add_fn_prelude(ERR, err);
        module.add_fn_prelude(IS_ERR, is_err);
        module.add_fn_prelude(IS_OK, is_ok);

        module
    }
}

impl Documentation for CtxError {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(OK, DOC_OK),
            LHelp::new(ERR, DOC_ERR),
            LHelp::new(IS_OK, DOC_IS_OK),
            LHelp::new(IS_ERR, DOC_IS_ERR),
            LHelp::new_verbose(MOD_ERROR, DOC_MOD_ERROR, DOC_MOD_ERROR_VERBOSE),
        ]
    }
}

pub fn ok(args: &[LValue], _: &LEnv, _: &CtxError) -> Result<LValue, LError> {
    Ok(vec![LValue::from(OK), args.into()].into())
}

pub fn err(args: &[LValue], _: &LEnv, _: &CtxError) -> Result<LValue, LError> {
    Ok(vec![LValue::from(ERR), args.into()].into())
}

pub fn is_err(args: &[LValue], _: &LEnv, _: &CtxError) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(IS_ERR, args.into(), args.len(), 1..1));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(s) = &list[0] {
            match s.as_str() {
                OK => Ok(false.into()),
                ERR => Ok(true.into()),
                _ => Err(WrongType(
                    IS_ERR,
                    list[0].clone(),
                    (&list[0]).into(),
                    NameTypeLValue::Other("{ok,err}".to_string()),
                )),
            }
        } else {
            Err(WrongType(
                IS_ERR,
                list[0].clone(),
                (&list[0]).into(),
                NameTypeLValue::Other("{ok,err}".to_string()),
            ))
        }
    } else {
        Err(WrongType(
            IS_ERR,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}

pub fn is_ok(args: &[LValue], _: &LEnv, _: &CtxError) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(IS_ERR, args.into(), args.len(), 1..1));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(s) = &list[0] {
            match s.as_str() {
                OK => Ok(true.into()),
                ERR => Ok(false.into()),
                _ => Err(WrongType(
                    IS_ERR,
                    list[0].clone(),
                    (&list[0]).into(),
                    NameTypeLValue::Other("{ok,err}".to_string()),
                )),
            }
        } else {
            Err(WrongType(
                IS_ERR,
                list[0].clone(),
                (&list[0]).into(),
                NameTypeLValue::Other("{ok,err}".to_string()),
            ))
        }
    } else {
        Err(WrongType(
            IS_ERR,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}
