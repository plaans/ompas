use crate::rae::module::mod_rae_exec::LAMBDA_GET_METHOD_GENERATOR;
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
//const IS_LOCKED: &str = "locked?";
const LAMBDA_IS_LOCKED: &str = "(define locked?
    (lambda (r)
        (get-map state (list 'locked r))))";

const MACRO_ASSERT: &str = "(defmacro assert
    (lambda args
        `(set! state (set-map state (quote ,args)))))";

const MACRO_RETRACT: &str = "(defmacro retract
    (lambda args
        `(set! state (remove-key-value-map state (quote ,args)))))";

const LAMBDA_GET_PRECONDITIONS: &str = "(define get-preconditions\
    (lambda (label)\
        (get rae-method-pre-conditions-map label)))";

const LAMBDA_GET_SCORE: &str = "(define get-score\
    (lambda (label)\
        (get rae-method-score-map label)))";

const LAMBDA_EVAL_PRE_CONDITIONS: &str = "(define eval-pre-conditions
(lambda args
    (eval (cons (get-preconditions (car args)) (quote-list (cdr args))))))";

const LAMBDA_COMPUTE_SCORE: &str = "(define compute-score 
    (lambda args
        (eval (cons (get-score (car args)) (quote-list (cdr args))))))";

pub const EVAL_PRE_CONDITIONS: &str = "eval-pre-conditions";
pub const COMPUTE_SCORE: &str = "compute-score";

#[derive(Default)]
pub struct CtxRaeSim {}

impl GetModule for CtxRaeSim {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(()),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_ASSERT,
                MACRO_RETRACT,
                LAMBDA_GET_PRECONDITIONS,
                LAMBDA_GET_SCORE,
                LAMBDA_EVAL_PRE_CONDITIONS,
                LAMBDA_COMPUTE_SCORE,
                LAMBDA_IS_LOCKED,
            ]
            .into(),
            label: MOD_RAE_SIM.to_string(),
        };

        module.add_fn_prelude(SUCCESS, success);
        module.add_fn_prelude(FAILURE, failure);
        module.add_fn_prelude(IS_SUCCESS, is_success);
        module.add_fn_prelude(IS_FAILURE, is_failure);

        module
    }
}

pub fn success(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(vec![LValue::from(SUCCESS), args.into()].into())
}

pub fn failure(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(vec![LValue::from(FAILURE), args.into()].into())
}

pub fn is_failure(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn is_success(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
