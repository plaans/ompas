use crate::doc::{Documentation, LHelp};
use ompas_lisp::core::*;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;

/*
LANGUAGE LITERALS
 */
const MOD_MATH: &str = "mod-math";
const DOC_MOD_MATH: &str =
    "Module handling mathematical functions for basic arithmetic operations and comparisons.";
const DOC_MOD_MATH_VERBOSE: &str = "functions:\n\
                                    -arithmetic: '+', '-', '*','/'\n\
                                    -comparison: '>', '>=', '<', '<='\n\
                                    -trigonometry: cos, sin\n\
                                    -constants: pi";

//Trigonometry
const SIN: &str = "sin";
const COS: &str = "cos";

//Constants
const PI: &str = "pi";

#[derive(Default, Debug)]
pub struct CtxMath {}

impl GetModule for CtxMath {
    /// Provides all the functions to make some basic computation.
    /// New functions and constants will be added in time, when needed.
    /// Here is a list of all the elements provided by this module
    /// -Basic functions: "+", "-", "*", "/".
    /// -Comparisons: "<", ">", "<=", ">=".
    /// -Trigonometry: "sin", "cos".
    /// -Constants: "pi".
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_MATH,
        };

        module.add_fn_prelude(COS, Box::new(cos));
        module.add_fn_prelude(SIN, Box::new(sin));
        module.add_prelude(PI, std::f64::consts::PI.into());

        module
    }
}

/*
HELP
 */

const DOC_COS: &str = "Takes 1 argument. Return the cosinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_SIN: &str = "Takes 1 argument. Return the sinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";

impl Documentation for CtxMath {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_MATH, DOC_MOD_MATH, Some(DOC_MOD_MATH_VERBOSE)),
            LHelp::new(SIN, DOC_SIN, None),
            LHelp::new(COS, DOC_COS, None),
        ]
    }
}

///Compute the sin of a LNumber
/// Only takes one element in args
pub fn sin(args: &[LValue], _: &LEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.sin().into())
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Number)),
    }
}

/// Compute the cos of a LNumber
/// Only takes one element in args
pub fn cos(args: &[LValue], _: &LEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.cos().into())
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Number)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /*fn generate_env() -> (LEnv, ContextCollection) {
        let env = LEnv::empty();
        let ctxs = ContextCollection::default();
        load_module(&mut env, &mut ctxs, CtxMath::get_module());
        (env, ctxs)
    }*/

    #[test]
    fn test_cos() {
        let env = LEnv::default();
        let ctx = CtxMath::default();
        assert_eq!(cos(&[0.0.into()], &env, &ctx).unwrap(), 1.0.into())
    }

    #[test]
    fn test_sin() {
        let env = LEnv::default();
        let ctx = CtxMath::default();
        assert_eq!(sin(&[0.0.into()], &env, &ctx).unwrap(), 0.0.into())
    }
}
