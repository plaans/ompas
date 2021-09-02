use crate::core::LEnv;
use crate::modules::doc::{Documentation, LHelp};
use crate::structs::LError::{WrongNumberOfArgument, WrongType};
use crate::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use rand::Rng;
use std::sync::Arc;

//LANGUAGE
const MOD_MATH: &str = "mod-math";
const DOC_MOD_MATH: &str =
    "Module handling mathematical functions for basic arithmetic operations and comparisons.";
const DOC_MOD_MATH_VERBOSE: &str = "functions:\n\
                                    - random: rand-int-in-range, rand-float-in-range\n\
                                    - trigonometry: cos, sin\n\
                                    - constants: pi";

//Trigonometry
const SIN: &str = "sin";
const COS: &str = "cos";
const RAND_INT_IN_RANGE: &str = "rand-int-in-range";
const RAND_FLOAT_IN_RANGE: &str = "rand-float-in-range";

//DOCUMENTATION
const DOC_COS: &str = "Takes 1 argument. Return the cosinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_SIN: &str = "Takes 1 argument. Return the sinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_RAND_INT_IN_RANGE: &str = "Returns a random int between two numbers";
const DOC_RAND_FLOAT_IN_RANGE: &str = "Returns a random float between two numbers";
const DOC_RAND_INT_IN_RANGE_VERBOSE: &str = "Example:\n(rand-int-in-range 1 10)\n=> 2";
const DOC_RAND_FLOAT_IN_RANGE_VERBOSE: &str =
    "Example:\n(rand-float-in-range 1 10)\n=> 2.32511455...";

//Constants
const PI: &str = "pi";

#[derive(Default, Debug)]
pub struct CtxMath {}

impl GetModule for CtxMath {
    /// Provides all the functions to make some basic computation.
    /// New functions and constants will be added in time, when needed.
    /// Here is a list of all the elements provided by this module
    /// -Random: "rand-int-in-range", "rand-float-in-range".
    /// -Trigonometry: "sin", "cos".
    /// -Constants: "pi".
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_MATH,
        };

        module.add_fn_prelude(COS, cos);
        module.add_fn_prelude(SIN, sin);
        module.add_fn_prelude(RAND_INT_IN_RANGE, rand_int_in_range);
        module.add_fn_prelude(RAND_FLOAT_IN_RANGE, rand_float_in_range);

        module.add_prelude(PI, std::f64::consts::PI.into());

        module
    }
}

impl Documentation for CtxMath {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(MOD_MATH, DOC_MOD_MATH, DOC_MOD_MATH_VERBOSE),
            LHelp::new(SIN, DOC_SIN),
            LHelp::new(COS, DOC_COS),
            LHelp::new_verbose(
                RAND_INT_IN_RANGE,
                DOC_RAND_INT_IN_RANGE,
                DOC_RAND_INT_IN_RANGE_VERBOSE,
            ),
            LHelp::new_verbose(
                RAND_FLOAT_IN_RANGE,
                DOC_RAND_FLOAT_IN_RANGE,
                DOC_RAND_FLOAT_IN_RANGE_VERBOSE,
            ),
        ]
    }
}

///Compute the sin of a LNumber
/// Only takes one element in args
pub fn sin(args: &[LValue], _: &LEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(SIN, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.sin().into())
        }
        lv => Err(WrongType(
            SIN,
            lv.clone(),
            lv.into(),
            NameTypeLValue::Number,
        )),
    }
}

/// Compute the cos of a LNumber
/// Only takes one element in args
pub fn cos(args: &[LValue], _: &LEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(COS, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.cos().into())
        }
        lv => Err(WrongType(
            COS,
            lv.clone(),
            lv.into(),
            NameTypeLValue::Number,
        )),
    }
}

/// Returns an integer randomly picked between two numbers.
pub fn rand_int_in_range(args: &[LValue], _: &LEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAND_INT_IN_RANGE,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    if let LValue::Number(n1) = &args[0] {
        if let LValue::Number(n2) = &args[1] {
            let value: i64 = rand::thread_rng().gen_range(n1.into()..n2.into());
            Ok(value.into())
        } else {
            Err(WrongType(
                RAND_INT_IN_RANGE,
                args[1].clone(),
                (&args[1]).into(),
                NameTypeLValue::Number,
            ))
        }
    } else {
        Err(WrongType(
            RAND_INT_IN_RANGE,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::Number,
        ))
    }
}

/// Returns a float randomly picked between two numbers.
pub fn rand_float_in_range(args: &[LValue], _: &LEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAND_FLOAT_IN_RANGE,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    if let LValue::Number(n1) = &args[0] {
        if let LValue::Number(n2) = &args[1] {
            let value: f64 = rand::thread_rng().gen_range(n1.into()..n2.into());
            Ok(value.into())
        } else {
            Err(WrongType(
                RAND_FLOAT_IN_RANGE,
                args[1].clone(),
                (&args[1]).into(),
                NameTypeLValue::Number,
            ))
        }
    } else {
        Err(WrongType(
            RAND_FLOAT_IN_RANGE,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::Number,
        ))
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
