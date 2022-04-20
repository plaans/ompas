use rand::Rng;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LRuntimeError::{WrongNumberOfArgument, WrongType};
use sompas_structs::lerror::LResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::typelvalue::KindLValue;

//LANGUAGE
const MOD_MATH: &str = "math";
const DOC_MOD_MATH: &str =
    "Module handling mathematical functions for basic arithmetic operations and comparisons.";
const DOC_MOD_MATH_VERBOSE: &str = "functions:\n\
                                    - random: rand-int-in-range, rand-float-in-range\n\
                                    - trigonometry: cos, sin\n\
                                    - constants: pi";

//Trigonometry
const SIN: &str = "sin";
const COS: &str = "cos";
const SQRT: &str = "sqrt";
const POW: &str = "pow";
const SQUARE: &str = "square";
const ABS: &str = "abs";
const RAND_INT_IN_RANGE: &str = "rand-int-in-range";
const RAND_FLOAT_IN_RANGE: &str = "rand-float-in-range";

//DOCUMENTATION
const DOC_COS: &str = "Takes 1 argument. Return the cosinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_SIN: &str = "Takes 1 argument. Return the sinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_SQRT: &str = "Takes 1 argument. Return the square root of the number.";
const DOC_POW: &str =
    "Takes 2 arguments. Return the first argument to the power of the second argument.";
const DOC_ABS: &str = "Compute the absolute value of a number.";
const DOC_SQUARE: &str = "Takes 1 argument. Return the first argument to the power of 2.";
const DOC_RAND_INT_IN_RANGE: &str = "Returns a random int between two numbers";
const DOC_RAND_FLOAT_IN_RANGE: &str = "Returns a random float between two numbers";
const DOC_RAND_INT_IN_RANGE_VERBOSE: &str = "Example:\n(rand-int-in-range 1 10)\n=> 2";
const DOC_RAND_FLOAT_IN_RANGE_VERBOSE: &str =
    "Example:\n(rand-float-in-range 1 10)\n=> 2.32511455...";

//Constants
const PI: &str = "pi";

#[derive(Default, Debug)]
pub struct CtxMath {}

impl IntoModule for CtxMath {
    /// Provides all the functions to make some basic computation.
    /// New functions and constants will be added in time, when needed.
    /// Here is a list of all the elements provided by this module
    /// -Random: "rand-int-in-range", "rand-float-in-range".
    /// -Trigonometry: "sin", "cos".
    /// -Constants: "pi".
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_MATH.into(),
        };

        module.add_fn_prelude(COS, cos);
        module.add_fn_prelude(SIN, sin);
        module.add_fn_prelude(POW, pow);
        module.add_fn_prelude(SQUARE, square);
        module.add_fn_prelude(ABS, abs);
        module.add_fn_prelude(RAND_INT_IN_RANGE, rand_int_in_range);
        module.add_fn_prelude(RAND_FLOAT_IN_RANGE, rand_float_in_range);

        module.add_prelude(PI, std::f32::consts::PI.into());

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new_verbose(MOD_MATH, DOC_MOD_MATH, DOC_MOD_MATH_VERBOSE),
            LHelp::new(SIN, DOC_SIN),
            LHelp::new(COS, DOC_COS),
            LHelp::new(SQRT, DOC_SQRT),
            LHelp::new(POW, DOC_POW),
            LHelp::new(SQUARE, DOC_SQUARE),
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
            LHelp::new(ABS, DOC_ABS),
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        vec![SIN, COS, SQRT, POW, SQUARE, ABS].into()
    }
}

///Compute the sin of a LNumber
/// Only takes one element in args
lfn!{pub sin(args, _){
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(SIN, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.sin().into())
        }
        lv => Err(WrongType(SIN, lv.clone(), lv.into(), TypeLValue::Number)),
    }
}

/// Compute the cos of a LNumber
/// Only takes one element in args
lfn!{pub cos(args, _){
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(COS, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.cos().into())
        }
        lv => Err(WrongType(COS, lv.clone(), lv.into(), TypeLValue::Number)),
    }
}

lfn!{pub sqrt(args, _){
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(SQRT, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.sqrt().into())
        }
        lv => Err(WrongType(SQRT, lv.clone(), lv.into(), TypeLValue::Number)),
    }
}

lfn!{pub pow(args, _){
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(POW, args.into(), args.len(), 2..2));
    }

    if let LValue::Number(n) = &args[0] {
        if let LValue::Number(p) = &args[1] {
            let f: f64 = n.into();
            let p: f64 = p.into();
            Ok(f.powf(p).into())
        } else {
            Err(WrongType(
                POW,
                args[1].clone(),
                (&args[1]).into(),
                TypeLValue::Number,
            ))
        }
    } else {
        Err(WrongType(
            POW,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Number,
        ))
    }
}

lfn!{pub square(args, _){
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(SQUARE, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.powi(2).into())
        }
        lv => Err(WrongType(SQUARE, lv.clone(), lv.into(), TypeLValue::Number)),
    }
}

lfn!{pub abs(args, _){
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(SQUARE, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => Ok(match n {
            LNumber::Int(i) => LNumber::Int(i.abs()),
            LNumber::Float(f) => LNumber::Float(f.abs()),
        }
        .into()),
        lv => Err(WrongType(SQUARE, lv.clone(), lv.into(), TypeLValue::Number)),
    }
}

/// Returns an integer randomly picked between two numbers.
lfn!{pub rand_int_in_range(args, _){
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
                TypeLValue::Number,
            ))
        }
    } else {
        Err(WrongType(
            RAND_INT_IN_RANGE,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Number,
        ))
    }
}

/// Returns a float randomly picked between two numbers.
lfn!{pub rand_float_in_range(args, _){
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
                TypeLValue::Number,
            ))
        }
    } else {
        Err(WrongType(
            RAND_FLOAT_IN_RANGE,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Number,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cos() {
        let env = LEnv::default();
        assert_eq!(cos(&[0.0.into()], &env).unwrap(), 1.0.into());
    }

    #[test]
    fn test_sin() {
        let env = LEnv::default();
        assert_eq!(sin(&[0.0.into()], &env).unwrap(), 0.0.into());
    }

    #[test]
    fn test_sqrt() {
        let env = LEnv::default();
        assert_eq!(sqrt(&[4.0.into()], &env).unwrap(), 2.0.into());
    }

    #[test]
    fn test_power() {
        let env = LEnv::default();
        assert_eq!(pow(&[2.0.into(), 4.0.into()], &env).unwrap(), 16.0.into());
    }

    #[test]
    fn test_square() {
        let env = LEnv::default();
        assert_eq!(square(&[2.0.into()], &env).unwrap(), 4.0.into());
    }

    #[test]
    fn test_abs() {
        let env = LEnv::default();
        assert_eq!(abs(&[(-1.0).into()], &env).unwrap(), 1.0.into());
        assert_eq!(abs(&[3.0.into()], &env).unwrap(), 3.0.into());
    }
}
