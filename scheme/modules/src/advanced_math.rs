use rand::Rng;
use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;

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
#[scheme_fn]
pub fn sin(n: LNumber) -> f64 {
    f64::from(&n).sin()
}

/// Compute the cos of a LNumber
/// Only takes one element in args
#[scheme_fn]
pub fn cos(n: LNumber) -> f64 {
    f64::from(&n).cos()
}

#[scheme_fn]
pub fn sqrt(n: LNumber) -> f64 {
    f64::from(&n).sqrt()
}

#[scheme_fn]
pub fn pow(n: LNumber, p: LNumber) -> f64 {
    f64::from(&n).powf(f64::from(&p))
}

#[scheme_fn]
pub fn square(n: LNumber) -> f64 {
    f64::from(&n).powi(2)
}

#[scheme_fn]
pub fn abs(n: LNumber) -> LNumber {
    match n {
        LNumber::Int(i) => LNumber::Int(i.abs()),
        LNumber::Float(f) => LNumber::Float(f.abs()),
    }
}

/// Returns an integer randomly picked between two numbers.
#[scheme_fn]
pub fn rand_int_in_range(low: i64, up: i64) -> i64 {
    let r: i64 = rand::thread_rng().gen_range(low..up);
    r
}

/// Returns a float randomly picked between two numbers.
#[scheme_fn]
pub fn rand_float_in_range(low: f64, up: f64) -> f64 {
    let r: f64 = rand::thread_rng().gen_range(low..up);
    r
}

#[cfg(test)]
mod tests {
    use super::*;
    use sompas_structs::lenv::LEnv;

    #[test]
    fn test_cos() {
        let env = LEnv::default();
        assert_eq!(cos(&env, &[0.0.into()]).unwrap(), 1.0.into());
    }

    #[test]
    fn test_sin() {
        let env = LEnv::default();
        assert_eq!(sin(&env, &[0.0.into()]).unwrap(), 0.0.into());
    }

    #[test]
    fn test_sqrt() {
        let env = LEnv::default();
        assert_eq!(sqrt(&env, &[4.0.into()]).unwrap(), 2.0.into());
    }

    #[test]
    fn test_power() {
        let env = LEnv::default();
        assert_eq!(pow(&env, &[2.0.into(), 4.0.into()]).unwrap(), 16.0.into());
    }

    #[test]
    fn test_square() {
        let env = LEnv::default();
        assert_eq!(square(&env, &[2.0.into()]).unwrap(), 4.0.into());
    }

    #[test]
    fn test_abs() {
        let env = LEnv::default();
        assert_eq!(abs(&env, &[(-1.0).into()]).unwrap(), 1.0.into());
        assert_eq!(abs(&env, &[3.0.into()]).unwrap(), 3.0.into());
    }
}
