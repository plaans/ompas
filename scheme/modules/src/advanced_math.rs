use rand::Rng;
use sompas_language::advanced_math::*;
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;
use sompas_structs::lnumber::LNumber;

/// Provides all the functions to make some basic computation.
/// New functions and constants will be added in time, when needed.
/// Here is a list of all the elements provided by this module
/// -Random: "rand-int-in-range", "rand-float-in-range".
/// -Trigonometry: "sin", "cos".
/// -Constants: "pi".
#[derive(Default, Debug)]
pub struct ModMath {}

impl From<ModMath> for LModule {
    fn from(m: ModMath) -> Self {
        let mut module = LModule::new(m, MOD_ADVANCED_MATH, DOC_MOD_ADVANCED_MATH);

        module.add_fn(SIN, sin, DOC_SIN, true);
        module.add_fn(COS, cos, DOC_COS, true);
        module.add_fn(SQRT, sqrt, DOC_SQRT, true);
        module.add_fn(POW, pow, DOC_POW, true);
        module.add_fn(SQUARE, square, DOC_SQUARE, true);
        module.add_fn(ABS, abs, DOC_ABS, true);
        module.add_fn(
            RAND_INT_IN_RANGE,
            rand_int_in_range,
            (DOC_RAND_INT_IN_RANGE, DOC_RAND_INT_IN_RANGE_VERBOSE),
            false,
        );
        module.add_fn(
            RAND_FLOAT_IN_RANGE,
            rand_float_in_range,
            (DOC_RAND_FLOAT_IN_RANGE, DOC_RAND_FLOAT_IN_RANGE_VERBOSE),
            false,
        );

        module.add_value(PI, std::f64::consts::PI.into(), DOC_PI);

        module
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
