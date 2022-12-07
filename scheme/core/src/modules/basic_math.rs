use sompas_language::basic_math::*;
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::ops::Not;

#[derive(Default)]
pub struct ModBasicMath {}

impl From<ModBasicMath> for LModule {
    fn from(t: ModBasicMath) -> LModule {
        let mut module = LModule::new(t, MOD_BASIC_MATH, DOC_MOD_BASIC_MATH);
        module.add_fn(NOT, not, DOC_NOT, true);
        module.add_fn(NOT_SHORT, not, DOC_NOT_SHORT, true);
        module.add_fn(NEQ, neq, DOC_NEQ, true);
        module.add_fn(ADD, add, DOC_ADD, true);
        module.add_fn(SUB, sub, DOC_SUB, true);
        module.add_fn(MUL, mul, DOC_MUL, true);
        module.add_fn(DIV, div, DOC_DIV, true);
        module.add_fn(GT, gt, DOC_GT, true);
        module.add_fn(LT, lt, DOC_LT, true);
        module.add_fn(GEQ, geq, DOC_GEQ, true);
        module.add_fn(LEQ, leq, DOC_LEQ, true);
        module.add_fn(EQ, eq, DOC_EQ, true);
        module
    }
}

/// Logical functional not
/// true => nil
/// nil => true
#[scheme_fn]
pub fn not(b: bool) -> bool {
    b.not()
}

#[scheme_fn]
pub fn neq(a: &LValue, b: &LValue) -> bool {
    a != b
}

#[scheme_fn]
pub fn add(args: Vec<LNumber>) -> LNumber {
    let mut result = LNumber::Float(0.0);
    for value in &args {
        result = &result + value;
    }
    result
}

/// Substract function. Only takes two args.
/// # Example
/// ``` lisp
/// (- 10 2) => 8
#[scheme_fn]
pub fn sub(a: LNumber, b: LNumber) -> LNumber {
    a - b
}

#[scheme_fn]
pub fn mul(args: Vec<LNumber>) -> LNumber {
    let mut result = LNumber::Float(1.0);
    for value in args {
        result = result * value;
    }
    result
}

/// Division function. Only takes two args.
/// # Example
/// ``` lisp
/// (/ 10 2) => 5
#[scheme_fn]
pub fn div(a: LNumber, b: LNumber) -> LNumber {
    a / b
}

/// Compares two values. Returns true if the first arg is greater than the second. Nil Otherwise
#[scheme_fn]
pub fn gt(a: LNumber, b: LNumber) -> bool {
    a > b
}
/// Compares two values. Returns true if the first arg is less than the second. Nil Otherwise
#[scheme_fn]
pub fn lt(a: LNumber, b: LNumber) -> bool {
    a < b
}
/// Compares two values. Returns true if the first arg is greater or equal to the second. Nil Otherwise
#[scheme_fn]
pub fn geq(a: LNumber, b: LNumber) -> bool {
    a >= b
}
/// Compares two values. Returns true if the first arg is less or equal to the second. Nil Otherwise
#[scheme_fn]
pub fn leq(a: LNumber, b: LNumber) -> bool {
    a < b
}

/// Compares two values. Returns true if the first and second args are equal. Nil Otherwise
#[scheme_fn]
pub fn eq(a: &LValue, b: &LValue) -> bool {
    a == b
}
/*
#[cfg(test)]
mod tests {
    use super::*;
    use Vec;

    #[test]
    fn test_add() {
        let env = LEnv::default();
        let result = add(&vec![3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(5.0)), result);
    }

    #[test]
    fn test_sub() {
        let env = LEnv::default();
        let result = sub(&vec![3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Int(1)), result);
    }

    #[test]
    fn test_mul() {
        let env = LEnv::default();
        let result = mul(&vec![3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(6.0)), result);
    }

    #[test]
    fn test_div() {
        let env = LEnv::default();
        let result = div(&vec![3.0.into(), 2.0.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(1.5)), result);
    }

    #[test]
    fn test_gt() {
        let env = LEnv::default();
        let result_true: bool = gt(&vec![3.into(), 2.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = gt(&vec![2.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false_2: bool = gt(&vec![3.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_geq() {
        let env = LEnv::default();
        let result_true: bool = geq(&[3.into(), 2.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = geq(&[2.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true_2: bool = geq(&[3.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }

    #[test]
    fn test_lt() {
        let env = LEnv::default();
        let result_false: bool = lt(&[3.into(), 2.into()], &env).unwrap().try_into().unwrap();
        let result_true: bool = lt(&[2.into(), 3.into()], &env).unwrap().try_into().unwrap();
        let result_false_2: bool = lt(&[3.into(), 3.into()], &env).unwrap().try_into().unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_leq() {
        let env = LEnv::default();
        let result_false: bool = leq(&[3.into(), 2.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true: bool = leq(&[2.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true_2: bool = leq(&[3.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }
}*/
