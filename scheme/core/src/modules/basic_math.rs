use im::Vector;
use macro_rules_attribute::macro_rules_attribute;
use sompas_language::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lfn_extended;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::ops::Not;

/// Logical functional not
/// true => nil
/// nil => true
#[macro_rules_attribute(lfn_extended!)]
pub fn not(b: bool) -> bool {
    b.not()
}

#[macro_rules_attribute(lfn_extended!)]
pub fn neq(a: LValue, b: LValue) -> bool {
    a != b
}

#[macro_rules_attribute(lfn_extended!)]
pub fn add(args: Vector<LNumber>) -> LNumber {
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
#[macro_rules_attribute(lfn_extended!)]
pub fn sub(a: LNumber, b: LNumber) -> LNumber {
    a - b
}

#[macro_rules_attribute(lfn_extended!)]
pub fn mul(args: &Vector<LNumber>) -> LNumber {
    let mut result = LNumber::Float(1.0);
    for value in args {
        result = &result * value;
    }
    result
}
/// Division function. Only takes two args.
/// # Example
/// ``` lisp
/// (/ 10 2) => 5
#[macro_rules_attribute(lfn_extended!)]
pub fn div(a: LNumber, b: LNumber) -> LNumber {
    a / b
}

/// Compares two values. Returns true if the first arg is greater than the second. Nil Otherwise
#[macro_rules_attribute(lfn_extended!)]
pub fn gt(a: LNumber, b: LNumber) -> bool {
    a > b
}
/// Compares two values. Returns true if the first arg is less than the second. Nil Otherwise
#[macro_rules_attribute(lfn_extended!)]
pub fn lt(a: LNumber, b: LNumber) -> bool {
    a < b
}
/// Compares two values. Returns true if the first arg is greater or equal to the second. Nil Otherwise
#[macro_rules_attribute(lfn_extended!)]
pub fn geq(a: LNumber, b: LNumber) -> bool {
    a >= b
}
/// Compares two values. Returns true if the first arg is less or equal to the second. Nil Otherwise
#[macro_rules_attribute(lfn_extended!)]
pub fn leq(a: LNumber, b: LNumber) -> bool {
    a < b
}

/// Compares two values. Returns true if the first and second args are equal. Nil Otherwise
#[macro_rules_attribute(lfn_extended!)]
pub fn eq(a: LNumber, b: LNumber) -> bool {
    a == b
}

#[cfg(test)]
mod tests {
    use super::*;
    use im::vector;

    #[test]
    fn test_add() {
        let env = LEnv::default();
        let result = add(&vector![3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(5.0)), result);
    }

    #[test]
    fn test_sub() {
        let env = LEnv::default();
        let result = sub(&vector![3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Int(1)), result);
    }

    #[test]
    fn test_mul() {
        let env = LEnv::default();
        let result = mul(&vector![3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(6.0)), result);
    }

    #[test]
    fn test_div() {
        let env = LEnv::default();
        let result = div(&vector![3.0.into(), 2.0.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(1.5)), result);
    }

    #[test]
    fn test_gt() {
        let env = LEnv::default();
        let result_true: bool = gt(&vector![3.into(), 2.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = gt(&vector![2.into(), 3.into()], &env)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false_2: bool = gt(&vector![3.into(), 3.into()], &env)
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
}
