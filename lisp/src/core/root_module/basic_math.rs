use crate::core::root_module::basic_math::language::*;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::*;
use crate::core::structs::lerror::LResult;
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;

pub mod language {

    //Mathematical functions
    pub const ADD: &str = "+";
    pub const SUB: &str = "-";
    pub const MUL: &str = "*";
    pub const DIV: &str = "/";

    //Comparison
    pub const GT: &str = ">";
    pub const LT: &str = "<";
    pub const GE: &str = ">=";
    pub const LE: &str = "<=";
    pub const EQ: &str = "=";

    pub const DOC_EQ: &str =
        "Takes 2 arguments. Return true if two arguments are equal. False otherwise.";
    pub const DOC_ADD: &str = "Takes 2+ arguments. Return the addition.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_SUB: &str =
        "Takes 2 arguments. Return the substraction of the first by the second.\
return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_MUL: &str = "Takes 2+ arguments. Return the multiplication.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_DIV: &str = "Takes 2 arguments. Return the division of the first by the second.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
    pub const DOC_GT: &str = "Takes 2 arguments. Return *true* if the first is greater than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
    pub const DOC_GE: &str = "Takes 2 arguments. Return *true* if the first is greater or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
    pub const DOC_LT: &str = "Takes 2 arguments. Return *true* if the first is less than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
    pub const DOC_LE: &str = "Takes 2 arguments. Return *true* if the first is less or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";

    pub const NOT: &str = "not";
    pub const NOT_SHORT: &str = "!";
}

/// Logical functional not
/// true => nil
/// nil => true
pub fn not(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(NOT, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Nil => Ok(LValue::True),
        _ => Ok(LValue::Nil),
    }
}

pub fn add(args: &[LValue], _: &LEnv) -> LResult {
    let mut result = LValue::Number(LNumber::Float(0.0));
    for value in args {
        result = (&result + value)?;
    }
    Ok(result)
}

/// Substract function. Only takes two args.
/// # Example
/// ``` lisp
/// (- 10 2) => 8
pub fn sub(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => &args[0] - &args[1],
        i => Err(WrongNumberOfArgument(SUB, args.into(), i, 2..2)),
    }
}

pub fn mul(args: &[LValue], _: &LEnv) -> LResult {
    let mut result = LValue::Number(LNumber::Float(1.0));
    for value in args {
        result = (&result * value)?;
    }
    Ok(result)
}
/// Division function. Only takes two args.
/// # Example
/// ``` lisp
/// (/ 10 2) => 5
pub fn div(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => &args[0] / &args[1],
        i => Err(WrongNumberOfArgument(DIV, args.into(), i, 2..2)),
    }
}

/// Compares two values. Returns true if the first arg is greater than the second. Nil Otherwise
pub fn gt(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] > args[1]).into()),
        i => Err(WrongNumberOfArgument(GT, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is less than the second. Nil Otherwise
pub fn lt(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] < args[1]).into()),
        i => Err(WrongNumberOfArgument(LT, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is greater or equal to the second. Nil Otherwise
pub fn ge(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] >= args[1]).into()),
        i => Err(WrongNumberOfArgument(GE, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is less or equal to the second. Nil Otherwise
pub fn le(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] <= args[1]).into()),
        i => Err(WrongNumberOfArgument(LE, args.into(), i, 2..2)),
    }
}

/// Compares two values. Returns true if the first and second args are equal. Nil Otherwise
pub fn eq(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] == args[1]).into()),
        i => Err(WrongNumberOfArgument(EQ, args.into(), i, 2..2)),
    }
}

#[cfg(test)]
mod tests {
    use crate::core::root_module::basic_math::{add, div, ge, gt, le, lt, mul, sub};
    use crate::core::structs::lenv::LEnv;
    use crate::core::structs::lnumber::LNumber;
    use crate::core::structs::lvalue::LValue;
    use std::convert::TryInto;

    #[test]
    fn test_add() {
        let env = LEnv::default();
        let result = add(&[3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(5.0)), result);
    }

    #[test]
    fn test_sub() {
        let env = LEnv::default();
        let result = sub(&[3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Int(1)), result);
    }

    #[test]
    fn test_mul() {
        let env = LEnv::default();
        let result = mul(&[3.into(), 2.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(6.0)), result);
    }

    #[test]
    fn test_div() {
        let env = LEnv::default();
        let result = div(&[3.0.into(), 2.0.into()], &env).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(1.5)), result);
    }

    #[test]
    fn test_gt() {
        let env = LEnv::default();
        let result_true: bool = gt(&[3.into(), 2.into()], &env).unwrap().try_into().unwrap();
        let result_false: bool = gt(&[2.into(), 3.into()], &env).unwrap().try_into().unwrap();
        let result_false_2: bool = gt(&[3.into(), 3.into()], &env).unwrap().try_into().unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_ge() {
        let env = LEnv::default();
        let result_true: bool = ge(&[3.into(), 2.into()], &env).unwrap().try_into().unwrap();
        let result_false: bool = ge(&[2.into(), 3.into()], &env).unwrap().try_into().unwrap();
        let result_true_2: bool = ge(&[3.into(), 3.into()], &env).unwrap().try_into().unwrap();
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
    fn test_le() {
        let env = LEnv::default();
        let result_false: bool = le(&[3.into(), 2.into()], &env).unwrap().try_into().unwrap();
        let result_true: bool = le(&[2.into(), 3.into()], &env).unwrap().try_into().unwrap();
        let result_true_2: bool = le(&[3.into(), 3.into()], &env).unwrap().try_into().unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }
}
