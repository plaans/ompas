use sompas_language::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LError::WrongNumberOfArgument;
use sompas_structs::lerror::LResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;

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

pub fn neq(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] != args[1]).into()),
        i => Err(WrongNumberOfArgument(EQ, args.into(), i, 2..2)),
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
pub fn geq(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] >= args[1]).into()),
        i => Err(WrongNumberOfArgument(GEQ, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is less or equal to the second. Nil Otherwise
pub fn leq(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => Ok((args[0] <= args[1]).into()),
        i => Err(WrongNumberOfArgument(LEQ, args.into(), i, 2..2)),
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

    use super::*;

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
