use sompas_language::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LError::*;
use sompas_structs::lerror::LResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::TypeLValue;

/// Returns true if LValue is Nil
pub fn is_nil(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => Ok((TypeLValue::from(&args[0]) == TypeLValue::Nil).into()),
        i => Err(WrongNumberOfArgument(IS_NIL, args.into(), i, 1..1)),
    }
}

/// Returns true is LValue is number
pub fn is_number(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => Ok((TypeLValue::from(&args[0]) == TypeLValue::Number).into()),
        i => Err(WrongNumberOfArgument(IS_NUMBER, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is integer
pub fn is_integer(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => {
            if let LValue::Number(LNumber::Int(_)) = &args[0] {
                Ok(true.into())
            } else {
                Ok(false.into())
            }
        }
        i => Err(WrongNumberOfArgument(IS_INT, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is float
pub fn is_float(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => {
            if let LValue::Number(LNumber::Float(_)) = &args[0] {
                Ok(true.into())
            } else {
                Ok(false.into())
            }
        }
        i => Err(WrongNumberOfArgument(IS_FLOAT, args.into(), i, 1..1)),
    }
}
/// Returns true if LValue is boolean
pub fn is_bool(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => Ok((TypeLValue::from(args.get(0).unwrap()) == TypeLValue::Bool).into()),
        i => Err(WrongNumberOfArgument(IS_BOOL, args.into(), i, 1..1)),
    }
}
/// Returns true if LValue is a function
pub fn is_fn(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => Ok((TypeLValue::from(args.get(0).unwrap()) == TypeLValue::Fn).into()),
        i => Err(WrongNumberOfArgument(IS_FN, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a symbol
pub fn is_symbol(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_SYMBOL, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a string
pub fn is_string(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::String(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_SYMBOL, args.into(), i, 1..1)),
    }
}
/// Returns true if LValue is a list
pub fn is_list(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::List(_) | LValue::Nil => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_LIST, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a lambda
pub fn is_lambda(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_LAMBDA, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a hashmap
pub fn is_map(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Map(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_MAP, args.into(), i, 1..1)),
    }
}

/// Returns true if two LValues are equals.
/// The difference with eq is that it compares all kind of LValue.
pub fn is_equal(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            IS_EQUAL,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    if let LValue::List(l1) = &args[0] {
        if let LValue::List(l2) = &args[1] {
            Ok((l1 == l2).into())
        } else {
            Err(WrongType(
                IS_EQUAL,
                args[1].clone(),
                (&args[1]).into(),
                TypeLValue::List,
            ))
        }
    } else {
        Err(WrongType(
            IS_EQUAL,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}

/// Returns true if a list is not empty
pub fn is_pair(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            IS_PAIR,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(l) = &args[0] {
        Ok((!l.is_empty()).into())
    } else {
        Err(WrongType(
            IS_PAIR,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}
