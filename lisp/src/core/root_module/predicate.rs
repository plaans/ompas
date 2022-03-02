use crate::core::root_module::predicate::language::*;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::*;
use crate::core::structs::lerror::LResult;
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::typelvalue::TypeLValue;

pub mod language {

    pub const IS_NUMBER: &str = "number?";
    pub const IS_FLOAT: &str = "float?";
    pub const IS_INT: &str = "int?";
    pub const IS_BOOL: &str = "bool?";
    pub const IS_SYMBOL: &str = "symbol?";
    pub const IS_STRING: &str = "string?";
    pub const IS_FN: &str = "fn?";
    pub const IS_MUT_FN: &str = "mut-fn?";
    pub const IS_LIST: &str = "list?";
    pub const IS_MAP: &str = "map?";
    pub const IS_LAMBDA: &str = "lambda?";
    pub const IS_QUOTE: &str = "quote?";
    pub const IS_PAIR: &str = "pair?";
    pub const IS_EQUAL: &str = "equal?";

    pub const IS_NIL: &str = "null?";

    pub const DOC_IS_NIL: &str = "Return true if symbol is LValue::Nil or empty list.";
    pub const DOC_IS_NUMBER: &str = "Return true if symbol is LValue::Number";
    pub const DOC_IS_BOOL: &str = "Return true if symbol is LValue::Bool";
    pub const DOC_IS_SYMBOL: &str = "Return true if symbol is LValue::Symbol";
    pub const DOC_IS_MAP: &str = "Return true if symbol is map";
    pub const DOC_IS_LIST: &str = "Return true if symbol is list";
    pub const DOC_IS_LAMBDA: &str = "Return true if symbol is lambda";
    pub const DOC_IS_QUOTE: &str = "Return true if symbol is quote";
    pub const DOC_IS_FN: &str = "Return true if symbol is LValue::Fn";
    pub const DOC_IS_MUT_FN: &str = "Return true if symbol is LValue::MutFn";

    pub const DOC_IS_PAIR: &str = "Return true if the list is not empty, false otherwise.";
    pub const DOC_IS_EQUAL: &str = "Return true if the 2 LValues are equal, false otherwise.";
}

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
