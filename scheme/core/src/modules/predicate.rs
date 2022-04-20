use sompas_language::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lerror::LRuntimeError::*;
use sompas_structs::lfn;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::KindLValue;

/// Returns true if LValue is Nil
lfn! {pub is_nil(args, _){
    match args.len() {
        1 => Ok((KindLValue::from(&args[0]) == KindLValue::Nil).into()),
        i => Err(WrongNumberOfArgument(IS_NIL, args.into(), i, 1..1)),
    }
}}

/// Returns true is LValue is number
lfn! {pub is_number(args, _){
    match args.len() {
        1 => Ok((KindLValue::from(&args[0]) == KindLValue::Number).into()),
        i => Err(WrongNumberOfArgument(IS_NUMBER, args.into(), i, 1..1)),
    }
}}

/// Returns true if LValue is integer
lfn! {pub is_integer(args, _){
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
}}

/// Returns true if LValue is float
lfn! {pub is_float(args, _){
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
}}
/// Returns true if LValue is boolean
lfn! {pub is_bool(args, _){
    match args.len() {
        1 => Ok((KindLValue::from(args.get(0).unwrap()) == KindLValue::Bool).into()),
        i => Err(WrongNumberOfArgument(IS_BOOL, args.into(), i, 1..1)),
    }
}}
/// Returns true if LValue is a function
lfn! {pub is_fn(args, _){
    match args.len() {
        1 => Ok((KindLValue::from(args.get(0).unwrap()) == KindLValue::Fn).into()),
        i => Err(WrongNumberOfArgument(IS_FN, args.into(), i, 1..1)),
    }
}}

/// Returns true if LValue is a symbol
lfn! {pub is_symbol(args, _){
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_SYMBOL, args.into(), i, 1..1)),
    }
}}

/// Returns true if LValue is a string
lfn! {pub is_string(args, _){
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::String(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_SYMBOL, args.into(), i, 1..1)),
    }
}}
/// Returns true if LValue is a list
lfn! {pub is_list(args, _){
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::List(_) | LValue::Nil => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_LIST, args.into(), i, 1..1)),
    }
}}

/// Returns true if LValue is a lambda
lfn! {pub is_lambda(args, _){
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_LAMBDA, args.into(), i, 1..1)),
    }
}}

/// Returns true if LValue is a hashmap
lfn! {pub is_map(args, _){
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Map(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_MAP, args.into(), i, 1..1)),
    }
}}

/// Returns true if two LValues are equals.
/// The difference with eq is that it compares all kind of LValue.
lfn! {pub is_equal(args, _){
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
                KindLValue::List,
            ))
        }
    } else {
        Err(WrongType(
            IS_EQUAL,
            args[0].clone(),
            (&args[0]).into(),
            KindLValue::List,
        ))
    }
}}

/// Returns true if a list is not empty
lfn! {pub is_pair(args, _){
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
            KindLValue::List,
        ))
    }
}}
