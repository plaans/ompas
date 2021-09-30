use crate::core::LEnv;
use crate::language::scheme_primitives::*;
use crate::structs::LError::{
    NotInListOfExpectedTypes, SpecialError, WrongNumberOfArgument, WrongType,
};
use crate::structs::{LError, LNumber, LValue, NameTypeLValue};
use im::HashMap;

/// Default function of the Lisp Environement.
/// Does nothing outside returning a string.
pub fn default(_args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(LValue::String("default function".to_string()))
}

/// Returns a list of all the keys present in the environment
pub fn env_get_keys(_: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(env
        .keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn env_get_macros(_: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(env
        .macros()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn env_get_macro(args: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            ENV_GET_MACRO,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    if let LValue::Symbol(s) = &args[0] {
        Ok(match env.get_macro(s).cloned() {
            Some(l) => l.into(),
            None => LValue::Nil,
        })
    } else {
        Err(WrongType(
            ENV_GET_MACRO,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::Symbol,
        ))
    }
}

#[deprecated]
pub fn begin(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.last() {
        None => Err(LError::SpecialError(
            "begin",
            "no SExpr after begin".to_string(),
        )),
        Some(v) => Ok(v.clone()),
    }
}

/// Returns a list
pub fn list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.is_empty() {
        Ok(LValue::Nil)
    } else {
        Ok(LValue::List(args.to_vec()))
    }
}

/// Construct a map
pub fn map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        0 => Ok(LValue::Map(Default::default())),
        1 => match args.get(0).unwrap() {
            LValue::List(list_sv) => {
                let mut facts: HashMap<LValue, LValue> = Default::default();
                for sv in list_sv {
                    match sv {
                        LValue::List(val_sv) => {
                            if val_sv.len() != 2 {
                                return Err(WrongNumberOfArgument(
                                    MAP,
                                    val_sv.into(),
                                    val_sv.len(),
                                    2..2,
                                ));
                            }

                            let key = val_sv[0].clone();
                            let value = val_sv[1].clone();
                            facts.insert(key, value);
                        }
                        lv => {
                            return Err(WrongType(MAP, lv.clone(), lv.into(), NameTypeLValue::List))
                        }
                    }
                }
                Ok(LValue::Map(facts))
            }
            LValue::Nil => Ok(LValue::Map(Default::default())),
            lv => Err(WrongType(MAP, lv.clone(), lv.into(), NameTypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(MAP, args.into(), args.len(), 1..1)),
    }
}

#[deprecated]
pub fn set(args: &[LValue], env: &LEnv, ctx: &()) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            SET,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }
    match &args[0] {
        LValue::Map(_) => set_map(args, env, ctx),
        LValue::List(_) | LValue::Nil => set_list(args, env, ctx),
        _ => Err(NotInListOfExpectedTypes(
            SET,
            args[0].clone(),
            (&args[0]).into(),
            vec![
                NameTypeLValue::List,
                NameTypeLValue::Map,
                NameTypeLValue::Nil,
            ],
        )),
    }
}

pub fn get(args: &[LValue], env: &LEnv, ctx: &()) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            GET,
            args.into(),
            0,
            1..std::usize::MAX,
        ));
    }
    match &args[0] {
        LValue::Map(_) => get_map(args, env, ctx),
        LValue::List(_) | LValue::Nil => get_list(args, env, ctx),
        _ => Err(NotInListOfExpectedTypes(
            GET,
            args[0].clone(),
            (&args[0]).into(),
            vec![
                NameTypeLValue::List,
                NameTypeLValue::Map,
                NameTypeLValue::Nil,
            ],
        )),
    }
}

pub fn get_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            GET_MAP,
            args.into(),
            0,
            1..std::usize::MAX,
        ));
    }

    match &args[0] {
        LValue::Map(map) => {
            let key = &args[1];
            let value = map.get(key).unwrap_or(&LValue::Nil);
            Ok(value.clone())
        }
        lv => Err(WrongType(
            GET_MAP,
            lv.clone(),
            lv.into(),
            NameTypeLValue::Map,
        )),
    }
}

pub fn set_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            SET_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    match &args[0] {
        LValue::Map(m) => match &args[1] {
            LValue::List(val_sv) => {
                if val_sv.len() == 2 {
                    let key = val_sv.get(0).unwrap().clone();
                    let value = val_sv.get(1).unwrap().clone();
                    Ok(m.update(key, value).into())
                } else {
                    Err(WrongNumberOfArgument(
                        SET_MAP,
                        val_sv.into(),
                        val_sv.len(),
                        2..2,
                    ))
                }
            }
            lv => Err(WrongType(
                SET_MAP,
                lv.clone(),
                lv.into(),
                NameTypeLValue::List,
            )),
        },
        lv => Err(WrongType(
            SET_MAP,
            lv.clone(),
            lv.into(),
            NameTypeLValue::Map,
        )),
    }
}

pub fn remove_key_value_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            REMOVE_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    match &args[0] {
        LValue::Map(m) => match &args[1] {
            LValue::List(val_sv) => {
                if val_sv.len() == 2 {
                    let key = val_sv.get(0).unwrap().clone();
                    let value = val_sv.get(1).unwrap().clone();
                    match m.get(&key) {
                        None => {
                            return Err(SpecialError(
                                REMOVE_MAP,
                                format!("map does not contain key {}", key),
                            ))
                        }
                        Some(v) => {
                            if *v == value {
                                let mut m = m.clone();
                                m.remove(&key);
                                Ok(m.into())
                            } else {
                                Err(SpecialError(
                                    REMOVE_MAP,
                                    format!("map does not entry key ({}:{})", key, value),
                                ))
                            }
                        }
                    }
                } else {
                    Err(WrongNumberOfArgument(
                        REMOVE_MAP,
                        val_sv.into(),
                        val_sv.len(),
                        2..2,
                    ))
                }
            }
            lv => Err(WrongType(
                REMOVE_MAP,
                lv.clone(),
                lv.into(),
                NameTypeLValue::List,
            )),
        },
        lv => Err(WrongType(
            REMOVE_MAP,
            lv.clone(),
            lv.into(),
            NameTypeLValue::Map,
        )),
    }
}

pub fn remove_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            REMOVE_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    match &args[0] {
        LValue::Map(m) => match &args[1] {
            LValue::List(val_sv) => {
                if val_sv.len() == 1 {
                    let key = val_sv.get(0).unwrap().clone();
                    let mut new_m = m.clone();
                    new_m.remove(&key);
                    Ok(new_m.into())
                } else {
                    Err(WrongNumberOfArgument(
                        REMOVE_MAP,
                        val_sv.into(),
                        val_sv.len(),
                        1..1,
                    ))
                }
            }
            lv => Err(WrongType(
                REMOVE_MAP,
                lv.clone(),
                lv.into(),
                NameTypeLValue::List,
            )),
        },
        lv => Err(WrongType(
            REMOVE_MAP,
            lv.clone(),
            lv.into(),
            NameTypeLValue::Map,
        )),
    }
}

/// Merges two hashmap tables
pub fn union_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            UNION_MAP,
            args.into(),
            args.len(),
            2..2,
        ));
    }
    let map1 = &args[0];
    let map2 = &args[1];

    if let LValue::Map(map1) = map1.clone() {
        if let LValue::Map(map2) = map2.clone() {
            Ok(map1.union(map2).into())
        } else {
            Err(WrongType(
                UNION_MAP,
                map2.clone(),
                map2.into(),
                NameTypeLValue::Map,
            ))
        }
    } else {
        Err(WrongType(
            UNION_MAP,
            map1.clone(),
            map1.into(),
            NameTypeLValue::Map,
        ))
    }
}

/*pub fn print(args: &[LValue], _:& LEnv, _: & ()) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), 0, 1..std::usize::MAX));
    }
    return Ok(args.get(0).unwrap().clone());
}*/

///It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
//TODO: implement all the casesn
pub fn cons(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(CONS, args.into(), args.len(), 2..2));
    }
    let first = &args[0];
    let second = &args[1];
    match second {
        LValue::List(list) => {
            let mut new_list = vec![first.clone()];
            new_list.append(&mut list.clone());
            Ok(new_list.into())
        }
        LValue::Nil => Ok(vec![first.clone()].into()),
        _ => Ok(vec![first.clone(), second.clone()].into()),
    }
}

///It takes a list as argument, and returns its first element.
pub fn car(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match &args[0] {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.first().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(WrongType(CAR, lv.clone(), lv.into(), NameTypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(CAR, args.into(), args.len(), 1..1)),
    }
}

///It takes a list as argument, and returns a list without the first element
pub fn cdr(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match &args[0] {
            LValue::List(list) => {
                if list.len() < 2 {
                    Ok(LValue::Nil)
                } else {
                    //let slice = &list[1..];
                    //let vec = slice.to_vec();
                    let mut new_list = list.clone();
                    new_list.remove(0);
                    Ok(new_list.into())
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(WrongType(CDR, lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(CDR, args.into(), args.len(), 1..1))
    }
}

///It merges two or more list into one.
pub fn append(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    let mut new_list = Vec::new();
    for element in args {
        match element {
            LValue::List(list) => new_list.append(&mut list.clone()),
            LValue::Nil => {}
            _ => {
                return Err(WrongType(
                    APPEND,
                    element.clone(),
                    element.into(),
                    NameTypeLValue::List,
                ))
            }
        }
    }
    Ok(new_list.into())
}

///It takes a list and returns the last element.
pub fn last(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.last().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            lv => Err(WrongType(LAST, lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(LAST, args.into(), args.len(), 1..1))
    }
}

///It takes two arguments of which the second must be a list,
/// if the first argument is a member of the second argument,
/// and then it returns the remainder of the list beginning with the first argument.
pub fn member(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(MEMBER, args.into(), args.len(), 2..2));
    }
    let value_to_find = &args[0];
    match &args[1] {
        LValue::List(list) => {
            for (k, element) in list.iter().enumerate() {
                if element == value_to_find {
                    return Ok(list[k..].into());
                }
            }
            Ok(LValue::Nil)
        }
        lv => Err(WrongType(
            MEMBER,
            lv.clone(),
            lv.into(),
            NameTypeLValue::List,
        )),
    }
}

pub fn get_list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            GET_LIST,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    if let LValue::List(vec) = &args[0] {
        if let LValue::Number(LNumber::Int(i)) = &args[1] {
            if vec.len() > *i as usize {
                Ok(vec[*i as usize].clone())
            } else {
                Err(SpecialError(
                    GET_LIST,
                    format!(
                        "list: {}. {} is out of bound, must be in [{};{}]",
                        LValue::from(args),
                        i,
                        0,
                        vec.len() - 1
                    ),
                ))
            }
        } else {
            Err(WrongType(
                GET_LIST,
                args[1].clone(),
                (&args[1]).into(),
                NameTypeLValue::Int,
            ))
        }
    } else {
        Err(WrongType(
            GET_LIST,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}

pub fn set_list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 3 {
        return Err(WrongNumberOfArgument(
            GET_LIST,
            args.into(),
            args.len(),
            3..3,
        ));
    }

    if let LValue::List(vec) = &args[0] {
        if let LValue::Number(LNumber::Int(i)) = &args[2] {
            if vec.len() > *i as usize {
                let mut vec = vec.clone();
                vec[*i as usize] = args[1].clone();
                Ok(vec.into())
            } else {
                Err(SpecialError(
                    GET_LIST,
                    format!("index out of bound, must be in [{};{}]", 0, vec.len() - 1),
                ))
            }
        } else {
            Err(WrongType(
                GET_LIST,
                args[1].clone(),
                (&args[1]).into(),
                NameTypeLValue::Int,
            ))
        }
    } else {
        Err(WrongType(
            GET_LIST,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}

/// It takes a list and returns a list with the top elements in reverse order.
pub fn reverse(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                let mut new_list = list.clone();
                new_list.reverse();
                Ok(new_list.into())
            }
            lv => Err(WrongType(
                REVERSE,
                lv.clone(),
                lv.into(),
                NameTypeLValue::List,
            )),
        }
    } else {
        Err(WrongNumberOfArgument(
            REVERSE,
            args.into(),
            args.len(),
            1..1,
        ))
    }
}

/// return the length of the object if it is a table or a list.
pub fn length(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            REVERSE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    match &args[0] {
        LValue::List(l) => Ok(l.len().into()),
        LValue::Map(m) => Ok(m.len().into()),
        LValue::Nil => Ok(0.into()),
        lv => Err(NotInListOfExpectedTypes(
            LEN,
            lv.clone(),
            lv.into(),
            vec![NameTypeLValue::List, NameTypeLValue::Map],
        )),
    }
}
/// Returns true if a hashmap or list is empty
pub fn empty(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(EMPTY, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::List(l) => Ok(l.is_empty().into()),
        LValue::Map(m) => Ok(m.is_empty().into()),
        LValue::Nil => Ok(true.into()),
        lv => Err(NotInListOfExpectedTypes(
            EMPTY,
            lv.clone(),
            lv.into(),
            vec![
                NameTypeLValue::List,
                NameTypeLValue::Map,
                NameTypeLValue::Nil,
            ],
        )),
    }
}

/// Logical functional not
/// true => nil
/// nil => true
pub fn not(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(NOT, args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Nil => Ok(LValue::True),
        _ => Ok(LValue::Nil),
    }
}

pub fn add(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn sub(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => &args[0] - &args[1],
        i => Err(WrongNumberOfArgument(SUB, args.into(), i, 2..2)),
    }
}

pub fn mul(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn div(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => &args[0] / &args[1],
        i => Err(WrongNumberOfArgument(DIV, args.into(), i, 2..2)),
    }
}

/// Compares two values. Returns true if the first arg is greater than the second. Nil Otherwise
pub fn gt(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] > args[1]).into()),
        i => Err(WrongNumberOfArgument(GT, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is less than the second. Nil Otherwise
pub fn lt(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] < args[1]).into()),
        i => Err(WrongNumberOfArgument(LT, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is greater or equal to the second. Nil Otherwise
pub fn ge(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] >= args[1]).into()),
        i => Err(WrongNumberOfArgument(GE, args.into(), i, 2..2)),
    }
}
/// Compares two values. Returns true if the first arg is less or equal to the second. Nil Otherwise
pub fn le(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] <= args[1]).into()),
        i => Err(WrongNumberOfArgument(LE, args.into(), i, 2..2)),
    }
}

/// Compares two values. Returns true if the first and second args are equal. Nil Otherwise
pub fn eq(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] == args[1]).into()),
        i => Err(WrongNumberOfArgument(EQ, args.into(), i, 2..2)),
    }
}

//Predicates

//Type verification
/// Returns true if LValue is Nil
pub fn is_nil(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(&args[0]) == NameTypeLValue::Nil).into()),
        i => Err(WrongNumberOfArgument(IS_NIL, args.into(), i, 1..1)),
    }
}

/// Returns true is LValue is number
pub fn is_number(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(&args[0]) == NameTypeLValue::Number).into()),
        i => Err(WrongNumberOfArgument(IS_NUMBER, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is integer
pub fn is_integer(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => {
            if let LValue::Number(LNumber::Int(_)) = &args[0] {
                Ok(true.into())
            } else {
                Ok(false.into())
            }
        }
        i => Err(WrongNumberOfArgument(IS_INTEGER, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is float
pub fn is_float(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn is_bool(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Bool).into()),
        i => Err(WrongNumberOfArgument(IS_BOOL, args.into(), i, 1..1)),
    }
}
/// Returns true if LValue is a function
pub fn is_fn(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Fn).into()),
        i => Err(WrongNumberOfArgument(IS_FN, args.into(), i, 1..1)),
    }
}
/// Returns true if LValue is a mut function
pub fn is_mut_fn(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::MutFn).into()),
        i => Err(WrongNumberOfArgument(IS_MUT_FN, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a symbol
pub fn is_symbol(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_SYMBOL, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a string
pub fn is_string(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::String(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_SYMBOL, args.into(), i, 1..1)),
    }
}
/// Returns true if LValue is a list
pub fn is_list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::List(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_LIST, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a lambda
pub fn is_lambda(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_LAMBDA, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a quote
pub fn is_quote(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Quote(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(IS_QUOTE, args.into(), i, 1..1)),
    }
}

/// Returns true if LValue is a hashmap
pub fn is_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn is_equal(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
                NameTypeLValue::List,
            ))
        }
    } else {
        Err(WrongType(
            IS_EQUAL,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}

/// Returns true if a list is not empty
pub fn is_pair(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
            NameTypeLValue::List,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::core::LEnv;
    use crate::functions::*;
    use std::convert::TryInto;

    #[test]
    fn test_add() {
        let env = LEnv::default();
        let result = add(&[3.into(), 2.into()], &env, &()).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(5.0)), result);
    }

    #[test]
    fn test_sub() {
        let env = LEnv::default();
        let result = sub(&[3.into(), 2.into()], &env, &()).unwrap();
        assert_eq!(LValue::Number(LNumber::Int(1)), result);
    }

    #[test]
    fn test_mul() {
        let env = LEnv::default();
        let result = mul(&[3.into(), 2.into()], &env, &()).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(6.0)), result);
    }

    #[test]
    fn test_div() {
        let env = LEnv::default();
        let result = div(&[3.0.into(), 2.0.into()], &env, &()).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(1.5)), result);
    }

    #[test]
    fn test_gt() {
        let env = LEnv::default();
        let result_true: bool = gt(&[3.into(), 2.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = gt(&[2.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_false_2: bool = gt(&[3.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_ge() {
        let env = LEnv::default();
        let result_true: bool = ge(&[3.into(), 2.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = ge(&[2.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_true_2: bool = ge(&[3.into(), 3.into()], &env, &())
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
        let result_false: bool = lt(&[3.into(), 2.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_true: bool = lt(&[2.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_false_2: bool = lt(&[3.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_le() {
        let env = LEnv::default();
        let result_false: bool = le(&[3.into(), 2.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_true: bool = le(&[2.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        let result_true_2: bool = le(&[3.into(), 3.into()], &env, &())
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }
}

/*
def let(*args):
    args = list(args)
    x = cons(_let, args)
    require(x, len(args)>1)
    bindings, body = args[0], args[1:]
    require(x, all(isa(b, list) and len(b)==2 and isa(b[0], Symbol)
                   for b in bindings), "illegal binding list")
    vars, vals = zip(*bindings)
    return [[_lambda, list(vars)]+map(expand, body)] + map(expand, vals)
 */
/*pub fn _let(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() < 2 {
        return Err(WrongNumberOfArgument(
            args.into(),
            args.len(),
            2..std::usize::MAX,
        ));
    }
    let (bindings, _body) = (&args[0], &args[1..]);

    //Verification of the bindings
    if let LValue::List(bindings) = bindings {
        for b in bindings {
            if let LValue::List(binding) = b {
                if binding.len() == 2 {
                    if matches!(&args[0], LValue::Symbol(_)) {
                    } else {
                        return Err(SpecialError("Illegal binding list".to_string()));
                    }
                } else {
                    return Err(SpecialError("Illegal binding list".to_string()));
                }
            } else {
                return Err(SpecialError("Illegal binding list".to_string()));
            }
        }
    } else {
        return Err(SpecialError("Illegal binding list".to_string()));
    }

    Ok(LValue::Nil)
}*/
