use crate::core::root_module::list::language::*;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError;
use crate::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::typelvalue::TypeLValue;

pub mod language {

    /*
    LIST FUNCTIONS
     */
    pub const FIRST: &str = "first";
    pub const SECOND: &str = "second";
    pub const THIRD: &str = "third";
    pub const REST: &str = "rest";
    pub const CAR: &str = "car";
    pub const CDR: &str = "cdr";
    pub const APPEND: &str = "append";
    pub const LAST: &str = "last";
    pub const EMPTY: &str = "empty";
    pub const LEN: &str = "len";
    pub const MEMBER: &str = "member";
    pub const REVERSE: &str = "reverse";
    pub const GET_LIST: &str = "get-list";
    pub const SET_LIST: &str = "set-list";
    pub const CONS: &str = "cons";

    pub const DOC_FIRST: &str = "Return the first element of a list or nil.";
    pub const DOC_SECOND: &str = "Return the second element of a list or nil.";
    pub const DOC_THIRD: &str = "Return the third element of a list or nil.";
    pub const DOC_REST: &str = "Same as cdr";
    pub const DOC_CAR: &str =
        "Takes a list of at least one element, and return the first element. Nil otherwise.";
    pub const DOC_CDR: &str =
        "Takes a list of at least one element, and return a list without the first element.";
    pub const DOC_APPEND: &str = "Takes two list and return a list merging both.";
    pub const DOC_MEMBER: &str = "Takes two arguments of which the second must be a list. \
if the first argument is a member of the second argument,\
and then it returns the remainder of the list beginning with the first argument.";
    pub const DOC_LAST: &str =
        "Takes a list of at least one element and return the last element. Nil otherwise";
    pub const DOC_EMPTY: &str = "Return true if the a list or map is empty.";
    pub const DOC_LEN: &str = "Return the len of a list or a map";
    pub const DOC_REVERSE: &str =
        "Takes a list and return a list with all elements reversed in order";
    pub const DOC_LIST: &str = "Return a list of the LValues given is argument";
    pub const DOC_GET_LIST: &str = "todo!";
    pub const DOC_SET_LIST: &str = "todo!";
    pub const DOC_CONS: &str = "Takes two objects and merges into a list.";
}

/// Returns a list
pub fn list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.is_empty() {
        Ok(LValue::Nil)
    } else {
        Ok(LValue::List(args.to_vec()))
    }
}

///It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
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

pub fn first(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
            lv => Err(WrongType(FIRST, lv.clone(), lv.into(), TypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(FIRST, args.into(), args.len(), 1..1)),
    }
}

pub fn second(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match &args[0] {
            LValue::List(list) => {
                if list.len() >= 2 {
                    Ok(list[1].clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(WrongType(SECOND, lv.clone(), lv.into(), TypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(SECOND, args.into(), args.len(), 1..1)),
    }
}

pub fn third(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match &args[0] {
            LValue::List(list) => {
                if list.len() >= 3 {
                    Ok(list[2].clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(WrongType(THIRD, lv.clone(), lv.into(), TypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(THIRD, args.into(), args.len(), 1..1)),
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
            lv => Err(WrongType(CAR, lv.clone(), lv.into(), TypeLValue::List)),
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
            lv => Err(WrongType(CDR, lv.clone(), lv.into(), TypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(CDR, args.into(), args.len(), 1..1))
    }
}

///It takes a list as argument, and returns a list without the first element
pub fn rest(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
            lv => Err(WrongType(REST, lv.clone(), lv.into(), TypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(REST, args.into(), args.len(), 1..1))
    }
}

///It merges two or more list into one.
pub fn append(args: &[LValue], _env: &LEnv, _ctx: &()) -> Result<LValue, LError> {
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
                    TypeLValue::List,
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
            lv => Err(WrongType(LAST, lv.clone(), lv.into(), TypeLValue::List)),
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
        lv => Err(WrongType(MEMBER, lv.clone(), lv.into(), TypeLValue::List)),
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
                TypeLValue::Int,
            ))
        }
    } else {
        Err(WrongType(
            GET_LIST,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}

pub fn set_list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 3 {
        return Err(WrongNumberOfArgument(
            SET_LIST,
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
                    SET_LIST,
                    format!("index out of bound, must be in [{};{}]", 0, vec.len() - 1),
                ))
            }
        } else {
            Err(WrongType(
                SET_LIST,
                args[1].clone(),
                (&args[1]).into(),
                TypeLValue::Int,
            ))
        }
    } else {
        Err(WrongType(
            SET_LIST,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
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
            lv => Err(WrongType(REVERSE, lv.clone(), lv.into(), TypeLValue::List)),
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
