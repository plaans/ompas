use sompas_language::*;
use sompas_macros::scheme_fn;
use sompas_structs::lerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::KindLValue;
use sompas_structs::{lerror, wrong_type};
use std::ops::Deref;

/// Returns a list
#[scheme_fn]
pub fn list(args: Vec<LValue>) -> LValue {
    if args.is_empty() {
        LValue::Nil
    } else {
        args.into()
    }
}

///It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
#[scheme_fn]
pub fn cons(a: LValue, b: LValue) -> Vec<LValue> {
    match b {
        LValue::List(list) => {
            let mut new_list = vec![a.clone()];
            new_list.append(&mut list.deref().clone());
            new_list
        }
        LValue::Nil => vec![a.clone()],
        _ => vec![a.clone(), b.clone()],
    }
}

#[scheme_fn]
pub fn first(list: LValue) -> LResult {
    match &list {
        LValue::List(list) => {
            if !list.is_empty() {
                Ok(list[0].clone())
            } else {
                Ok(LValue::Nil)
            }
        }
        LValue::Nil => Ok(LValue::Nil),
        lv => Err(wrong_type!(FIRST, lv, KindLValue::List)),
    }
}

#[scheme_fn]
pub fn second(list: LValue) -> LResult {
    match &list {
        LValue::List(list) => {
            if list.len() >= 2 {
                Ok(list[1].clone())
            } else {
                Ok(LValue::Nil)
            }
        }
        LValue::Nil => Ok(LValue::Nil),
        lv => Err(wrong_type!(SECOND, lv, KindLValue::List)),
    }
}

#[scheme_fn]
pub fn third(list: LValue) -> LResult {
    match &list {
        LValue::List(list) => {
            if list.len() >= 3 {
                Ok(list[2].clone())
            } else {
                Ok(LValue::Nil)
            }
        }
        LValue::Nil => Ok(LValue::Nil),
        lv => Err(wrong_type!(THIRD, lv, KindLValue::List)),
    }
}

///It takes a list as argument, and returns its first element.
#[scheme_fn]
pub fn car(list: LValue) -> LResult {
    match &list {
        LValue::List(list) => {
            if !list.is_empty() {
                Ok(list[0].clone())
            } else {
                Ok(LValue::Nil)
            }
        }
        LValue::Nil => Ok(LValue::Nil),
        lv => Err(wrong_type!(CAR, lv, KindLValue::List)),
    }
}

///It takes a list as argument, and returns a list without the first element
#[scheme_fn]
pub fn cdr(list: LValue) -> Result<Vec<LValue>, LRuntimeError> {
    match &list {
        LValue::List(list) => {
            if list.len() < 2 {
                Ok(vec![])
            } else {
                //let slice = &list[1..];
                //let vec = slice.to_vec();
                let mut new_list = list.deref().clone();
                new_list.remove(0);
                Ok(new_list)
            }
        }
        LValue::Nil => Ok(vec![]),
        lv => Err(wrong_type!(CDR, lv, KindLValue::List)),
    }
}

///It takes a list as argument, and returns a list without the first element
#[scheme_fn]
pub fn rest(list: LValue) -> Result<Vec<LValue>, LRuntimeError> {
    match &list {
        LValue::List(list) => {
            if list.len() < 2 {
                Ok(vec![])
            } else {
                let mut new_list = list.deref().clone();
                new_list.remove(0);
                Ok(new_list)
            }
        }
        LValue::Nil => Ok(vec![]),
        lv => Err(wrong_type!(REST, lv, KindLValue::List)),
    }
}

///It merges two or more list into one.
#[scheme_fn]
pub fn append(args: &[LValue]) -> Result<Vec<LValue>, LRuntimeError> {
    let mut new_list = vec![];
    for element in args {
        match element {
            LValue::List(list) => new_list.append(&mut list.deref().clone()),
            LValue::Nil => {}
            _ => return Err(wrong_type!(APPEND, element, KindLValue::List)),
        }
    }
    Ok(new_list.into())
}

///It takes a list and returns the last element.
#[scheme_fn]
pub fn last(list: Vec<LValue>) -> LValue {
    if !list.is_empty() {
        list.last().unwrap().clone()
    } else {
        LValue::Nil
    }
}

///It takes two arguments of which the second must be a list,
/// if the first argument is a member of the second argument,
/// and then it returns the remainder of the list beginning with the first argument.
#[scheme_fn]
pub fn member(val: &LValue, list: Vec<LValue>) -> LValue {
    for (k, e) in list.iter().enumerate() {
        if e == val {
            return list[k..].into();
        }
    }
    LValue::Nil
}

#[scheme_fn]
pub fn get_list(list: Vec<LValue>, index: i64) -> LResult {
    if list.len() > index as usize {
        Ok(list[index as usize].clone())
    } else {
        Err(lerror!(
            GET_LIST,
            format!(
                "list: {}. {} is out of bound, must be in [{};{}]",
                LValue::from(&list),
                index,
                0,
                list.len() - 1
            )
        ))
    }
}

#[scheme_fn]
pub fn set_list(
    list: Vec<LValue>,
    value: LValue,
    index: i64,
) -> Result<Vec<LValue>, LRuntimeError> {
    if list.len() > index as usize {
        let mut vec = list.clone();
        vec[index as usize] = value;
        Ok(vec)
    } else {
        Err(lerror!(
            SET_LIST,
            format!("index out of bound, must be in [{};{}]", 0, list.len() - 1)
        ))
    }
}

/// It takes a list and returns a list with the top elements in reverse order.
#[scheme_fn]
pub fn reverse(list: Vec<LValue>) -> Vec<LValue> {
    let mut new_list: Vec<LValue> = vec![];
    for e in list.iter().rev() {
        new_list.push(e.clone())
    }
    new_list
}

/// Returns a list of element present in all lists
#[scheme_fn]
pub fn intersection(lists: Vec<Vec<LValue>>) -> Vec<LValue> {
    let mut intersec = vec![];
    let first = &lists[0];
    for e in first {
        let mut in_all = true;
        for list in &lists[1..] {
            in_all &= list.contains(e);
        }

        if in_all {
            intersec.push(e.clone())
        }
    }

    intersec
}
