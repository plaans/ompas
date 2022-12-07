use sompas_language::list::*;
use sompas_macros::scheme_fn;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_type};
use std::ops::Deref;

#[derive(Default)]
pub struct ModList {}

impl From<ModList> for LModule {
    fn from(m: ModList) -> LModule {
        let mut module = LModule::new(m, MOD_LIST, DOC_MOD_LIST);
        module.add_fn(FN_LIST, fn_list, (DOC_LIST, DOC_LIST_VERBOSE), true);
        module.add_fn(FIRST, first, DOC_FIRST, true);
        module.add_fn(SECOND, second, DOC_SECOND, true);
        module.add_fn(THIRD, third, DOC_THIRD, true);
        module.add_fn(REST, rest, DOC_REST, true);
        module.add_fn(CAR, car, DOC_CAR, true);
        module.add_fn(CDR, cdr, DOC_CDR, true);
        module.add_fn(APPEND, append, DOC_APPEND, true);
        module.add_fn(LAST, last, DOC_LAST, true);
        module.add_fn(MEMBER, member, DOC_MEMBER, true);
        module.add_fn(REVERSE, reverse, DOC_REVERSE, true);
        module.add_fn(
            GET_LIST,
            get_list,
            (DOC_GET_LIST, DOC_GET_LIST_VERBOSE),
            true,
        );
        module.add_fn(
            SET_LIST,
            set_list,
            (DOC_SET_LIST, DOC_SET_LIST_VERBOSE),
            true,
        );
        module.add_fn(CONS, cons, (DOC_CONS, DOC_CONS_VERBOSE), true);
        module.add_fn(
            INTERSECTION,
            intersection,
            (DOC_INTERSECTION, DOC_INTERSECTION_VERBOSE),
            true,
        );
        module
    }
}

/// Returns a list
#[scheme_fn]
pub fn fn_list(args: &[LValue]) -> LValue {
    if args.is_empty() {
        LValue::Nil
    } else {
        args.into()
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
    Ok(new_list)
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

/// It takes a list and returns a list with the top elements in reverse order.
#[scheme_fn]
pub fn reverse(list: Vec<LValue>) -> Vec<LValue> {
    let mut new_list: Vec<LValue> = vec![];
    for e in list.iter().rev() {
        new_list.push(e.clone())
    }
    new_list
}

#[scheme_fn]
pub fn get_list(list: Vec<LValue>, index: i64) -> LResult {
    if list.len() > index as usize {
        Ok(list[index as usize].clone())
    } else {
        Err(lruntimeerror!(
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
        Err(lruntimeerror!(
            SET_LIST,
            format!("index out of bound, must be in [{};{}]", 0, list.len() - 1)
        ))
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
