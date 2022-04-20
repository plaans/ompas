use anyhow::anyhow;
use function_name;
use im::{vector, Vector};
use macro_rules_attribute::macro_rules_attribute;
use sompas_language::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::KindLValue;
use sompas_structs::{check_number_of_args, lfn, lfn_extended, list, wrong_type};
/// Returns a list
/*lfn! {
pub list(args, _){
    if args.is_empty() {
        Ok(LValue::Nil)
    } else {
        Ok(args.into())
    }
}
}*/

#[macro_rules_attribute(lfn_extended!)]
pub fn list(args: Vector<LValue>) -> LValue {
    if args.is_empty() {
        LValue::Nil
    } else {
        args.into()
    }
}

///It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
/*lfn! {pub cons(args, _){
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(CONS, args.into(), args.len(), 2..2));
    }
    let first = &args[0];
    let second = &args[1];
    match second {
        LValue::List(list) => {
            let mut new_list = vector![first.clone()];
            new_list.append(list.clone());
            Ok(new_list.into())
        }
        LValue::Nil => Ok(vec![first.clone()].into()),
        _ => Ok(vec![first.clone(), second.clone()].into()),
    }
}
    }*/

#[macro_rules_attribute(lfn_extended!)]
pub fn cons(a: LValue, b: LValue) -> Vector<LValue> {
    match b {
        LValue::List(list) => {
            let mut new_list = vector![a.clone()];
            new_list.append(list.clone());
            new_list
        }
        LValue::Nil => vector![a.clone()],
        _ => vector![a.clone(), b.clone()],
    }
}

lfn! {pub first(args, _){
    match args.len() {
        1 => match &args[0] {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.front().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(wrong_type!(FIRST, lv, KindLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(FIRST, args.into(), args.len(), 1..1)),
    }
}
    }

lfn! {pub second(args, _){
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
            lv => Err(wrong_type!(SECOND,lv,KindLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(SECOND, args.into(), args.len(), 1..1)),
    }
}
    }

lfn! {pub third(args, _){
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
            lv => Err(wrong_type!(THIRD, lv, KindLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(THIRD, args.into(), args.len(), 1..1)),
    }
}
    }

///It takes a list as argument, and returns its first element.
lfn! {pub car(args, _){
    match args.len() {
        1 => match &args[0] {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.front().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(wrong_type!(CAR, lv, KindLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(CAR, args.into(), args.len(), 1..1)),
    }
}
    }

///It takes a list as argument, and returns a list without the first element
lfn! {pub cdr(args, _){
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
                    Ok(LValue::List(new_list))
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(wrong_type!(CDR, lv, KindLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(CDR, args.into(), args.len(), 1..1))
    }
}
    }

///It takes a list as argument, and returns a list without the first element
lfn! {pub rest(args, _){
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
                    Ok(LValue::List(new_list))
                }
            }
            LValue::Nil => Ok(LValue::Nil),
            lv => Err(wrong_type!(REST, lv, KindLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(REST, args.into(), args.len(), 1..1))
    }
}
    }

///It merges two or more list into one.
lfn! {pub append(args, _){
    let mut new_list = vector![];
    for element in args {
        match element {
            LValue::List(list) => new_list.append(list.clone()),
            LValue::Nil => {}
            _ => {
                return Err(WrongType(
                    APPEND,
                    element.clone(),
                    element.into(),
                    KindLValue::List,
                ))
            }
        }
    }
    Ok(new_list.into())
}
    }

///It takes a list and returns the last element.
lfn! {pub last(args, _){
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.last().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            lv => Err(wrong_type!(LAST, lv, KindLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(LAST, args.into(), args.len(), 1..1))
    }
}
    }

///It takes two arguments of which the second must be a list,
/// if the first argument is a member of the second argument,
/// and then it returns the remainder of the list beginning with the first argument.
lfn! {pub member(args, _){
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(MEMBER, args.into(), args.len(), 2..2));
    }
    let value_to_find = &args[0];
    match &args[1] {
        LValue::List(list) => {
            for (k, element) in list.iter().enumerate() {
                if element == value_to_find {
                    return Ok(list.clone().slice(k..).into());
                }
            }
            Ok(LValue::Nil)
        }
        lv => Err(wrong_type!(MEMBER, lv, KindLValue::List)),
    }
}
    }

lfn! {pub get_list(args, _){
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
                Err(anyhow!(
                    "list: {}. {} is out of bound, must be in [{};{}]",
                    LValue::from(args),
                    i,
                    0,
                    vec.len() - 1
                )
                .into())
            }
        } else {
            Err(WrongType(
                GET_LIST,
                args[1].clone(),
                (&args[1]).into(),
                KindLValue::Int,
            ))
        }
    } else {
        Err(WrongType(
            GET_LIST,
            args[0].clone(),
            (&args[0]).into(),
            KindLValue::List,
        ))
    }
}
    }

lfn! {pub set_list(args, _){
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
                Err(anyhow!("index out of bound, must be in [{};{}]", 0, vec.len() - 1).into())
            }
        } else {
            Err(WrongType(
                SET_LIST,
                args[1].clone(),
                (&args[1]).into(),
                KindLValue::Int,
            ))
        }
    } else {
        Err(WrongType(
            SET_LIST,
            args[0].clone(),
            (&args[0]).into(),
            KindLValue::List,
        ))
    }
}
    }

/// It takes a list and returns a list with the top elements in reverse order.
lfn! {pub reverse(args, _){
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                let mut new_list: Vector<LValue> = vector![];
                for e in list.iter().rev() {
                    new_list.push_back(e.clone())
                }
                Ok(new_list.into())
            }
            lv => Err(wrong_type!(REVERSE, lv, KindLValue::List)),
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
    }

/// Returns a list of element present in all lists
lfn! {pub intersection(args, _){
    if args.is_empty() {
        return Ok(LValue::Nil);
    }
    let mut intersection = vec![];
    let mut vec_list = vec![];
    for e in args {
        match e {
            LValue::List(list) => {
                vec_list.push(list);
            }
            LValue::Nil => return Ok(LValue::Nil),
            _ => {
                return Err(WrongType(
                    INTERSECTION,
                    e.clone(),
                    e.into(),
                    KindLValue::List,
                ))
            }
        }
    }

    for e in vec_list[0] {
        let mut in_all = true;
        for list in &vec_list[1..] {
            in_all &= list.contains(e);
        }

        if in_all {
            intersection.push(e.clone())
        }
    }

    Ok(intersection.into())
}
}
