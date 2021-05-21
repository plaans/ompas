use crate::core::RefLEnv;
use crate::structs::LError::{
    NotInListOfExpectedTypes, SpecialError, WrongNumberOfArgument, WrongType,
};
use crate::structs::{LError, LValue, NameTypeLValue};
use im::HashMap;
use std::convert::TryFrom;

pub fn env(_: &[LValue], env: &RefLEnv, _: &()) -> Result<LValue, LError> {
    Ok(env
        .keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn begin(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    match args.last() {
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone()),
    }
}

pub fn default(_args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    Ok(LValue::Symbol("default function".to_string()))
}

pub fn list(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    Ok(LValue::List(args.to_vec()))
}

pub fn map(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    let mut facts: HashMap<LValue, LValue> = Default::default();
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    match args.get(0).unwrap() {
        LValue::List(list_sv) => {
            //println!("list_sv : {:?}", list_sv);
            for sv in list_sv {
                match sv {
                    LValue::List(val_sv) => {
                        //println!("sv: {:?}", val_sv);
                        if val_sv.len() != 3 {
                            return Err(WrongNumberOfArgument(val_sv.into(), val_sv.len(), 3..3));
                        }
                        if String::try_from(&val_sv[1])?.as_str().eq(".") {
                            //println!("insert a new fact");
                            let key = val_sv[0].clone();
                            let value = val_sv[2].clone();
                            facts.insert(key, value);
                        } else {
                            //println!("doesn't match pattern")
                        }
                    }
                    lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
                }
            }
        }
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
    }
    Ok(LValue::Map(facts))
}

//TODO: Define set behaviour for other type of LValue
pub fn set(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }
    let lv = args.get(0).unwrap();
    match lv {
        LValue::Map(s) => {
            let mut facts = s.clone();
            if let LValue::List(list) = args.get(1).unwrap() {
                if list.len() != 2 {
                    return Err(WrongNumberOfArgument(args.into(), list.len(), 2..2));
                }
                let key = list.get(0).unwrap();
                let value = list.get(1).unwrap();
                facts.insert(key.clone(), value.clone());
            }
            Ok(LValue::Map(facts))
        }
        lv => Err(LError::SpecialError(format!(
            "Cannot set a {}",
            NameTypeLValue::from(lv)
        ))),
    }
}

pub fn get(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), 0, 1..std::usize::MAX));
    }
    let lv = args.get(0).unwrap();
    match lv {
        LValue::Map(map) => {
            if args.len() == 2 {
                let key = &args[1];
                let value = map.get(key).unwrap_or(&LValue::Nil);
                Ok(value.clone())
            } else if args.len() == 1 {
                Ok(LValue::Map(map.clone()))
            } else {
                Err(WrongNumberOfArgument(lv.clone(), args.len(), 1..2))
            }
        }
        lv => {
            if args.len() > 1 {
                Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
            } else {
                Ok(lv.clone())
            }
        }
    }
}

pub fn get_map(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), 0, 1..std::usize::MAX));
    }

    match &args[0] {
        LValue::Map(map) => {
            let key = &args[1];
            let value = map.get(key).unwrap_or(&LValue::Nil);
            Ok(value.clone())
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}

pub fn set_map(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    match &args[0] {
        LValue::Map(m) => match &args[1] {
            LValue::List(val_sv) => {
                if val_sv.len() == 3 {
                    if String::try_from(&val_sv[1])
                        .unwrap_or_else(|_| String::from(""))
                        .as_str()
                        .eq(".")
                    {
                        let key = val_sv.get(0).unwrap().clone();
                        let value = val_sv.get(2).unwrap().clone();
                        Ok(m.update(key, value).into())
                    } else {
                        Err(SpecialError(
                            "Expected an entry of the format (<key> . <value>)".to_string(),
                        ))
                    }
                } else {
                    Err(WrongNumberOfArgument(val_sv.into(), val_sv.len(), 3..3))
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        },
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}

/*pub fn print(args: &[LValue], _:& RefLEnv, _: & ()) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), 0, 1..std::usize::MAX));
    }
    return Ok(args.get(0).unwrap().clone());
}*/

///It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
//TODO: implement all the cases
pub fn cons(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }
    let first = &args[0];
    let second = &args[1];
    match second {
        LValue::List(list) => {
            let mut new_list = vec![first.clone()];
            new_list.append(&mut list.clone());
            Ok(new_list.into())
        }
        LValue::Nil => {
            Ok(vec![first.clone()].into())
        }
        _ => Ok(vec![first.clone(), second.clone()].into())
    }
}

///It takes a list as argument, and returns its first element.
pub fn car(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.first().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
}

///It takes a list as argument, and returns a list without the first element
pub fn cdr(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match &args[0] {
            LValue::List(list) => {
                if list.len() < 2 {
                    Ok(LValue::Nil)
                } else {
                    let mut new_list = list.clone();
                    new_list.remove(0);
                    Ok(new_list.into())
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
}

///It merges two or more list into one.
pub fn append(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    let mut new_list = Vec::new();
    for element in args {
        match element {
            LValue::List(list) => new_list.append(&mut list.clone()),
            _ => {
                return Err(WrongType(
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
pub fn last(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                if !list.is_empty() {
                    Ok(list.last().unwrap().clone())
                } else {
                    Ok(LValue::Nil)
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
}

///It takes two arguments of which the second must be a list,
/// if the first argument is a member of the second argument,
/// and then it returns the remainder of the list beginning with the first argument.
pub fn member(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
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
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
    }
}

/// It takes a list and returns a list with the top elements in reverse order.
pub fn reverse(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() == 1 {
        match args.first().unwrap() {
            LValue::List(list) => {
                let mut new_list = list.clone();
                new_list.reverse();
                Ok(new_list.into())
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
}

/// return the length of the object if it is a table or a list.
pub fn length(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::List(l) => Ok(l.len().into()),
        LValue::Map(m) => Ok(m.len().into()),
        lv => Err(NotInListOfExpectedTypes(
            lv.clone(),
            lv.into(),
            vec![NameTypeLValue::List, NameTypeLValue::Map],
        )),
    }
}

pub fn empty(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::List(l) => Ok(l.is_empty().into()),
        LValue::Map(m) => Ok(m.is_empty().into()),
        LValue::Nil => Ok(true.into()),
        lv => Err(NotInListOfExpectedTypes(
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

pub fn not(args: &[LValue], _: &RefLEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Nil => Ok(LValue::True),
        _ => Ok(LValue::Nil),
    }
}
