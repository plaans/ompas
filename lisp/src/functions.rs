use crate::core::LEnv;
use crate::structs::LError::{
    NotInListOfExpectedTypes, SpecialError, WrongNumberOfArgument, WrongType,
};
use crate::structs::{LError, LNumber, LValue, NameTypeLValue};
use im::HashMap;
use std::convert::TryFrom;

pub fn env(_: &[LValue], env: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(env
        .keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn begin(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.last() {
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone()),
    }
}

pub fn default(_args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(LValue::Symbol("default function".to_string()))
}

pub fn list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    Ok(LValue::List(args.to_vec()))
}

pub fn map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn set(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn get(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn get_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn set_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn union_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }
    let map1 = &args[0];
    let map2 = &args[1];

    if let LValue::Map(map1) = map1.clone() {
        if let LValue::Map(map2) = map2.clone() {
            Ok(map1.union(map2).into())
        } else {
            Err(WrongType(map2.clone(), map2.into(), NameTypeLValue::Map))
        }
    } else {
        Err(WrongType(map1.clone(), map1.into(), NameTypeLValue::Map))
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
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        },
        _ => Err(WrongNumberOfArgument(args.into(), args.len(), 1..1)),
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
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
}

///It merges two or more list into one.
pub fn append(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List)),
        }
    } else {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
}

///It takes two arguments of which the second must be a list,
/// if the first argument is a member of the second argument,
/// and then it returns the remainder of the list beginning with the first argument.
pub fn member(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn reverse(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
pub fn length(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn empty(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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

pub fn not(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
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

pub fn sub(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => &args[0] - &args[1],
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn mul(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(1.0));
    for value in args {
        result = (&result * value)?;
    }
    Ok(result)
}

pub fn div(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => &args[0] / &args[1],
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

//Comparison functions
pub fn gt(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] > args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn lt(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] < args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn ge(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] >= args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn le(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] <= args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn eq(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] == args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

//Predicates

//Type verification
pub fn is_nil(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(&args[0]) == NameTypeLValue::Nil).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_number(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(&args[0]) == NameTypeLValue::Number).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_integer(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => {
            if let LValue::Number(LNumber::Int(_)) = &args[0] {
                Ok(true.into())
            } else {
                Ok(false.into())
            }
        }
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}
pub fn is_float(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => {
            if let LValue::Number(LNumber::Float(_)) = &args[0] {
                Ok(true.into())
            } else {
                Ok(false.into())
            }
        }
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_bool(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Bool).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_fn(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::Fn).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_mut_fn(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => Ok((NameTypeLValue::from(args.get(0).unwrap()) == NameTypeLValue::MutFn).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_symbol(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::List(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_lambda(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_quote(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Quote(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_map(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => match args.get(0).unwrap() {
            LValue::Map(_) => Ok(LValue::True),
            _ => Ok(LValue::Nil),
        },
        i => Err(WrongNumberOfArgument(args.into(), i, 1..1)),
    }
}

pub fn is_equal(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }
    if let LValue::List(l1) = &args[0] {
        if let LValue::List(l2) = &args[1] {
            Ok((l1 == l2).into())
        } else {
            Err(WrongType(
                args[1].clone(),
                (&args[1]).into(),
                NameTypeLValue::List,
            ))
        }
    } else {
        Err(WrongType(
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}

pub fn is_pair(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    if let LValue::List(l) = &args[0] {
        Ok((!l.is_empty()).into())
    } else {
        Err(WrongType(
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
