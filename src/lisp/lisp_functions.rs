use crate::lisp::lisp_language::TYPE_OBJECT;
use crate::lisp::lisp_struct::LError::*;
use crate::lisp::lisp_struct::*;
use aries_utils::input::Sym;
//use std::collections::HashMap;
use crate::lisp::lisp_language::*;
use crate::lisp::{LEnv, eval, parse};
use im::HashMap;
use std::fs::File;
use std::io::{Read};

//Mathematical functions
pub fn add(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(0.0));
    for value in values {
        result = (&result + value)?;
    }
    Ok(result)
}

pub fn sub(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => values.get(0).unwrap() - values.get(1).unwrap(),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

pub fn mul(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(1.0));
    for value in values {
        result = (&result * value)?;
    }
    Ok(result)
}

pub fn div(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => values.get(0).unwrap() / values.get(0).unwrap(),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

//Comparison functions
pub fn gt(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] > values[1])),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

pub fn lt(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] < values[1])),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

pub fn ge(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] >= values[1])),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

pub fn le(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] <= values[1])),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

pub fn eq(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] == values[1])),
        i => Err(WrongNumberOfArgument(values.into(),i, 2..2)),
    }
}

//Type verification
pub fn is_none(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::None,
        )),
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_number(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::Number,
        )),
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_bool(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::Bool,
        )),
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_fn(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::LFn,
        )),
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_type(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Type(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_symbol(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_object(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Object(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_state_function(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::StateFunction(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_map(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Map(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}
pub fn is_list(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::List(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_lambda(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn is_quote(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Quote(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(values.into(),i, 1..1)),
    }
}

pub fn begin(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    match values.last() {
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone()),
    }
}

pub fn default(_values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    Ok(LValue::String("default function".to_string()))
}

pub fn subtype(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.into(),values.len(), 1..1));
    }
    let parent_type: LType = match values.get(0).unwrap() {
        LValue::Symbol(s) => match s.as_str() {
            TYPE_INT => LType::Int,
            TYPE_FLOAT => LType::Float,
            TYPE_BOOL => LType::Object,
            TYPE_OBJECT => LType::Object,
            _str => match env.get_sym_type(s) {
                None => return Err(SpecialError(format!("{} has no type",s))),
                Some(lst) => match lst {
                    LSymType::Type(_) => LType::Symbol(s.clone()),
                    lst => {
                        return Err(WrongType(lst.into(), lst.into(), NameTypeLValue::Type))
                    }
                },
            },
        },
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    };
    Ok(LValue::SymType(LSymType::Type(Some(parent_type))))
}

pub fn state_function(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    let mut vec_params: Vec<Sym> = Vec::new();
    let mut t_value: Sym = Sym::from(TYPE_OBJECT);
    for (i, value) in values.iter().enumerate() {
        match value {
            LValue::Symbol(s) => {
                if is_type(&values[i..i + 1], env)?.as_bool()? {
                    if i == values.len() - 1 {
                        t_value = s.clone();
                    } else {
                        vec_params.push(s.clone())
                    }
                } else {
                    match env.sym_types.get(s) {
                        None => {
                            return Err(WrongType(
                                value.clone(),
                                value.into(),
                                NameTypeLValue::Type,
                            ))
                        }
                        Some(lst) => {
                            return Err(WrongType(
                                value.clone(),
                                lst.into(),
                                NameTypeLValue::Type,
                            ))
                        }
                    }
                }
            }
            lv => {
                return Err(WrongType(
                    lv.clone(),
                    lv.into(),
                    NameTypeLValue::Type,
                ))
            }
        }
    }
    Ok(LValue::SymType(LSymType::StateFunction(LStateFunction {
        t_params: vec_params,
        t_value,
    })))
}

pub fn list(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    Ok(LValue::List(values.to_vec()))
}

pub fn map(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    let mut facts: HashMap<LValue, LValue> = Default::default();
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.into(),values.len(), 1..1))
    }
    match values.get(0).unwrap() {
        LValue::List(list_sv) => {
            //println!("list_sv : {:?}", list_sv);
            for sv in list_sv {
                match sv {
                    LValue::List(val_sv) => {
                        //println!("sv: {:?}", val_sv);
                        if val_sv.get(1).unwrap().as_sym().unwrap_or_else(|_| Sym::from("")).as_str().eq(".") {
                            //println!("insert a new fact");
                            let key = val_sv.get(0).unwrap().clone();
                            let value = val_sv.get(2).unwrap().clone();
                            facts.insert(key, value);
                        } else {
                            //println!("doesn't match pattern")
                        }
                    }
                    lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
                }
            }
        }
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
    }
    Ok(LValue::Map(facts))

}

//TODO: Define set behaviour for other type of LValue
pub fn set(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 2 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 2..2));
    }
    let lv = values.get(0).unwrap();
    match lv {
        LValue::Map(s) => {
            let mut facts = s.clone();
            if let LValue::List(list) = values.get(1).unwrap() {
                if list.len() != 2 {
                    return Err(WrongNumberOfArgument(values.into(),list.len(), 2..2))
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

pub fn get(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    if values.is_empty() {
        return Err(WrongNumberOfArgument(values.into(), 0, 1..std::usize::MAX));
    }
    let lv=values.get(0).unwrap();
    match lv {
        LValue::Map(map) => {
            if values.len() == 2 {
                let key = values.get(1).unwrap();
                let value = map.get(key).unwrap_or(&LValue::None);
                Ok(value.clone())
            } else if values.len() == 1 {
                Ok(LValue::Map(map.clone()))
            } else {
                Err(WrongNumberOfArgument(lv.clone(), values.len(), 1..2))
            }
        }
        lv => {
            if values.len() > 1 {
                return Err(WrongNumberOfArgument(values.into(),values.len(), 1..1));
            }
            Ok(lv.clone())
        }
    }
}

pub fn get_type(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1));
    }
    let sym_type = match values.get(0).unwrap() {
        LValue::Symbol(s) => match env.get_sym_type(s) {
            None => return Err(LError::SpecialError(format!("{} has no type", s))),
            Some(lst) => lst.clone(),
        },
        LValue::Number(n) => match n {
            LNumber::Int(_) => LSymType::Type(Some(LType::Int)),
            LNumber::Float(_) => LSymType::Type(Some(LType::Float)),
        },
        LValue::Bool(_) => LSymType::Type(Some(LType::Bool)),
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Atom)),
    };

    Ok(LValue::SymType(sym_type))
}

pub fn define(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 2 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 2..2))
    }
    match values.get(0).unwrap() {
        LValue::Symbol(s) =>  {
            let exp = eval(values.get(1).unwrap(), env)?;
            env.add_entry(s.to_string(), exp);
        }
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
    }
    Ok(LValue::None)


}
pub fn _if(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 3 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 3..3))
    }
    let test = values.get(0).unwrap();
    let conseq = values.get(1).unwrap();
    let alt = values.get(2).unwrap();
    match eval(test, env) {
        Ok(LValue::Bool(true)) => eval(conseq, env),
        Ok(LValue::Bool(false)) => eval(alt, env),
        Ok(lv) => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Bool)),
        Err(e) => Err(e),
    }
}
pub fn type_of(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 2 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 2..2))
    }

    match values.get(0).unwrap() {
        LValue::Symbol(s) =>  {
            let lv = values.get(1).unwrap();
            let exp = eval(lv, env)?;
            let sym_type = match &exp {
                LValue::SymType(lst) => lst.clone(),
                LValue::Symbol(s) => match env.get_sym_type(&s) {
                    None => {
                        return Err(WrongType(
                            exp.clone(),
                            exp.into(),
                            NameTypeLValue::SymType,
                        ))
                    }
                    Some(lst) => match lst {
                        LSymType::Type(_) => LSymType::Object(s.into()),
                        lst => return Err(WrongType(s.into(), lst.into(), NameTypeLValue::Type))
                    }
                },
                lv => {
                    return Err(WrongType(
                        lv.clone(),
                        lv.into(),
                        NameTypeLValue::SymType,
                    ))
                }
            };
            env.add_entry(s.to_string(), LValue::Symbol(s.clone()));
            env.add_sym_type(s.clone(), sym_type);
        }
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
    }
    Ok(LValue::None)
}
pub fn print(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    if values.is_empty() {
        return Err(WrongNumberOfArgument(values.into(), 0, 1..std::usize::MAX))
    }
    return Ok(values.get(0).unwrap().clone())
}


pub fn lambda(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 2 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 2..2))
    }
    let params = match values.get(0).unwrap() {
        LValue::List(list) => {
            let mut vec_sym = Vec::new();
            for val in list {
                match val {
                    LValue::Symbol(s) => vec_sym.push(s.clone()),
                    lv  => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
                }
            }
            vec_sym
        }
        lv => return Err(WrongType(lv.clone(),lv.into(), NameTypeLValue::List))
    };
    let body = values.get(1).unwrap();
    Ok(LValue::Lambda(LLambda::new(
        params,
        body.clone(),
        env.clone(),
    )))
}
pub fn quote(values: &[LValue], _env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
    Ok(LValue::Quote(Box::new(values.get(0).unwrap().clone())))
}


pub fn read(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
    let file_name = match values.get(0).unwrap() {
        LValue::Symbol(s) => s.to_string(),
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
    };
    let mut file = match File::open(file_name) {
        Ok(f) => f,
        Err(e) => return Err(SpecialError(e.to_string())),
    };
    let mut buffer = String::new();
    match file.read_to_string(&mut buffer) {
        Ok(_) => {}
        Err(e) => return Err(SpecialError(e.to_string())),
    };
    let lv = parse(buffer.as_str(), env)?;
    eval(&lv, env)

}

pub fn write(values: &[LValue], env: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
    let file_name = match values.get(0).unwrap() {
        LValue::Symbol(s) => s.to_string(),
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
    };
    env.to_file(file_name);
    Ok(LValue::None)
}

///It takes two arguments, an element and a list and returns a list with the element inserted at the first place.
pub fn cons(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 2 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 2..2))
    }
    let first = values.first().unwrap();
    let second = values.get(1).unwrap();
    match (first, second) {
        (lv_first, LValue::List(list)) => {
            let mut new_list = vec![lv_first.clone()];
            new_list.append(&mut list.clone());
            Ok(new_list.into())
        },
        (lv_first, LValue::None) => {
            Ok(lv_first.clone())
        }
        (lv_f, lv_s) => {
            Ok(vec![lv_f.clone(), lv_s.clone()].into())
        },
    }
}

///It takes a list as argument, and returns its first element.
pub fn car(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    if values.len() == 1 {
        match values.first().unwrap() {
            LValue::List(list) =>{
                if list.len() >= 1 {
                    Ok(list.first().unwrap().clone())
                }else {
                    Ok(LValue::None)
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
        }
    }
    else {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
}

///It takes a list as argument, and returns a list without the first element
pub fn cdr(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    if values.len() == 1 {
        match values.first().unwrap() {
            LValue::List(list) =>{
                return if list.len() > 2 {
                    Ok(LValue::None)
                }else {
                    let mut new_list = list.clone();
                    new_list.remove(0);
                    Ok(new_list.into())
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
        }
    }
    else {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
}

///It merges two or more list into one.
pub fn append(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    let mut new_list = Vec::new();
    for element in values {
        match element {
            LValue::List(list) => {
                new_list.append(&mut list.clone())
            },
            _ => return Err(WrongType(element.clone(), element.into(), NameTypeLValue::List))
        }
    }
    Ok(new_list.into())
}

///It takes a list and returns the last element.
pub fn last(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    if values.len() == 1 {
        match values.first().unwrap() {
            LValue::List(list) =>{
                if list.len() >= 1 {
                    Ok(list.last().unwrap().clone())
                }else {
                    Ok(LValue::None)
                }
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
        }
    }
    else {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
}

///It takes two arguments of which the second must be a list,
/// if the first argument is a member of the second argument,
/// and then it returns the remainder of the list beginning with the first argument.
pub fn member(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    if values.len() != 2 {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 2..2))
    }
    let value_to_find = values.get(1).unwrap();
    match values.get(1).unwrap() {
        LValue::List(list) => {
            for (k,element) in list.iter().enumerate() {
               if element == value_to_find {
                   return Ok(list[k-1..].into())
               }
            }
            Ok(LValue::None)
        }
        lv => {
            Err(WrongType(lv.clone(), lv.into(),NameTypeLValue::List))
        }
    }
}

/// It takes a list and returns a list with the top elements in reverse order.
pub fn reverse(values: &[LValue], _: &mut LEnv) -> Result<LValue, LError> {
    if values.len() == 1 {
        match values.first().unwrap() {
            LValue::List(list) =>{
                let mut new_list = list.clone();
                new_list.reverse();
                Ok(new_list.into())
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
        }
    }
    else {
        return Err(WrongNumberOfArgument(values.into(), values.len(), 1..1))
    }
}
