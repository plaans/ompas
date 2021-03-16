use crate::lisp::lisp_language::TYPE_OBJECT;
use crate::lisp::lisp_struct::LError::*;
use crate::lisp::lisp_struct::*;
use aries_utils::input::Sym;
//use std::collections::HashMap;
use crate::lisp::lisp_language::*;
use crate::lisp::LEnv;
use im::HashMap;

//Mathematical functions
pub fn add(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(0.0));
    for value in values {
        result = (&result + value)?;
    }
    Ok(result)
}

pub fn sub(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => values.get(0).unwrap() - values.get(0).unwrap(),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

pub fn mul(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(1.0));
    for value in values {
        result = (&result * value)?;
    }
    Ok(result)
}

pub fn div(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => values.get(0).unwrap() / values.get(0).unwrap(),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

//Comparison functions
pub fn gt(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] > values[1])),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

pub fn lt(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] < values[1])),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

pub fn ge(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] >= values[1])),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

pub fn le(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] <= values[1])),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

pub fn eq(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] == values[1])),
        i => Err(WrongNumberOfArgument(i, 2..2)),
    }
}

//Type verification
pub fn is_none(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::None,
        )),
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_number(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::Number,
        )),
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_bool(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::Bool,
        )),
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_fn(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::LFn,
        )),
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_type(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Type(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_symbol(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_object(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Object(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_state_function(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::StateFunction(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_map(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Map(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}
pub fn is_list(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::List(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_lambda(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Lambda(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn is_quote(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Quote(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false)),
        },
        i => Err(WrongNumberOfArgument(i, 1..1)),
    }
}

pub fn begin(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.last() {
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone()),
    }
}

pub fn default(_values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    Ok(LValue::String("default function".to_string()))
}

pub fn subtype(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.len(), 1..1));
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
                        return Err(WrongType(lst.to_string(), lst.into(), NameTypeLValue::Type))
                    }
                },
            },
        },
        lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol)),
    };
    Ok(LValue::SymType(LSymType::Type(Some(parent_type))))
}

pub fn state_function(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
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
                                value.to_string(),
                                value.into(),
                                NameTypeLValue::Type,
                            ))
                        }
                        Some(lst) => {
                            return Err(WrongType(
                                value.to_string(),
                                lst.into(),
                                NameTypeLValue::Type,
                            ))
                        }
                    }
                }
            }
            lv => {
                return Err(WrongType(
                    lv.to_string(),
                    lv.clone().into(),
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

pub fn list(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    Ok(LValue::List(values.to_vec()))
}

pub fn map(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    let mut facts: HashMap<LValue, LValue> = Default::default();
    for value in values {
        match value {
            LValue::List(l) => {
                if l.len() != 2 {
                    return Err(WrongNumberOfArgument(l.len(), 2..2));
                }
                let key = l.get(0).unwrap();
                let value = l.get(1).unwrap();
                facts.insert(key.clone(), value.clone());
            }
            lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::List)),
        }
    }
    Ok(LValue::Map(facts))
}

//TODO: Define set behaviour for other type of LValue
pub fn set(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    if values.len() < 2 {
        return Err(WrongNumberOfArgument(values.len(), 2..std::usize::MAX));
    }
    match values.get(0).unwrap() {
        LValue::Map(s) => {
            let mut facts = s.clone();
            for value in &values[1..] {
                match value {
                    LValue::List(l) => {
                        if l.len() != 2 {
                            return Err(WrongNumberOfArgument(l.len(), 2..2));
                        }
                        let key = l.get(0).unwrap();
                        let value = l.get(1).unwrap();
                        facts.insert(key.clone(), value.clone());
                    }
                    lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::List)),
                }
            }
            Ok(LValue::Map(facts))
        }
        lv => Err(LError::SpecialError(format!(
            "Cannot set a {}",
            NameTypeLValue::from(lv)
        ))),
    }
}

pub fn get(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    if values.is_empty() {
        return Err(WrongNumberOfArgument(0, 1..std::usize::MAX));
    }

    match values.get(0).unwrap() {
        LValue::Map(map) => {
            if values.len() == 2 {
                let key = values.get(1).unwrap();
                let value = map.get(key).unwrap_or(&LValue::None);
                Ok(value.clone())
            } else if values.len() == 1 {
                Ok(LValue::Map(map.clone()))
            } else {
                Err(WrongNumberOfArgument(values.len(), 1..2))
            }
        }
        lv => {
            if values.len() > 1 {
                return Err(WrongNumberOfArgument(values.len(), 1..1));
            }
            Ok(lv.clone())
        }
    }
}

pub fn get_type(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    if values.len() != 1 {
        return Err(WrongNumberOfArgument(values.len(), 1..1));
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
        lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Atom)),
    };

    Ok(LValue::SymType(sym_type))
}
