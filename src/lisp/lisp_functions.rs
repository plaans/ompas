//TODO: Vérifier si les fonctions ne doivent prendre que deux paramètres
use crate::lisp::lisp_struct::LispError::*;
use crate::lisp::lisp_struct::*;
use crate::lisp::lisp_language::TYPE_OBJECT;

pub fn get(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        1 => Ok(values[0].clone()),
        len => Err(WrongNumerOfArgument(len, 1..1)),
    }
}



//Mathematical functions
pub fn add(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    let mut result = LispValue::Atom(LispAtom::Number(LispNumber::Float(0.0)));
    for value in values {
        result = (result + value)?;
    }
    Ok(result)
}

pub fn sub(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => {
            let mut first_val: f64 = 0.0;
            let mut second_val: f64 = 0.0;
            for (i, val) in values.iter().enumerate() {
                match val {
                    LispValue::Atom(LispAtom::Number(LispNumber::Int(int))) => match i {
                        0 => first_val = *int as f64,
                        1 => second_val = *int as f64,
                        _ => panic!("Strong error"),
                    },
                    LispValue::Atom(LispAtom::Number(LispNumber::Float(float))) => match i {
                        0 => first_val = *float,
                        1 => second_val = *float,
                        _ => panic!("Strong error"),
                    },
                    lv => {
                        return Err(LispError::WrongType(
                            lv.clone().into(),
                            NameTypeLispValue::NAtom,
                        ))
                    }
                };
            }

            Ok(LispValue::Atom(LispAtom::Number(LispNumber::Float(
                first_val - second_val,
            ))))
        }
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn mul(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    let mut result: f64 = 1.0;
    for value in values {
        match value {
            LispValue::Atom(LispAtom::Number(LispNumber::Int(i))) => result *= i as f64,
            LispValue::Atom(LispAtom::Number(LispNumber::Float(f))) => result *= f,
            l => return Err(LispError::WrongType(l.into(), NameTypeLispValue::NAtom)),
        }
    }
    Ok(LispValue::Atom(LispAtom::Number(LispNumber::Float(result))))
}

pub fn div(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => {
            let mut first_val: f64 = 0.0;
            let mut second_val: f64 = 0.0;
            for (i, val) in values.iter().enumerate() {
                match val {
                    LispValue::Atom(LispAtom::Number(LispNumber::Int(int))) => match i {
                        0 => first_val = *int as f64,
                        1 => second_val = *int as f64,
                        _ => panic!("Strong error"),
                    },
                    LispValue::Atom(LispAtom::Number(LispNumber::Float(float))) => match i {
                        0 => first_val = *float,
                        1 => second_val = *float,
                        _ => panic!("Strong error"),
                    },
                    lv => {
                        return Err(LispError::WrongType(
                            lv.clone().into(),
                            NameTypeLispValue::NAtom,
                        ))
                    }
                };
            }

            Ok(LispValue::Atom(LispAtom::Number(LispNumber::Float(
                first_val / second_val,
            ))))
        }
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

//Comparison functions
pub fn gt(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => Ok(LispValue::Atom(LispAtom::Bool(values[0] > values[1]))),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn lt(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => Ok(LispValue::Atom(LispAtom::Bool(values[0] < values[1]))),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn ge(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => Ok(LispValue::Atom(LispAtom::Bool(values[0] >= values[1]))),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn le(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => Ok(LispValue::Atom(LispAtom::Bool(values[0] <= values[1]))),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn eq(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        2 => Ok(LispValue::Atom(LispAtom::Bool(values[0] == values[1]))),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

//Type verification
pub fn is_none(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        1 => Ok(LispValue::Atom(LispAtom::Bool(
            NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::None,
        ))),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_number(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        1 => Ok(LispValue::Atom(LispAtom::Bool(
            NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::NAtom,
        ))),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_bool(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        1 => Ok(LispValue::Atom(LispAtom::Bool(
            NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::BAtom,
        ))),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_fn(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        1 => Ok(LispValue::Atom(LispAtom::Bool(
            NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::LispFn,
        ))),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn begin(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    Ok(values.last().unwrap().clone())
}

pub fn default(_values: Vec<LispValue>) -> Result<LispValue, LispError> {
    Ok(LispValue::String("default function".to_string()))
}

pub fn var(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    //println!("in function var");
    match values.len() {
        2 => {
            let atom_type = match values[0].clone() {
                LispValue::Type(ltype) => ltype,
                lv => return Err(WrongType(lv.into(), NameTypeLispValue::Type))
            };
            let atom_value:LispAtom = match values[1].clone() {
                LispValue::Variable(lvar) => {
                    if lvar.v_type == atom_type {
                        lvar.value
                    }else {
                        return Err(SpecialError(format!("Expected an atom of type {}, got {}", atom_type,lvar.v_type)))
                    }
                },
                LispValue::Atom(latom) => {
                    latom
                }
                lv => return Err(WrongType(lv.into(), NameTypeLispValue::Variable)),
            };
            Ok(LispValue::Variable(LispVariable {
                v_type: atom_type,
                value: atom_value,
            }))
        }
        len => Err(WrongNumerOfArgument(len, 2..2))
    }
}

pub fn object(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    match values.len() {
        1 => {
            //only the name of the symbol
            let l_value = values[0].clone();
            let t_type = LispType::Symbol(TYPE_OBJECT.into());
            let value = match l_value {
                LispValue::Atom(LispAtom::Symbol(s)) => LispAtom::Symbol(s),
                lv => return Err(WrongType(lv.into(), NameTypeLispValue::SAtom))
            };
            Ok(LispValue::Variable(LispVariable {
                v_type: t_type,
                value
            }))
        }
        2 => {
            //TODO: Add binding to type for object
            let sym_type = values[0].clone();
            let sym_value = values[1].clone();
            let t_type = match sym_type {
                LispValue::Type(LispType::Symbol(s)) => LispType::Symbol(s),
                lv => return Err(WrongType(lv.into(), NameTypeLispValue::Type))
            };
            let value = match sym_value {
                LispValue::Atom(LispAtom::Symbol(s)) => LispAtom::Symbol(s),
                lv => return Err(WrongType(lv.into(), NameTypeLispValue::SAtom))
            };
            Ok(LispValue::Variable(LispVariable {
                v_type: t_type,
                value
            }))
        }
        len => Err(WrongNumerOfArgument(len, 1..2))
    }
}

pub fn state_function(values: Vec<LispValue>) -> Result<LispValue, LispError> {
    let mut vec_params:Vec<LispType> = Vec::new();
    let mut t_value:LispType = LispType::Symbol(TYPE_OBJECT.into());
    for (i,value) in values.iter().enumerate() {
        match value {
            LispValue::Type(ltype) => {
                if i == values.len()-1 {
                    t_value = ltype.clone();
                }
                else {
                    vec_params.push(ltype.clone())
                }
            }
            lv => return Err(WrongType(lv.clone().into(), NameTypeLispValue::Type))
        }
    }
    Ok(LispValue::StateFunction(LispStateFunction {
        t_params: vec_params,
        t_value: t_value
    }))
}
