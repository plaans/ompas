//TODO: Vérifier si les fonctions ne doivent prendre que deux paramètres
use crate::lisp::lisp_struct::LispError::*;
use crate::lisp::lisp_struct::*;

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

pub fn var(_values: Vec<LispValue>) -> Result<LispValue, LispError> {
    Ok(LispValue::Variable(LispVariable {
        v_type: LispType::Bool,
        value: LispAtom::Bool(true),
    }))
}
