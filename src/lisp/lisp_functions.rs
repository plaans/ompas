//TODO: Vérifier si les fonctions ne doivent prendre que deux paramètres
use crate::lisp::lisp_language::{TYPE_OBJECT, TYPE_BOOL};
use crate::lisp::lisp_struct::LError::*;
use crate::lisp::lisp_struct::*;
use aries_utils::input::Sym;
//use std::collections::HashMap;
use im::HashMap;
use crate::lisp::LEnv;

pub fn get(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(values[0].clone()),
        len => Err(WrongNumerOfArgument(len, 1..1)),
    }
}

//Mathematical functions
pub fn add(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(0.0));
    for value in values {
        result = (&result + value)?;
    }
    Ok(result)
}

pub fn sub(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => {
            let mut first_val: f64 = 0.0;
            let mut second_val: f64 = 0.0;
            for (i, val) in values.iter().enumerate() {
                match val {
                    LValue::Number(LNumber::Int(int)) => match i {
                        0 => first_val = *int as f64,
                        1 => second_val = *int as f64,
                        _ => panic!("Strong error"),
                    },
                    LValue::Number(LNumber::Float(float)) => match i {
                        0 => first_val = *float,
                        1 => second_val = *float,
                        _ => panic!("Strong error"),
                    },
                    lv => return Err(LError::WrongType(lv.to_string(), lv.clone().into(), NameTypeLValue::Number)),
                };
            }

            Ok(LValue::Number(LNumber::Float(
                first_val - second_val,
            )))
        }
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn mul(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    let mut result: f64 = 1.0;
    for value in values {
        match value {
            LValue::Number(LNumber::Int(i)) => result *= *i as f64,
            LValue::Number(LNumber::Float(f)) => result *= *f,
            l => return Err(LError::WrongType(l.to_string(), l.into(), NameTypeLValue::Number)),
        }
    }
    Ok(LValue::Number(LNumber::Float(result)))
}

pub fn div(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => {
            let mut first_val: f64 = 0.0;
            let mut second_val: f64 = 0.0;
            for (i, val) in values.iter().enumerate() {
                match val {
                    LValue::Number(LNumber::Int(int)) => match i {
                        0 => first_val = *int as f64,
                        1 => second_val = *int as f64,
                        _ => panic!("Strong error"),
                    },
                    LValue::Number(LNumber::Float(float)) => match i {
                        0 => first_val = *float,
                        1 => second_val = *float,
                        _ => panic!("Strong error"),
                    },
                    lv => return Err(LError::WrongType(lv.to_string(), lv.clone().into(), NameTypeLValue::Number)),
                };
            }

            Ok(LValue::Number(LNumber::Float(
                first_val / second_val,
            )))
        }
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

//Comparison functions
pub fn gt(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] > values[1])),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn lt(values: &[LValue], _env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] < values[1])),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn ge(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] >= values[1])),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn le(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] <= values[1])),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

pub fn eq(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        2 => Ok(LValue::Bool(values[0] == values[1])),
        i => Err(WrongNumerOfArgument(i, 2..2)),
    }
}

//Type verification
pub fn is_none(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::None,
        )),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_number(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::Number,
        )),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_bool(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::Bool,
        )),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_fn(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => Ok(LValue::Bool(
            NameTypeLValue::from(values.get(0).unwrap()) == NameTypeLValue::LFn,
        )),
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_type(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap(){
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Type(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false))
                }
            },
            lv=> Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol))
        }
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_symbol(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap(){
            LValue::Symbol(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false))
        }
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_object(values : &[LValue], env: &LEnv)-> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap(){
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Object(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false))
                }
            },
            lv=> Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol))
        }
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_state_variable(values : &[LValue], _env: &LEnv)-> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap(){
            LValue::StateVariable(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false))
        }
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_state_function(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap(){
            LValue::Symbol(s) => match env.sym_types.get(s.into()) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::StateFunction(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false))
                }
            },
            lv=> Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol))
        }
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn is_fact_base(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap(){
            LValue::FactBase(_) => Ok(LValue::Bool(true)),
            _ => Ok(LValue::Bool(false))
        }
        i => Err(WrongNumerOfArgument(i, 1..1)),
    }
}

pub fn begin(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.last(){
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone())
    }
}
pub fn begins(values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    match values.last(){
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone())
    }
}

pub fn default(_values: &[LValue],_env : &LEnv) -> Result<LValue, LError> {
    Ok(LValue::String("default function".to_string()))
}

pub fn var(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    //println!("in function var");
    match values.len() {
        2 => {
            let sym_type = values.get(0).unwrap();
            if !is_type(&values[0..0], env)?.as_bool()? {
               return Err(WrongType(sym_type.to_string(), sym_type.into(), NameTypeLValue::Symbol))
            }
            let sym_value = values.get(1).unwrap();
            Ok(LValue::SymType(LSymType::Variable(LVariable {
                v_type: sym_type.as_sym()?,
                value: sym_value.as_sym()?
            })))
        }
        len => Err(WrongNumerOfArgument(len, 2..2)),
    }
}

pub fn object(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => {
            //TODO: Add binding to type for object
            let sym_type = values.get(0).unwrap();
            if is_type(values.into(), env)?.as_bool()?{
                let sym = sym_type.as_sym()?;
                Ok(LValue::SymType(LSymType::Object(LType::Symbol(sym))))
            }else {
                Err(LError::SpecialError("".to_string()))
            }

        }
        len => Err(WrongNumerOfArgument(len, 1..1)),
    }
}

pub fn state_function(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    let mut vec_params: Vec<Sym> = Vec::new();
    let mut t_value: Sym = Sym::from(TYPE_OBJECT);
    for (i,value) in values.iter().enumerate() {
        match value {
            LValue::Symbol(s) => {
                if is_type(&values[i..i+1], env)?.as_bool()? {
                    if i == values.len() - 1 {
                        t_value = s.clone();
                    } else {
                        vec_params.push(s.clone())
                    }
                }
                //TODO::Régler le problème avec les types des symboles
                else {
                    let sym_type = env.sym_types.get(s).unwrap();
                    return Err(WrongType(sym_type.to_string(), sym_type.into(), NameTypeLValue::Type))
                }
            }
            lv => return Err(WrongType(lv.to_string(), lv.clone().into(), NameTypeLValue::Type)),
        }
    }
    Ok(LValue::SymType(LSymType::StateFunction(LStateFunction {
        t_params: vec_params,
        t_value: t_value,
    })))
}

pub fn def_type(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => {
            if is_type(values, env)?.as_bool()? {
                Ok(LValue::SymType(LSymType::Type(LType::Symbol(values.get(0).unwrap().as_sym()?))))
            }
            else {
                Err(SpecialError("".to_string()))
            }
        }
        len => Err(WrongNumerOfArgument(len, 1..1)),
    }
}

pub fn state_variable(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    if values.len() < 2 {
        return Err(WrongNumerOfArgument(values.len(), 2..std::usize::MAX))
    }
    let mut params: Vec<Sym> = vec![];
    let mut value: Sym = Sym::from("");
    let mut sf : LStateFunction;

    let sym_sf = values.get(0).unwrap().as_sym_ref()?;
    sf = match env.sym_types.get(sym_sf){
        None => return Err(SpecialError("".to_string())),
        Some(lst) => lst.as_state_function()?
    };
    let n_expected_params = sf.t_params.len()+2;
    if !(n_expected_params == values.len()) {
        return Err(WrongNumerOfArgument(values.len(), n_expected_params..n_expected_params))
    }
    params.push(sym_sf.clone());

    for (i, val) in values[1..].iter().enumerate() {
        if i < values.len() - 2 {
            let mut sym_type:Sym = Sym::from(TYPE_OBJECT);
            let mut sym_value : Sym = Sym::from("");
            match val{
                LValue::Symbol(s) => {
                    match env.get_sym_type(s) {
                        LSymType::Variable(v) => {
                            sym_value = v.value.clone();
                            sym_type = v.v_type.clone();
                        }
                        LSymType::Object(o) => {
                            sym_value = s.clone();
                            sym_type = o.into();
                        }
                        lst => return Err(WrongType(lst.to_string(), lst.into(), NameTypeLValue::Object))
                    }
                }
                LValue::Number(n) => {
                    sym_value = n.into();
                    sym_type = n.get_sym_type();
                }
                LValue::Bool(b) => {
                    sym_value = b.to_string().into();
                    sym_type = Sym::from(TYPE_BOOL);
                }
                lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol))
            }
            if sym_type == sf.t_params[i] {
                params.push(sym_value);
            }
        } else {
            value = val.as_sym()?;
        }
    }

    Ok(LValue::StateVariable(LStateVariable::new(
        params.clone(),
        value,
    )))
}

pub fn factbase(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    let mut facts: HashMap<Vec<Sym>, Sym> = Default::default();
    for value in values {
        match value {
            LValue::StateVariable(sv) => {
                let (key, value) = sv.get_key_value();
                facts.insert(key, value);
            }
            lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::StateVariable)),
        }
    }
    Ok(LValue::FactBase(LFactBase::new(facts)))
}

//TODO: Define set behaviour for other type of LValue
pub fn set(values: &[LValue],env : &LEnv) -> Result<LValue, LError> {
    if values.len() < 2 {
        return Err(WrongNumerOfArgument(values.len(), 2..std::usize::MAX));
    }
    let result = match values.get(0).unwrap() {
        LValue::FactBase(fb) => {
            let mut facts = fb.get_facts();
            for value in &values[1..] {
                match value {
                    LValue::StateVariable(sv) => {
                        let (key, value) = sv.get_key_value();
                        facts.insert(key, value);
                    }
                    lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::StateVariable)),
                }
            }
            LValue::FactBase(LFactBase::new(facts))
        }
        _ => LValue::None
    };

    Ok(result)
}
