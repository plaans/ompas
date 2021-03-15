//TODO: Vérifier si les fonctions ne doivent prendre que deux paramètres
use crate::lisp::lisp_language::{TYPE_OBJECT};
use crate::lisp::lisp_struct::LError::*;
use crate::lisp::lisp_struct::*;
use aries_utils::input::Sym;
//use std::collections::HashMap;
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
        2 => {
            values.get(0).unwrap() - values.get(0).unwrap()
        }
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
        2 => {
            values.get(0).unwrap() / values.get(0).unwrap()
        }
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

pub fn is_variable(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => match values.get(0).unwrap() {
            LValue::Symbol(s) => match env.sym_types.get(s) {
                None => panic!("symbol as no type"),
                Some(sym_type) => match sym_type {
                    LSymType::Variable(_) => Ok(LValue::Bool(true)),
                    _ => Ok(LValue::Bool(false)),
                },
            },
            lv => Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::Symbol)),
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

//TODO: add verification functions for list, map, ref

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

pub fn begin(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.last() {
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone()),
    }
}
pub fn begins(values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    match values.last() {
        None => Err(LError::SpecialError("no SExpr after begin".to_string())),
        Some(v) => Ok(v.clone()),
    }
}

pub fn default(_values: &[LValue], _env: &LEnv) -> Result<LValue, LError> {
    Ok(LValue::String("default function".to_string()))
}

pub fn var(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    //println!("in function var");
    match values.len() {
        2 => {
            let sym_type = values.get(0).unwrap();
            if !is_type(&values[0..1], env)?.as_bool()? {
                return Err(WrongType(
                    sym_type.to_string(),
                    sym_type.into(),
                    NameTypeLValue::Symbol,
                ));
            }
            let sym_value = values.get(1).unwrap();
            Ok(LValue::SymType(LSymType::Variable(LVariable {
                v_type: sym_type.as_sym()?,
                value: sym_value.as_sym()?,
            })))
        }
        len => Err(WrongNumberOfArgument(len, 2..2)),
    }
}

pub fn object(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => {
            //TODO: Add binding to type for object
            let sym_type = values.get(0).unwrap();
            if is_type(values, env)?.as_bool()? {
                let sym = sym_type.as_sym()?;
                Ok(LValue::SymType(LSymType::Object(LType::Symbol(sym))))
            } else {
                Err(LError::SpecialError("".to_string()))
            }
        }
        len => Err(WrongNumberOfArgument(len, 1..1)),
    }
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
                }
                //TODO::Régler le problème avec les types des symboles
                else {
                    let sym_type = env.sym_types.get(s).unwrap();
                    return Err(WrongType(
                        sym_type.to_string(),
                        sym_type.into(),
                        NameTypeLValue::Type,
                    ));
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

pub fn def_type(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    match values.len() {
        1 => {
            if is_type(values, env)?.as_bool()? {
                Ok(LValue::SymType(LSymType::Type(LType::Symbol(
                    values.get(0).unwrap().as_sym()?,
                ))))
            } else {
                Err(SpecialError("".to_string()))
            }
        }
        len => Err(WrongNumberOfArgument(len, 1..1)),
    }
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
            lv => {
                return Err(WrongType(
                    lv.to_string(),
                    lv.into(),
                    NameTypeLValue::List,
                ))
            }
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
                    lv => {
                        return Err(WrongType(
                            lv.to_string(),
                            lv.into(),
                            NameTypeLValue::List,
                        ))
                    }
                }
            }
            Ok(LValue::Map(facts))
        }
        lv => Err(LError::SpecialError(format!("Cannot set a {}",NameTypeLValue::from(lv)))),
    }
}

pub fn get(values: &[LValue], env: &LEnv) -> Result<LValue, LError> {
    if values.is_empty() {
        return Err(WrongNumberOfArgument(0, 1..std::usize::MAX))
    }

    match values.get(0).unwrap() {
        LValue::Map(map) => {
            if values.len() == 2  {
                let key = values.get(1).unwrap();
                println!("In get for map: key = {}", key);
                let value = map.get(key).unwrap_or(&LValue::None);
                println!("In get for map: value = {}", value);
                Ok(value.clone())
            }
            else if values.len() == 1 {
                Ok(LValue::Map(map.clone()))
            }
            else {
                return Err(WrongNumberOfArgument(values.len(), 1..2))
            }

        }
        lv => {
            if values.len() > 1 {
                return Err(WrongNumberOfArgument(values.len(), 1..1))
            }
            match lv {
                LValue::Symbol(s) => {
                    match env.get_sym_type(s){
                        LSymType::Variable(v) => Ok(v.value.into()),
                        st => Ok(LValue::SymType(st))
                    }
                }
                lv => Ok(lv.clone()),
            }
        }
    }
}

#[cfg(test)]
mod tests
{
    use im::HashMap;
    use crate::lisp::lisp_struct::LValue;
    use std::hash::{Hash, Hasher, BuildHasher};
    use std::collections::hash_map::{DefaultHasher, RandomState};

    //#[test]
    pub fn test_hash_list(){
        let mut map: HashMap<LValue, LValue> = HashMap::new();
        let key = LValue::List(vec![LValue::Symbol("a".into()), LValue::Symbol("b".into())]);
        let value = LValue::Bool(true);
        map.insert(key.clone(), value);
        println!("get value: ");
        match map.get(&key) {
            None => println!("None"),
            Some(v) => println!("value: {}", v)
        }
    }

    //#[test]
    pub fn test_hasher(){
        let map : HashMap<LValue, LValue> = HashMap::new();
        let mut hasher1 = map.hasher().build_hasher();
        let mut hasher2  = map.hasher().build_hasher();
        let key = LValue::List(vec![LValue::Symbol("a".into()), LValue::Symbol("b".into())]);
        let value = LValue::Bool(true);
        key.hash(&mut hasher1);
        println!("hash value : {}", hasher1.finish());
        key.clone().hash(&mut hasher2);
        println!("hash value : {}", hasher2.finish());

    }

    //#[test]
    pub fn test_hash(){
        let mut map : HashMap<LValue, LValue> = HashMap::new();
        let mut hasher1 = map.hasher().build_hasher();
        let key1 = LValue::List(vec![LValue::Symbol("a".into()), LValue::Symbol("b".into())]);
        let key2 = LValue::Bool(true);
        let value = LValue::Bool(true);


        key2.hash(&mut hasher1);
        println!("hash value : {}", hasher1.finish());
        map.insert(key2.clone(), value.clone());
        let result_value =  map.get(&key2.clone()).unwrap_or(&LValue::None);
        println!("value: {}", result_value);
        let mut hasher2 = map.hasher().build_hasher();
        key2.hash(&mut hasher2);
        println!("hash value : {}", hasher2.finish());

        let mut hasher3 = map.hasher().build_hasher();
        key1.hash(&mut hasher3);
        println!("hash value : {}", hasher3.finish());
        map.insert(key1.clone(), value.clone());
        let value =  map.get(&key1).unwrap_or(&LValue::None);
        println!("value: {}", value);
        let mut hasher4 = map.hasher().build_hasher();
        key1.hash(&mut hasher4);
        println!("hash value : {}", hasher4.finish());

        println!("hash map:");
        for (key,value) in map.iter(){
            println!("{} = {}", key, value);
        }


    }

    //#[test]
    pub fn test_hash_with_vec(){
        let mut map: HashMap<Vec<LValue>, i32> = HashMap::new();
        let key = vec![LValue::Bool(true), LValue::Bool(true)];
        let value = 4;
        println!("insert value: ");
        map.insert(key,value);
        let search_key = vec![LValue::Bool(true), LValue::Bool(true)];
        println!("get value: ");
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)

    }
    //#[test]
    pub fn test_hash_with_LValue_List(){
        let mut map: HashMap<LValue, i32> = HashMap::new();
        let key = LValue::List(vec![LValue::Bool(true), LValue::Bool(true)]);
        let value = 4;

        println!("insert value: ");
        map.insert(key,value);
        println!("get value: ");
        let search_key = LValue::List(vec![LValue::Bool(true), LValue::Bool(true)]);
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)

    }
    //#[test]
    pub fn test_hash_with_LValue_Bool(){
        let mut map: HashMap<LValue, i32> = HashMap::new();
        let key = LValue::Bool(true);
        let value = 4;

        println!("insert value: ");
        map.insert(key,value);
        println!("get value: ");
        let search_key = LValue::Bool(true);
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)
    }

    #[test]
    pub fn test_hash_with_LValue_Quote(){
        let mut map: HashMap<LValue, i32> = HashMap::new();
        let key = LValue::Quote(Box::new(LValue::Bool(true)));
        let value = 4;

        println!("insert value: ");
        map.insert(key,value);
        println!("get value: ");
        let search_key = LValue::Quote(Box::new(LValue::Bool(true)));
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)
    }

}

