use std::collections::HashMap;
use crate::lisp::lisp_struct::*;
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_functions::*;

pub mod lisp_functions;
pub mod lisp_language;
pub mod lisp_struct;


pub struct LispEnv {
    pub symbols: HashMap<String, LispValue>,
}

impl Default for LispEnv {
    fn default() -> Self {
        let mut hash_map : HashMap<String, LispValue> = HashMap::default();
        //Mathematical functions
        hash_map.insert(ADD.to_string(), LispValue::LispFn(Box::new(add)));
        hash_map.insert(SUB.to_string(), LispValue::LispFn(Box::new(sub)));
        hash_map.insert(MUL.to_string(), LispValue::LispFn(Box::new(mul)));
        hash_map.insert(DIV.to_string(), LispValue::LispFn(Box::new(div)));
        //Comparison
        hash_map.insert(GT.to_string(), LispValue::LispFn(Box::new(gt)));
        hash_map.insert(LT.to_string(), LispValue::LispFn(Box::new(lt)));
        hash_map.insert(GE.to_string(), LispValue::LispFn(Box::new(ge)));
        hash_map.insert(LE.to_string(), LispValue::LispFn(Box::new(le)));
        hash_map.insert(EQ.to_string(), LispValue::LispFn(Box::new(eq)));

        //Type verification
        hash_map.insert(IS_NONE.to_string(), LispValue::LispFn(Box::new(is_none)));
        hash_map.insert(IS_NUMBER.to_string(), LispValue::LispFn(Box::new(is_number)));
        hash_map.insert(IS_BOOL.to_string(), LispValue::LispFn(Box::new(is_bool)));
        hash_map.insert(IS_FN.to_string(), LispValue::LispFn(Box::new(is_fn)));

        //Special entry
        hash_map.insert(BEGIN.to_string(), LispValue::LispFn(Box::new(begin)));
        hash_map.insert(PI.to_string(), LispValue::Atom(LispAtom::Number(LispNumber::Float(std::f64::consts::PI))));

        //Basic types
        hash_map.insert(TYPE_INT.to_string(), LispValue::Type(LispType::Int));
        hash_map.insert(TYPE_FLOAT.to_string(), LispValue::Type(LispType::Float));
        hash_map.insert(TYPE_BOOL.to_string(), LispValue::Type(LispType::Bool));
        hash_map.insert(TYPE_OBJECT.to_string(), LispValue::Type(LispType::Symbol(TYPE_OBJECT.into())));

        //Functions for the factbase
        hash_map.insert(VARIABLE.to_string(), LispValue::LispFn(Box::new(var)));
        Self {
            symbols: hash_map
        }
    }
}

impl LispEnv {
    pub fn get_symbol(&self, s : String) -> Result<LispValue, LispError> {
        match self.symbols.get(s.as_str()){
            None => Err(LispError::UndefinedSymbol(s)),
            Some(lv) => Ok(lv.clone())
        }
    }
}



//(begin (define ?v (var (:type object
//                        :value bob))