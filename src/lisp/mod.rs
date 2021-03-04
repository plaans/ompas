use crate::lisp::lisp_functions::*;
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::*;
//use std::collections::HashMap;
use im::HashMap;
use std::fs::File;
use std::io::Write;

pub mod lisp_functions;
pub mod lisp_language;
pub mod lisp_struct;

pub struct LEnv {
    symbols: HashMap<String, LValue>,
    new_entries: Vec<String>,
}

impl Default for LEnv {
    fn default() -> Self {
        let mut hash_map: HashMap<String, LValue> = HashMap::default();
        hash_map.insert(GET.to_string(), LValue::LFn(Box::new(get)));

        //Mathematical functions
        hash_map.insert(ADD.to_string(), LValue::LFn(Box::new(add)));
        hash_map.insert(SUB.to_string(), LValue::LFn(Box::new(sub)));
        hash_map.insert(MUL.to_string(), LValue::LFn(Box::new(mul)));
        hash_map.insert(DIV.to_string(), LValue::LFn(Box::new(div)));
        //Comparison
        hash_map.insert(GT.to_string(), LValue::LFn(Box::new(gt)));
        hash_map.insert(LT.to_string(), LValue::LFn(Box::new(lt)));
        hash_map.insert(GE.to_string(), LValue::LFn(Box::new(ge)));
        hash_map.insert(LE.to_string(), LValue::LFn(Box::new(le)));
        hash_map.insert(EQ.to_string(), LValue::LFn(Box::new(eq)));

        //Type verification
        hash_map.insert(IS_NONE.to_string(), LValue::LFn(Box::new(is_none)));
        hash_map.insert(IS_NUMBER.to_string(), LValue::LFn(Box::new(is_number)));
        hash_map.insert(IS_BOOL.to_string(), LValue::LFn(Box::new(is_bool)));
        hash_map.insert(IS_FN.to_string(), LValue::LFn(Box::new(is_fn)));

        //Special entry
        hash_map.insert(BEGIN.to_string(), LValue::LFn(Box::new(begin)));
        hash_map.insert(
            PI.to_string(),
            LValue::Atom(LAtom::Number(LNumber::Float(std::f64::consts::PI))),
        );

        //Basic types
        hash_map.insert(TYPE_INT.to_string(), LValue::Type(LType::Int));
        hash_map.insert(TYPE_FLOAT.to_string(), LValue::Type(LType::Float));
        hash_map.insert(TYPE_BOOL.to_string(), LValue::Type(LType::Bool));
        hash_map.insert(
            TYPE_OBJECT.to_string(),
            LValue::Type(LType::Symbol(TYPE_OBJECT.into())),
        );

        //Functions for the factbase
        hash_map.insert(VARIABLE.to_string(), LValue::LFn(Box::new(var)));
        hash_map.insert(
            STATE_FUNCTION.to_string(),
            LValue::LFn(Box::new(state_function)),
        );
        hash_map.insert(OBJECT.to_string(), LValue::LFn(Box::new(object)));
        hash_map.insert(TYPE.to_string(), LValue::LFn(Box::new(def_type)));
        hash_map.insert(
            STATE_VARIABLE.to_string(),
            LValue::LFn(Box::new(state_variable)),
        );
        hash_map.insert(FACTBASE.to_string(), LValue::LFn(Box::new(factbase)));

        hash_map.insert(SET.to_string(), LValue::LFn(Box::new(set)));

        Self {
            symbols: hash_map,
            new_entries: vec![],
        }
    }
}

impl LEnv {
    pub fn get_symbol(&self, s: String) -> Result<LValue, LError> {
        match self.symbols.get(s.as_str()) {
            None => Err(LError::UndefinedSymbol(s)),
            Some(lv) => Ok(lv.clone()),
        }
    }

    pub fn add_entry(&mut self, sym: String, exp: LValue) {
        self.symbols.insert(sym.clone(), exp);
        self.new_entries.push(sym.clone());
    }

    pub fn get_new_entries(&self) -> Vec<String> {
        self.new_entries.clone()
    }

    pub fn to_file(&self, name_file: String) {
        let mut file = File::create(name_file).unwrap();
        let mut new_entry: Vec<LValue> = vec![];
        let mut string = String::new();
        for entry in &self.new_entries {
            new_entry.push(self.symbols.get(entry.as_str()).unwrap().clone())
        }
        string.push_str("(begin ");
        unsafe {
            for entry in new_entry {
                string.push_str(entry.as_command().as_str())
            }
        }
        string.push_str(")");
        match file.write_all(string.as_bytes()) {
            Ok(_) => {}
            Err(e) => panic!("{}", e),
        }
        //eprintln!("write fact base to file");
    }
}

//(begin (define ?v (var (:type object
//                        :value bob))
