use crate::lisp::lisp_functions::*;
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::*;
//use std::collections::HashMap;
use aries_utils::input::Sym;
use im::HashMap;
use std::borrow::Borrow;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;

pub mod lisp_functions;
pub mod lisp_language;
pub mod lisp_struct;

#[derive(Clone)]
pub struct LEnv {
    symbols: HashMap<String, LValue>,
    sym_types: HashMap<Sym, LSymType>,
    new_entries: Vec<String>,
    outer: Option<Box<LEnv>>,
}

impl PartialEq for LEnv {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Default for LEnv {
    fn default() -> Self {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let mut symbols: HashMap<String, LValue> = HashMap::default();
        let mut sym_types: HashMap<Sym, LSymType> = HashMap::default();

        //Mathematical functions
        symbols.insert(ADD.to_string(), LValue::LFn(Rc::new(add)));
        symbols.insert(SUB.to_string(), LValue::LFn(Rc::new(sub)));
        symbols.insert(MUL.to_string(), LValue::LFn(Rc::new(mul)));
        symbols.insert(DIV.to_string(), LValue::LFn(Rc::new(div)));
        //Comparison
        symbols.insert(GT.to_string(), LValue::LFn(Rc::new(gt)));
        symbols.insert(LT.to_string(), LValue::LFn(Rc::new(lt)));
        symbols.insert(GE.to_string(), LValue::LFn(Rc::new(ge)));
        symbols.insert(LE.to_string(), LValue::LFn(Rc::new(le)));
        symbols.insert(EQ.to_string(), LValue::LFn(Rc::new(eq)));

        //Type verification
        symbols.insert(IS_NONE.to_string(), LValue::LFn(Rc::new(is_none)));
        symbols.insert(IS_NUMBER.to_string(), LValue::LFn(Rc::new(is_number)));
        symbols.insert(IS_BOOL.to_string(), LValue::LFn(Rc::new(is_bool)));
        symbols.insert(IS_FN.to_string(), LValue::LFn(Rc::new(is_fn)));
        symbols.insert(
            IS_STATE_FUNCTION.to_string(),
            LValue::LFn(Rc::new(is_state_function)),
        );
        symbols.insert(IS_OBJECT.to_string(), LValue::LFn(Rc::new(is_object)));
        symbols.insert(IS_TYPE.to_string(), LValue::LFn(Rc::new(is_type)));
        symbols.insert(IS_MAP.to_string(), LValue::LFn(Rc::new(is_map)));
        symbols.insert(IS_LIST.to_string(), LValue::LFn(Rc::new(is_list)));
        symbols.insert(IS_LAMBDA.to_string(), LValue::LFn(Rc::new(is_lambda)));
        symbols.insert(IS_QUOTE.to_string(), LValue::LFn(Rc::new(is_quote)));

        //Special entry
        symbols.insert(BEGIN.to_string(), LValue::LFn(Rc::new(begin)));
        symbols.insert(GET.to_string(), LValue::LFn(Rc::new(get)));
        symbols.insert(SET.to_string(), LValue::LFn(Rc::new(set)));
        symbols.insert(GET_TYPE.to_string(), LValue::LFn(Rc::new(get_type)));

        //Basic types

        //Functions for the factbase
        symbols.insert(
            STATE_FUNCTION.to_string(),
            LValue::LFn(Rc::new(state_function)),
        );
        symbols.insert(SUBTYPE.to_string(), LValue::LFn(Rc::new(subtype)));

        symbols.insert(MAP.to_string(), LValue::LFn(Rc::new(map)));
        symbols.insert(LIST.to_string(), LValue::LFn(Rc::new(list)));
        symbols.insert(STATE.to_string(), LValue::LFn(Rc::new(map)));
        //Sym_types

        sym_types.insert(
            TYPE_INT.into(),
            LSymType::Type(None),
        );
        sym_types.insert(
            TYPE_FLOAT.into(),
            LSymType::Type(None),
        );
        sym_types.insert(
            TYPE_BOOL.into(),
            LSymType::Type(None),
        );
        sym_types.insert(
            TYPE_OBJECT.into(),
            LSymType::Type(None),
        );

        symbols.insert(
            PI.to_string(),
            LValue::Number(LNumber::Float(std::f64::consts::PI)),
        );

        Self {
            symbols,
            sym_types,
            new_entries: vec![],
            outer: None,
        }
    }
}

impl LEnv {
    pub fn find(&self, var: &Sym) -> Option<&Self> {
        match self.symbols.get(var.as_str()) {
            None => match self.outer.borrow() {
                None => None,
                Some(env) => env.find(var),
            },
            Some(_) => Some(self),
        }
    }

    pub fn get_symbol(&self, s: &str) -> Option<LValue> {
        match self.symbols.get(s) {
            None => match self.outer.borrow() {
                None => None,
                Some(env) => env.get_symbol(s),
            },
            Some(v) => Some(v.clone()),
        }
    }

    pub fn get_sym_type(&self, sym: &Sym) -> Option<&LSymType> {
        self.sym_types.get(sym)
    }

    pub fn add_entry(&mut self, key: String, exp: LValue) {
        self.symbols.insert(key.clone(), exp);
        self.new_entries.push(key);
    }

    pub fn add_sym_type(&mut self, sym: Sym, sym_type: LSymType) {
        self.sym_types.insert(sym, sym_type);
    }

    pub fn get_new_entries(&self) -> Vec<String> {
        self.new_entries.clone()
    }

    pub fn to_file(&self, name_file: String) {
        let mut file = File::create(name_file).unwrap();
        let mut string = String::new();
        string.push_str("(begin \n");
        for key in &self.new_entries {
            let value = self.get_symbol(key).unwrap_or(LValue::None);
            match value {
                LValue::Symbol(s) => string.push_str(
                    format!("(typeof {} {})",
                            s,
                            self.get_sym_type(&s).unwrap_or(&LSymType::Object(LType::Object)).as_command())
                        .as_str()),
                lv => string.push_str(format!("(define {} {})", key, lv.as_command()).as_str()),
            }
        }

        string.push(')');
        match file.write_all(string.as_bytes()) {
            Ok(_) => {}
            Err(e) => panic!("{}", e),
        }
        //eprintln!("write fact base to file");
    }
}

//(begin (define ?v (var (:type object
//                        :value bob))
