use crate::lisp::lisp_functions::*;
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::*;
//use std::collections::HashMap;
use aries_utils::input::Sym;
use im::HashMap;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use std::borrow::Borrow;

pub mod lisp_functions;
pub mod lisp_language;
pub mod lisp_struct;

#[derive(Clone)]
pub struct LEnv {
    symbols: HashMap<String, LValue>,
    sym_types: HashMap<Sym, LSymType>,
    new_entries: Vec<String>,
    outer:  Option<Rc<LEnv>>,
}

//TODO: implement outer

impl Default for LEnv {
    fn default() -> Self {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let mut symbols: HashMap<String, LValue> = HashMap::default();
        let mut sym_types: HashMap<Sym, LSymType> = HashMap::default();
        symbols.insert(GET.to_string(), LValue::LFn(Rc::new(get)));

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
        symbols.insert(IS_VARIABLE.to_string(), LValue::LFn(Rc::new(is_variable)));

        //Special entry
        symbols.insert(BEGIN.to_string(), LValue::LFn(Rc::new(begin)));
        symbols.insert(
            PI.to_string(),
            LValue::Number(LNumber::Float(std::f64::consts::PI)),
        );

        //Basic types

        //Functions for the factbase
        symbols.insert(VARIABLE.to_string(), LValue::LFn(Rc::new(var)));
        symbols.insert(
            STATE_FUNCTION.to_string(),
            LValue::LFn(Rc::new(state_function)),
        );
        symbols.insert(OBJECT.to_string(), LValue::LFn(Rc::new(object)));
        symbols.insert(TYPE.to_string(), LValue::LFn(Rc::new(def_type)));

        symbols.insert(MAP.to_string(), LValue::LFn(Rc::new(map)));
        symbols.insert(SET.to_string(), LValue::LFn(Rc::new(set)));
        symbols.insert(LIST.to_string(), LValue::LFn(Rc::new(list)));
        //Sym_types
        sym_types.insert(
            TYPE_INT.into(),
            LSymType::Type(LType::Symbol(TYPE_ROOT.into())),
        );
        sym_types.insert(
            TYPE_FLOAT.into(),
            LSymType::Type(LType::Symbol(TYPE_ROOT.into())),
        );
        sym_types.insert(
            TYPE_BOOL.into(),
            LSymType::Type(LType::Symbol(TYPE_ROOT.into())),
        );
        sym_types.insert(
            TYPE_OBJECT.into(),
            LSymType::Type(LType::Symbol(TYPE_ROOT.into())),
        );

        Self {
            symbols,
            sym_types,
            new_entries: vec![],
            outer : None
        }
    }
}

impl LEnv {
    pub fn find(&self, var: &Sym) -> Option<&Self> {
        match self.symbols.get(var.as_str()) {
            None => match self.outer.borrow() {
                None => None,
                Some(env) => env.find(var)
            }
            Some(_) => Some(self)
        }
    }

    pub fn get_symbol(&self, s: &str) -> Result<LValue, LError> {
        match self.symbols.get(s) {
            None => Err(LError::UndefinedSymbol(s.to_string())),
            Some(lv) => Ok(lv.clone()),
        }
    }

    pub fn get_sym_type(&self, sym: &Sym) -> LSymType {
        match self.sym_types.get(sym) {
            None => LSymType::Object(LType::Object),
            Some(lst) => lst.clone(),
        }
    }

    pub fn add_entry(&mut self, sym: String, exp: LValue) {
        self.symbols.insert(sym.clone(), exp);
        self.new_entries.push(sym);
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
                LValue::Symbol(s) => {
                    string.push_str(
                        format!("(define {} {})", s, self.get_sym_type(&s).as_command()).as_str(),
                    );
                }
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
