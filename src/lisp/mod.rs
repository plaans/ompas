use crate::lisp::lisp_functions::*;
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::*;
//use std::collections::HashMap;
use aries_utils::input::Sym;
use im::HashMap;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;

pub mod lisp_functions;
pub mod lisp_language;
pub mod lisp_struct;

pub struct LEnv {
    symbols: HashMap<String, LValue>,
    sym_types: HashMap<Sym, LSymType>,
    new_entries: Vec<String>,
}

impl Default for LEnv {
    fn default() -> Self {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let  symbols: HashMap<String, LValue> = HashMap::default();
        let mut sym_types: HashMap<Sym, LSymType> = HashMap::default();
        let symbols = symbols.update(GET.to_string(), LValue::LFn(Rc::new(get)));

        //Mathematical functions
        let symbols = symbols.update(ADD.to_string(), LValue::LFn(Rc::new(add)));
        let symbols = symbols.update(SUB.to_string(), LValue::LFn(Rc::new(sub)));
        let symbols = symbols.update(MUL.to_string(), LValue::LFn(Rc::new(mul)));
        let symbols = symbols.update(DIV.to_string(), LValue::LFn(Rc::new(div)));
        //Comparison
        let symbols = symbols.update(GT.to_string(), LValue::LFn(Rc::new(gt)));
        let symbols = symbols.update(LT.to_string(), LValue::LFn(Rc::new(lt)));
        let symbols = symbols.update(GE.to_string(), LValue::LFn(Rc::new(ge)));
        let symbols = symbols.update(LE.to_string(), LValue::LFn(Rc::new(le)));
        let symbols = symbols.update(EQ.to_string(), LValue::LFn(Rc::new(eq)));

        //Type verification
        let symbols = symbols.update(IS_NONE.to_string(), LValue::LFn(Rc::new(is_none)));
        let symbols = symbols.update(IS_NUMBER.to_string(), LValue::LFn(Rc::new(is_number)));
        let symbols = symbols.update(IS_BOOL.to_string(), LValue::LFn(Rc::new(is_bool)));
        let symbols = symbols.update(IS_FN.to_string(), LValue::LFn(Rc::new(is_fn)));
        let symbols = symbols.update(IS_FACTBASE.to_string(), LValue::LFn(Rc::new(is_fact_base)));
        let symbols = symbols.update(
            IS_STATE_FUNCTION.to_string(),
            LValue::LFn(Rc::new(is_state_function)),
        );
        let symbols = symbols.update(
            IS_STATE_VARIABLE.to_string(),
            LValue::LFn(Rc::new(is_state_variable)),
        );
        let symbols = symbols.update(IS_OBJECT.to_string(), LValue::LFn(Rc::new(is_object)));
        let symbols = symbols.update(IS_TYPE.to_string(), LValue::LFn(Rc::new(is_type)));
        let symbols = symbols.update(IS_VARIABLE.to_string(), LValue::LFn(Rc::new(is_variable)));

        //Special entry
        let symbols = symbols.update(BEGIN.to_string(), LValue::LFn(Rc::new(begin)));
        let symbols = symbols.update(
            PI.to_string(),
            LValue::Number(LNumber::Float(std::f64::consts::PI)),
        );

        //Basic types

        //Functions for the factbase
        let symbols = symbols.update(VARIABLE.to_string(), LValue::LFn(Rc::new(var)));
        let symbols = symbols.update(
            STATE_FUNCTION.to_string(),
            LValue::LFn(Rc::new(state_function)),
        );
        let symbols = symbols.update(OBJECT.to_string(), LValue::LFn(Rc::new(object)));
        let symbols = symbols.update(TYPE.to_string(), LValue::LFn(Rc::new(def_type)));
        let symbols = symbols.update(
            STATE_VARIABLE.to_string(),
            LValue::LFn(Rc::new(state_variable)),
        );
        let symbols = symbols.update(FACTBASE.to_string(), LValue::LFn(Rc::new(factbase)));

        let symbols = symbols.update(SET.to_string(), LValue::LFn(Rc::new(set)));

        let symbols = symbols.update(STATE.to_string(), LValue::LFn(Rc::new(state)));

        //symbols.insert(LIST.to_string(), LValue::LFn(Rc::new(list)));

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
        }
    }
}

impl LEnv {
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
