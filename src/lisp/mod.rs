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
use aries_planning::parsing::sexpr::SExpr;
use crate::lisp::lisp_struct::LError::{SpecialError, WrongType, WrongNumberOfArgument};

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

        //Logical functions
        symbols.insert(AND.to_string(), LValue::LFn(Rc::new(and)));
        symbols.insert(OR.to_string(), LValue::LFn(Rc::new(or)));
        symbols.insert(NOT.to_string(), LValue::LFn(Rc::new(not)));

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
        symbols.insert(DEFINE.to_ascii_lowercase(), LValue::LFn(Rc::new(define)));
        symbols.insert(IF.to_string(), LValue::LFn(Rc::new(_if)));
        symbols.insert(TYPEOF.to_string(), LValue::LFn(Rc::new(type_of)));
        symbols.insert(READ.to_string(), LValue::LFn(Rc::new(read)));
        symbols.insert(WRITE.to_string(), LValue::LFn(Rc::new(write)));
        //symbols.insert(QUOTE.to_string(), LValue::LFn(Rc::new(quote)));
        symbols.insert(PRINT.to_string(), LValue::LFn(Rc::new(print)));
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

pub fn parse(str : &str, env: &LEnv) -> Result<LValue, LError> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => {
            parse_into_lvalue(&se, env)
        }
        Err(e) => Err(SpecialError(format!("Error in command: {}", e.to_string()))),
    }
}

pub fn parse_into_lvalue(se: &SExpr, env: &LEnv) -> Result<LValue,LError> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            //Test if its an int
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValue::Number(LNumber::Int(int))),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValue::Number(LNumber::Float(float))),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            Ok(LValue::Bool(true))
                        }
                        FALSE => {
                            //println!("atom is boolean false");
                            Ok(LValue::Bool(false))
                        }

                        s => match env.get_symbol(s){
                            None => Ok(LValue::Symbol(s.into())),
                            Some(s) => Ok(s)
                        }
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let mut vec_lvalue = Vec::new();

            for element in list.iter() {
                vec_lvalue.push(parse_into_lvalue(element, env)?);
            }
            match vec_lvalue.get(0) {
                None => {}
                Some(lv)  => if let LValue::Symbol(s) = lv {
                    match s.as_str() {
                        QUOTE => return match vec_lvalue.get(1) {
                            None => Err(SpecialError("expected a lvalue here".to_string())),
                            Some(lv) => Ok(LValue::Quote(Box::new(lv.clone())))
                        },
                        LAMBDA => {
                            if vec_lvalue.len() != 3 {
                                return Err(WrongNumberOfArgument(format!("{:?}", vec_lvalue), vec_lvalue.len(), 3..3))
                            }
                            let mut vec_sym = Vec::new();
                            if let LValue::List(list) = vec_lvalue.get(1).unwrap(){
                                for val in list{
                                    vec_sym.push(val.as_sym()?);
                                }
                            }else{
                                return Err(WrongType(vec_lvalue.get(1).unwrap().to_string(),lv.into(), NameTypeLValue::List))
                            }
                            return Ok(LValue::Lambda(LLambda::new(vec_sym, vec_lvalue.get(2).unwrap().clone(), env.clone())))
                        }
                        _ => {}
                    }
                }
            }
            Ok(LValue::List(vec_lvalue))
        }
    }
}

pub fn eval(lv: &LValue, env: &mut LEnv) -> Result<LValue, LError> {
    match lv {
        LValue::List(list) => {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = list.get(0).unwrap();
            let mut args = Vec::new();
            for arg in &list[1..] {
                args.push(eval(arg, env)?);
            }
            //println!("args:{:?}", args);
            match proc {
                LValue::LFn(f) => return f(args.as_slice(), env),
                LValue::Lambda(l) => {
                    let mut new_env = l.get_new_env(args.as_slice(), env)?;
                    eval(&l.get_body(), &mut new_env)
                }
                lv => Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::LFn))
            }
        }
        LValue::Quote(box_lv) => {
            Ok(*box_lv.clone())
        }
        LValue::Symbol(s) => {
            match env.get_symbol(s.as_str()) {
                None => Ok(lv.clone()),
                Some(lv) => Ok(lv)
            }
        }
        lv => Ok(lv.clone())
    }
}

//(begin (define ?v (var (:type object
//                        :value bob))
