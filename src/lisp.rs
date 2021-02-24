use aries_planning::parsing::sexpr::SExpr;
use aries_utils::input::{Sym, ErrLoc};
use std::ptr::hash;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Debug};
use rustyline::config::CompletionType::List;

#[derive(Clone)]
pub enum LispNumber {
    Int(i64),
    Float(f64)
}

impl Display for LispNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(),std::fmt::Error> {
        match self {
            LispNumber::Int(i) => write!(f, "{}", i),
            LispNumber::Float(fl) => write!(f, "{}", fl)
        }
    }
}

#[derive(Clone)]
pub enum LispValue{
    String(String),
    Number(LispNumber),
    SExpr(SExpr),
    Symbol(Sym),
    Bool(bool),
    LispFn(LispFn),
    None
}

#[derive(Clone)]
pub enum NameTypeLispValue{
    String,
    Number,
    SExpr,
    Symbol,
    Bool,
    LispFn,
    None,
}

impl Display for NameTypeLispValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLispValue::String => "String",
            NameTypeLispValue::Number => "Number",
            NameTypeLispValue::SExpr => "SExpr",
            NameTypeLispValue::Symbol => "Symbol",
            NameTypeLispValue::Bool => "Bool",
            NameTypeLispValue::LispFn => "LispFn",
            NameTypeLispValue::None => "None"
        };
        write!(f, "{}", str)
    }
}

impl From<LispValue> for NameTypeLispValue {
    fn from(lv: LispValue) -> Self {
        match lv {
            LispValue::String(_) => NameTypeLispValue::String,
            LispValue::Number(_) => NameTypeLispValue::Number,
            LispValue::SExpr(_) => NameTypeLispValue::SExpr,
            LispValue::Symbol(_) => NameTypeLispValue::Symbol,
            LispValue::Bool(_) => NameTypeLispValue::Bool,
            LispValue::LispFn(_) => NameTypeLispValue::LispFn,
            LispValue::None => NameTypeLispValue::None,
        }
    }
}

impl Debug for LispValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispValue::String(s) => write!(f, "{}",s),
            LispValue::Number(n)=> write!(f, "{}", n),
            LispValue::SExpr(s) => write!(f, "{}",s),
            LispValue::Symbol(s) => write!(f, "{}",s),
            LispValue::Bool(b) => write!(f, "{}",b),
            LispValue::LispFn(_) => write!(f, "LispFunction"),
            LispValue::None => write!(f, "None"),
        }
    }
}

impl Display for LispValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispValue::String(s) => write!(f, "{}",s),
            LispValue::Number(n)=> write!(f, "{}", n),
            LispValue::SExpr(s) => write!(f, "{}",s),
            LispValue::Symbol(s) => write!(f, "{}",s),
            LispValue::Bool(b) => write!(f, "{}",b),
            LispValue::LispFn(_) => write!(f, "LispFunction"),
            LispValue::None => write!(f, "None"),
        }
    }
}

pub enum LispError{
    WrongType(NameTypeLispValue, NameTypeLispValue),
    ErrLoc(ErrLoc),
    UndefinedSymbol(String)
}

impl Display for LispError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispError::WrongType(s1, s2) => write!(f, "expected {}, got {}", s1, s2),
            LispError::ErrLoc(e) => write!(f,"{}", e),
            LispError::UndefinedSymbol(s) => write!(f, "{} is undefined", s )
        }
    }
}

impl From<ErrLoc> for LispError {
    fn from(e: ErrLoc) -> Self {
        LispError::ErrLoc(e)
    }
}

pub struct LispEnv {
    pub symbols: HashMap<String, LispValue>,
}

impl Default for LispEnv {
    fn default() -> Self {
        let mut hash_map : HashMap<String, LispValue> = HashMap::default();
        hash_map.insert("+".to_string(), LispValue::LispFn(Box::new(lisp_functions::add)));
        hash_map.insert("begin".to_string(), LispValue::LispFn(Box::new(lisp_functions::begin)));
        hash_map.insert("pi".to_string(), LispValue::Number(LispNumber::Float(std::f64::consts::PI)));
        hash_map.insert("*".to_string(), LispValue::LispFn(Box::new(lisp_functions::multiply)));
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

pub type LispFn = Box<fn (Vec<LispValue>) -> Result<LispValue, LispError>>;
pub mod lisp_functions {
    use crate::lisp::{LispValue, LispError, LispNumber, NameTypeLispValue};

    pub fn add(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        let mut result:f64 = 0.0;
        for value in values {
            match value {
                LispValue::Number(LispNumber::Int(i)) => result+=i as f64,
                LispValue::Number(LispNumber::Float(f)) => result+=f,
                l => return Err(LispError::WrongType(l.into(), NameTypeLispValue::Number))
            }
        }
        Ok(LispValue::Number(LispNumber::Float(result)))
    }

    pub fn multiply(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        let mut result:f64 = 1.0;
        for value in values {
            match value {
                LispValue::Number(LispNumber::Int(i)) => result*=i as f64,
                LispValue::Number(LispNumber::Float(f)) => result*=f,
                l => return Err(LispError::WrongType(l.into(), NameTypeLispValue::Number))
            }
        }
        Ok(LispValue::Number(LispNumber::Float(result)))
    }

    pub fn begin(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        Ok(values.last().unwrap().clone())
    }

    pub fn default(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        Ok(LispValue::String("default function".to_string()))
    }
}