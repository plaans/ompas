use aries_planning::parsing::sexpr::SExpr;
use aries_utils::input::{Sym, ErrLoc};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Debug};
use rustyline::config::CompletionType::List;
use std::ops::{Range, Add, AddAssign};
use crate::lisp::commands::*;
use crate::lisp::lisp_functions::*;
use crate::lisp::LispError::*;
use crate::lisp::LispNumber::*;
use std::cmp::Ordering;

#[derive(Clone)]
pub enum LispNumber {
    Int(i64),
    Float(f64)
}

impl Display for LispNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(),std::fmt::Error> {
        match self {
            Int(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{}", fl)
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

impl PartialEq for NameTypeLispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NameTypeLispValue::String, NameTypeLispValue::String) => true,
            (NameTypeLispValue::Symbol, NameTypeLispValue::Symbol) => true,
            (NameTypeLispValue::SExpr, NameTypeLispValue::SExpr) => true,
            (NameTypeLispValue::Bool, NameTypeLispValue::Bool) => true,
            (NameTypeLispValue::Number, NameTypeLispValue::Number) => true,
            (NameTypeLispValue::LispFn, NameTypeLispValue::LispFn) => true,
            (NameTypeLispValue::None, NameTypeLispValue::None) => true,
            (_, _) => false
        }
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

impl PartialOrd for LispValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Int(i2))) => *i1 < *i2,
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Float(f2))) => *f1 < *f2,
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Float(f2))) => *i1 < (*f2 as i64),
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Int(i2))) => (*f1 as i64) < *i2,
            _ => false
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Int(i2))) => *i1 <= *i2,
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Float(f2))) => *f1 <= *f2,
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Float(f2))) => *i1 <= (*f2 as i64),
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Int(i2))) => (*f1 as i64) <= *i2,
            _ => false
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Int(i2))) => *i1 > *i2,
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Float(f2))) => *f1 > *f2,
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Float(f2))) => *i1 > (*f2 as i64),
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Int(i2))) => (*f1 as i64) > *i2,
            _ => false
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Int(i2))) => *i1 >= *i2,
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Float(f2))) => *f1 >= *f2,
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Float(f2))) => *i1 >= (*f2 as i64),
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Int(i2))) => (*f1 as i64) >= *i2,
            _ => false
        }
    }
}

impl PartialEq for LispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other){
            //bool comparison
            (LispValue::Bool(b1), LispValue::Bool(b2)) => *b1==*b2,
            //Number comparison
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Int(i2))) => *i1 == *i2,
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Float(f2))) => *f1 == *f2,
            (LispValue::Number(LispNumber::Int(i1)), LispValue::Number(LispNumber::Float(f2))) => *i1 == *f2 as i64,
            (LispValue::Number(LispNumber::Float(f1)), LispValue::Number(LispNumber::Int(i2))) => *f1 as i64 == *i2,
            //Text comparison
            (LispValue::String(s1), LispValue::String(s2)) => *s1 == *s2,
            (LispValue::Symbol(s1), LispValue::Symbol(s2)) => *s1 == *s2,
            (LispValue::String(s1), LispValue::Symbol(s2)) => *s1 == s2.to_string(),
            (LispValue::Symbol(s1), LispValue::String(s2)) => s1.to_string() == *s2,
            //function comparison
            (LispValue::LispFn(fn_1), LispValue::LispFn(fn_2)) => fn_1 == fn_2,

            _ => false
        }
    }

    fn ne(&self, other: &Self) -> bool {
        unimplemented!()
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

impl Add for LispValue {
    type Output = Result<LispValue, LispError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LispValue::Number(LispNumber::Float(f1)),LispValue::Number(LispNumber::Float(f2))) => Ok(LispValue::Number(LispNumber::Float(f1+f2))),
            (LispValue::Number(LispNumber::Int(i1)),LispValue::Number(LispNumber::Int(i2))) => Ok(LispValue::Number(LispNumber::Int(i1+i2))),
            (LispValue::Number(LispNumber::Int(i1)),LispValue::Number(LispNumber::Float(f2))) => Ok(LispValue::Number(LispNumber::Float(i1 as f64+f2))),
            (LispValue::Number(LispNumber::Float(f1)),LispValue::Number(LispNumber::Int(i2))) => Ok(LispValue::Number(LispNumber::Float(f1+i2 as f64))),
            (l, LispValue::Number(_)) => Err(LispError::WrongType(l.into(), NameTypeLispValue::Number)),
            (LispValue::Number(_),l) => Err(LispError::WrongType(l.into(), NameTypeLispValue::Number)),
            (l1,l2) => Err(LispError::SpecialError(format!("{} and {} cannot be add",NameTypeLispValue::from(l1), NameTypeLispValue::from(l2) )))
        }
    }
}

pub enum LispError{
    WrongType(NameTypeLispValue, NameTypeLispValue),
    WrongNumerOfArgument(usize, Range<usize>),
    ErrLoc(ErrLoc),
    UndefinedSymbol(String),
    SpecialError(String),
}

impl Display for LispError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispError::WrongType(s1, s2) => write!(f, "Got {}, expected {}", s1, s2),
            LispError::ErrLoc(e) => write!(f,"{}", e),
            LispError::UndefinedSymbol(s) => write!(f, "{} is undefined", s ),
            LispError::WrongNumerOfArgument(s,r) =>write!(f, "Got {}, expected {:?}", s, r),
            LispError::SpecialError(s) => write!(f,"{}",s)
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

        hash_map.insert(BEGIN.to_string(), LispValue::LispFn(Box::new(begin)));
        hash_map.insert(PI.to_string(), LispValue::Number(LispNumber::Float(std::f64::consts::PI)));
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

pub mod commands {
    //Mathematical functions
    pub const ADD: &str = "+";
    pub const SUB: &str = "-";
    pub const MUL: &str = "*";
    pub const DIV: &str = "/";

    //Comparison
    pub const GT: &str = ">";
    pub const LT: &str = "<";
    pub const GE: &str = ">=";
    pub const LE: &str = "<=";
    pub const EQ: &str = "=";

    //Verification
    pub const IS_NONE: &str = "none?";
    pub const IS_NUMBER: &str = "number?";
    pub const IS_BOOL: &str = "bool?";
    pub const IS_FN: &str = "fn?";

    //Other
    pub const BEGIN:&str = "begin";
    pub const DEFINE:&str = "define";
    pub const IF: &str = "IF";
    pub const PI: &str = "pi";
}

pub type LispFn = Box<fn (Vec<LispValue>) -> Result<LispValue, LispError>>;
pub mod lisp_functions {
    //TODO: Vérifier si les fonctions ne doivent prendre que deux paramètres
    use crate::lisp::{LispValue, LispError, LispNumber, NameTypeLispValue};
    use crate::lisp::LispError::WrongNumerOfArgument;
    use std::ops::Range;
    use crate::fact_base::FactBaseError::WrongType;

    //Mathematical functions
    pub fn add(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        let mut result = LispValue::Number(LispNumber::Float(0.0));
        for value in values {
            result = (result+value)?;
        }
        Ok(result)
    }

    pub fn sub(values:Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len() {
            2 => {
                let mut first_val:f64 = 0.0;
                let mut second_val:f64 = 0.0;
                for (i,val) in values.iter().enumerate() {
                    match val {
                        LispValue::Number(LispNumber::Int(int)) => {
                            match i {
                                0 => first_val = *int as f64,
                                1 => second_val = *int as f64,
                                _ => panic!("Strong error")
                            }
                        }
                        LispValue::Number(LispNumber::Float(float)) => {
                            match i {
                                0 => first_val = *float,
                                1 => second_val = *float,
                                _ => panic!("Strong error")
                            }
                        }
                        lv => return Err(LispError::WrongType(lv.clone().into(), NameTypeLispValue::Number))
                    };
                }

                Ok(LispValue::Number(LispNumber::Float(first_val - second_val)))
            }
            i => Err(WrongNumerOfArgument(i, 2..2))
        }
    }

    pub fn mul(values: Vec<LispValue>) -> Result<LispValue, LispError> {
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

    pub fn div(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len() {
            2 => {
                let mut first_val:f64 = 0.0;
                let mut second_val:f64 = 0.0;
                for (i,val) in values.iter().enumerate() {
                    match val {
                        LispValue::Number(LispNumber::Int(int)) => {
                            match i {
                                0 => first_val = *int as f64,
                                1 => second_val = *int as f64,
                                _ => panic!("Strong error")
                            }
                        }
                        LispValue::Number(LispNumber::Float(float)) => {
                            match i {
                                0 => first_val = *float,
                                1 => second_val = *float,
                                _ => panic!("Strong error")
                            }
                        }
                        lv => return Err(LispError::WrongType(lv.clone().into(), NameTypeLispValue::Number))
                    };
                }

                Ok(LispValue::Number(LispNumber::Float(first_val / second_val)))
            }
            i => Err(WrongNumerOfArgument(i, 2..2))
        }
    }

    //Comparison functions
    pub fn gt(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len(){
            2 => {
                Ok(LispValue::Bool(values[0] > values[1]))
            },
            i => Err(WrongNumerOfArgument(i, 2..2))
        }
    }

    pub fn lt(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len(){
            2 => {
                Ok(LispValue::Bool(values[0] < values[1]))
            },
            i => Err(WrongNumerOfArgument(i, 2..2))
        }    }

    pub fn ge(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len(){
            2 => {
                Ok(LispValue::Bool(values[0] >= values[1]))
            },
            i => Err(WrongNumerOfArgument(i, 2..2))
        }    }

    pub fn le(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len(){
            2 => {
                Ok(LispValue::Bool(values[0] <= values[1]))
            },
            i => Err(WrongNumerOfArgument(i, 2..2))
        }    }

    pub fn eq(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len(){
            2 => {

                Ok(LispValue::Bool(values[0] == values[1]))
            },
            i => Err(WrongNumerOfArgument(i, 2..2))
        }
    }

    //Type verification
    pub fn is_none(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len() {
            1 => Ok(LispValue::Bool(NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::None)),
            i => Err(WrongNumerOfArgument(i, 1..1))
        }
    }

    pub fn is_number(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len() {
            1 => Ok(LispValue::Bool(NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::Number)),
            i => Err(WrongNumerOfArgument(i, 1..1))
        }    }

    pub fn is_bool(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len() {
            1 => Ok(LispValue::Bool(NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::Bool)),
            i => Err(WrongNumerOfArgument(i, 1..1))
        }    }

    pub fn is_fn(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        match values.len() {
            1 => Ok(LispValue::Bool(NameTypeLispValue::from(values[0].clone()) == NameTypeLispValue::LispFn)),
            i => Err(WrongNumerOfArgument(i, 1..1))
        }    }

    pub fn begin(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        Ok(values.last().unwrap().clone())
    }

    pub fn default(values: Vec<LispValue>) -> Result<LispValue, LispError> {
        Ok(LispValue::String("default function".to_string()))
    }
}