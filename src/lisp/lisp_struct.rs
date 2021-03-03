use aries_planning::parsing::sexpr::SExpr;
use aries_utils::input::{ErrLoc, Sym};
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Range};
use std::collections::HashMap;
use crate::lisp::lisp_language::{OBJECT, TYPE_OBJECT};
use std::hash::Hash;
use std::hash;

pub enum LError {
    WrongType(NameTypeLValue, NameTypeLValue),
    WrongNumerOfArgument(usize, Range<usize>),
    ErrLoc(ErrLoc),
    UndefinedSymbol(String),
    SpecialError(String),
}

impl Display for LError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LError::WrongType(s1, s2) => write!(f, "Got {}, expected {}", s1, s2),
            LError::ErrLoc(e) => write!(f, "{}", e),
            LError::UndefinedSymbol(s) => write!(f, "{} is undefined", s),
            LError::WrongNumerOfArgument(s, r) => write!(f, "Got {}, expected {:?}", s, r),
            LError::SpecialError(s) => write!(f, "{}", s),
        }
    }
}

impl From<ErrLoc> for LError {
    fn from(e: ErrLoc) -> Self {
        LError::ErrLoc(e)
    }
}

#[derive(Clone)]
pub enum LNumber {
    Int(i64),
    Float(f64),
}

impl Display for LNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LNumber::Int(i) => write!(f, "{}", i),
            LNumber::Float(fl) => write!(f, "{}", fl),
        }
    }
}

#[derive(Clone)]
pub enum LType {
    Int,
    Bool,
    Float,
    Symbol(Sym),
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LType::Int => write!(f, "int"),
            LType::Bool => write!(f, "bool"),
            LType::Float => write!(f, "float"),
            LType::Symbol(s) => write!(f, "{}", s),
        }
    }
}

impl PartialEq for LType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LType::Int, LType::Int) => true,
            (LType::Bool, LType::Bool) => true,
            (LType::Float, LType::Float) => true,
            (LType::Symbol(s1), LType::Symbol(s2)) => s1 == s2,
            (_, _) => false,
        }
    }
}

#[derive(Clone)]
pub enum LAtom {
    Symbol(Sym),
    Number(LNumber),
    Bool(bool),
}

impl Into<LType> for LAtom {
    fn into(self) -> LType {
        match self {
            LAtom::Symbol(_) => LType::Symbol(TYPE_OBJECT.into()),
            LAtom::Number(LNumber::Float(_)) => LType::Float,
            LAtom::Number(LNumber::Int(_)) => LType::Int,
            LAtom::Bool(_) => LType::Bool,
        }
    }
}

impl Display for LAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LAtom::Symbol(s) => write!(f, "{}", s),
            LAtom::Number(n) => write!(f, "{}", n),
            LAtom::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone)]
pub struct LVariable {
    pub v_type: LType,
    pub value: LAtom,
}

impl Display for LVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} - {}", self.value, self.v_type)
    }
}

#[derive(Clone)]
pub struct LStateFunction {
    pub label : Sym,
    pub t_params: Vec<LType>,
    pub t_value: LType,
}

impl Display for LStateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut sr = String::new();
        sr.push_str(format!("{} (", self.label).as_str());
        for (i, t_param) in self.t_params.iter().enumerate() {
            sr.push_str(format!("{}", t_param).as_str());
            if i > 0 {
                sr.push(',');
            }
        }
        sr.push_str(format!(") = {}", self.t_value).as_str());
        write!(f, "{}", sr)
    }
}

#[derive(Clone)]
pub struct LStateVariable {
    params: Vec<LAtom>,
    value: LAtom,
}

impl Display for LStateVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut sr = String::new();
        sr.push('(');
        for (i, t_param) in self.params.iter().enumerate() {
            sr.push_str(format!("{}", t_param).as_str());
            if i > 0 {
                sr.push(',');
            }
        }
        sr.push_str(format!(") = {}", self.value).as_str());
        write!(f, "{}", sr)
    }
}

impl LStateVariable {
    pub fn new(params: Vec<LAtom>, value: LAtom) -> Self {
        Self {
            params,
            value
        }
    }

    pub fn get_key_value(&self) -> (Vec<LAtom>,LAtom){
        (self.params.clone(), self.value.clone())
    }
}

pub type LFn = Box<fn(Vec<LValue>) -> Result<LValue, LError>>;

#[derive(Clone)]
pub enum LValue {
    FactBase(LFactBase),
    Variable(LVariable),
    Type(LType),
    StateFunction(LStateFunction),
    StateVariable(LStateVariable),
    Atom(LAtom),
    String(String),
    SExpr(SExpr),
    LFn(LFn),
    None,
}

#[derive(Clone)]
pub struct LFactBase {
    facts : HashMap<Vec<LAtom>, LAtom>,
}

impl Display for LFactBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut s = String::new();
        for (key, value) in self.facts.clone() {
            s.push('(');
            for (i,k) in key.iter().enumerate() {
                s.push_str(format!("{}", k).as_str());
                if i < key.len()-1 {
                    s.push(',')
                }
            }
            s.push_str(format!(") = {}", value).as_str());
        };
        write!(f, "{}", s)
    }
}

impl LFactBase {
    pub fn new(facts: HashMap<Vec<LAtom>, LAtom>) -> Self {
        LFactBase {
            facts
        }
    }

    pub fn get_facts(&self) -> HashMap<Vec<LAtom>, LAtom> {
        self.facts.clone()
    }
}

#[derive(Clone)]
pub enum NameTypeLValue {
    Atom,
    Variable,
    Type,
    StateFunction,
    StateVariable,
    NAtom,
    BAtom,
    SAtom,
    String,
    SExpr,
    LFn,
    None,
    FactBase
}

impl Display for NameTypeLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLValue::Type => "Type",
            NameTypeLValue::StateFunction => "StateFunction",
            NameTypeLValue::StateVariable => "StateVariable",
            NameTypeLValue::NAtom => "Number Atom",
            NameTypeLValue::BAtom => "Boolean Atom",
            NameTypeLValue::SAtom => "Symbol Atom",
            NameTypeLValue::String => "String",
            NameTypeLValue::SExpr => "SExpr",
            NameTypeLValue::LFn => "LFn",
            NameTypeLValue::None => "None",
            NameTypeLValue::Variable => "Variable",
            NameTypeLValue::FactBase => "FactBase",
            NameTypeLValue::Atom => "Atom",
        };
        write!(f, "{}", str)
    }
}

impl PartialEq for NameTypeLValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NameTypeLValue::String, NameTypeLValue::String) => true,
            (NameTypeLValue::SExpr, NameTypeLValue::SExpr) => true,
            (NameTypeLValue::BAtom, NameTypeLValue::BAtom) => true,
            (NameTypeLValue::NAtom, NameTypeLValue::NAtom) => true,
            (NameTypeLValue::SAtom, NameTypeLValue::SAtom) => true,
            (NameTypeLValue::LFn, NameTypeLValue::LFn) => true,
            (NameTypeLValue::None, NameTypeLValue::None) => true,
            (NameTypeLValue::StateFunction, NameTypeLValue::StateFunction) => true,
            (NameTypeLValue::StateVariable, NameTypeLValue::StateVariable) => true,
            (NameTypeLValue::Type, NameTypeLValue::Type) => true,
            (_, _) => false,
        }
    }
}

impl From<LValue> for NameTypeLValue {
    fn from(lv: LValue) -> Self {
        match lv {
            LValue::Type(_) => NameTypeLValue::Type,
            LValue::StateFunction(_) => NameTypeLValue::StateFunction,
            LValue::StateVariable(_) => NameTypeLValue::StateVariable,
            LValue::Atom(LAtom::Number(_)) => NameTypeLValue::NAtom,
            LValue::Atom(LAtom::Bool(_)) => NameTypeLValue::BAtom,
            LValue::Atom(LAtom::Symbol(_)) => NameTypeLValue::SAtom,
            LValue::String(_) => NameTypeLValue::String,
            LValue::SExpr(_) => NameTypeLValue::SExpr,
            LValue::LFn(_) => NameTypeLValue::LFn,
            LValue::None => NameTypeLValue::None,
            LValue::Variable(_) => NameTypeLValue::Variable,
            LValue::FactBase(_) => NameTypeLValue::FactBase,
        }
    }
}

impl From<LAtom> for NameTypeLValue {
    fn from(la: LAtom) -> Self {
        match la {
            LAtom::Symbol(_) => NameTypeLValue::SAtom,
            LAtom::Number(_) => NameTypeLValue::NAtom,
            LAtom::Bool(_) => NameTypeLValue::BAtom,
        }
    }
}

impl PartialEq for LAtom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Int(i2))) => {
                *i1 == *i2
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Float(f2))) => {
                *f1 == *f2
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Float(f2))) => {
                *i1 == *f2 as i64
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Int(i2))) => {
                *f1 as i64 == *i2
            }
            (LAtom::Symbol(s1), LAtom::Symbol(s2)) => *s1 == *s2,
            (LAtom::Bool(b1), LAtom::Bool(b2)) => *b1 == *b2,
            _ => false,
        }
    }
}

impl Hash for LAtom {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            LAtom::Symbol(s) => s.hash(state),
            LAtom::Number(LNumber::Float(f)) => format!("{}",f).hash(state),
            LAtom::Number(LNumber::Int(i)) => i.hash(state),
            LAtom::Bool(b) => b.hash(state)
        }

    }
}

impl std::cmp::Eq for LAtom {

}

impl PartialOrd for LAtom {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Int(i2))) => {
                *i1 < *i2
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Float(f2))) => {
                *f1 < *f2
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Float(f2))) => {
                *i1 < (*f2 as i64)
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Int(i2))) => {
                (*f1 as i64) < *i2
            }
            _ => false,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Int(i2))) => {
                *i1 <= *i2
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Float(f2))) => {
                *f1 <= *f2
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Float(f2))) => {
                *i1 <= (*f2 as i64)
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Int(i2))) => {
                (*f1 as i64) <= *i2
            }
            _ => false,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Int(i2))) => {
                *i1 > *i2
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Float(f2))) => {
                *f1 > *f2
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Float(f2))) => {
                *i1 > (*f2 as i64)
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Int(i2))) => {
                (*f1 as i64) > *i2
            }
            _ => false,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Int(i2))) => {
                *i1 >= *i2
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Float(f2))) => {
                *f1 >= *f2
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Float(f2))) => {
                *i1 >= (*f2 as i64)
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Int(i2))) => {
                (*f1 as i64) >= *i2
            }
            _ => false,
        }
    }
}

impl PartialEq for LValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            //bool comparison
            //Number comparison
            (LValue::Atom(a1), LValue::Atom(a2)) => a1 == a2,
            //Text comparison
            (LValue::String(s1), LValue::String(s2)) => *s1 == *s2,
            //function comparison
            (LValue::LFn(fn_1), LValue::LFn(fn_2)) => fn_1 == fn_2,

            _ => false,
        }
    }
}

impl PartialOrd for LValue {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Atom(a1), LValue::Atom(a2)) => a1 < a2,
            _ => false,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Atom(a1), LValue::Atom(a2)) => a1 <= a2,
            _ => false,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Atom(a1), LValue::Atom(a2)) => a1 > a2,
            _ => false,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Atom(a1), LValue::Atom(a2)) => a1 >= a2,
            _ => false,
        }
    }
}

impl Debug for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::String(s) => write!(f, "string: {}", s),
            LValue::SExpr(s) => write!(f, "sexpr: {}", s),
            LValue::LFn(_) => write!(f, "LFunction"),
            LValue::None => write!(f, "None"),
            LValue::Variable(v) => write!(f, "{}", v),
            LValue::StateFunction(sf) => write!(f, "sf: {}", sf),
            LValue::StateVariable(sv) => write!(f, "sv: {}", sv),
            LValue::Atom(a) => write!(f, "atom: {}", a),
            LValue::Type(t) => write!(f, "type: {}", t),
            LValue::FactBase(fb) => write!(f, "fb:\n{}", fb)
        }
    }
}

impl Add for LAtom {
    type Output = Result<LAtom, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Float(f2))) => {
                Ok(LAtom::Number(LNumber::Float(f1 + f2)))
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Int(i2))) => {
                Ok(LAtom::Number(LNumber::Int(i1 + i2)))
            }
            (LAtom::Number(LNumber::Int(i1)), LAtom::Number(LNumber::Float(f2))) => {
                Ok(LAtom::Number(LNumber::Float(i1 as f64 + f2)))
            }
            (LAtom::Number(LNumber::Float(f1)), LAtom::Number(LNumber::Int(i2))) => {
                Ok(LAtom::Number(LNumber::Float(f1 + i2 as f64)))
            }
            (l, LAtom::Number(_)) => {
                Err(LError::WrongType(l.into(), NameTypeLValue::NAtom))
            }
            (LAtom::Number(_), l) => {
                Err(LError::WrongType(l.into(), NameTypeLValue::NAtom))
            }
            (l1, l2) => Err(LError::SpecialError(format!(
                "{} and {} cannot be add",
                NameTypeLValue::from(l1),
                NameTypeLValue::from(l2)
            ))),
        }
    }
}

impl Add for LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Atom(a1), LValue::Atom(a2)) => Ok(LValue::Atom((a1 + a2)?)),
            (LValue::Atom(a1), LValue::Variable(v2)) => {
                match v2.v_type {
                    LType::Int => Ok(LValue::Atom((a1+v2.value)?)),
                    LType::Bool => Err(LError::WrongType(NameTypeLValue::BAtom, NameTypeLValue::NAtom)),
                    LType::Float => Ok(LValue::Atom((a1+v2.value)?)),
                    LType::Symbol(_) => Err(LError::WrongType(NameTypeLValue::SAtom, NameTypeLValue::NAtom)),
                }
            }
            ( LValue::Variable(v1), LValue::Atom(a2)) => {
                match v1.v_type {
                    LType::Int => Ok(LValue::Atom((a2+v1.value)?)),
                    LType::Bool => Err(LError::WrongType(NameTypeLValue::BAtom, NameTypeLValue::NAtom)),
                    LType::Float => Ok(LValue::Atom((a2+v1.value)?)),
                    LType::Symbol(_) => Err(LError::WrongType(NameTypeLValue::SAtom, NameTypeLValue::NAtom)),
                }
            }
            (LValue::Atom(_), l) => {
                Err(LError::WrongType(l.into(), NameTypeLValue::NAtom))
            }
            (l, LValue::Atom(_)) => {
                Err(LError::WrongType(l.into(), NameTypeLValue::NAtom))
            }

            (l1, l2) => Err(LError::SpecialError(format!(
                "{} and {} cannot be add",
                NameTypeLValue::from(l1),
                NameTypeLValue::from(l2)
            ))),
        }
    }
}
