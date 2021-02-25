use std::ops::{Range, Add};
use aries_utils::input::{ErrLoc, Sym};
use std::fmt::{Formatter, Display, Debug};
use aries_planning::parsing::sexpr::SExpr;
use std::cmp::Ordering;

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
pub enum LispType{
    Int,
    Bool,
    Float,
    Symbol(Sym)
}

impl Display for LispType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispType::Int => write!(f, "int"),
            LispType::Bool => write!(f, "bool"),
            LispType::Float => write!(f, "float"),
            LispType::Symbol(s) => write!(f, "{}", s)
        }
    }
}

#[derive(Clone)]
pub enum LispAtom{
    Symbol(Sym),
    Number(LispNumber),
    Bool(bool)
}

impl Display for LispAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispAtom::Symbol(s) => write!(f, "{}", s),
            LispAtom::Number(n) => write!(f, "{}", n),
            LispAtom::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Clone)]
pub struct LispVariable{
    pub v_type : LispType,
    pub value : LispAtom
}

impl Display for LispVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} - {}", self.value, self.v_type)
    }
}

#[derive(Clone)]
pub struct LispStateFunction{
    t_params : Vec<LispType>,
    t_value : LispType,
}

impl Display for LispStateFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut sr = String::new();
        sr.push('(');
        for (i, t_param) in self.t_params.iter().enumerate() {
            sr.push_str(format!("{}", t_param).as_str());
            if i > 0 {
                sr.push(',');
            }
        }
        sr.push_str(format!(") = {}", self.t_value).as_str());
        write!(f,"{}", sr)
    }
}

#[derive(Clone)]
pub struct LispStateVariable{
    params : Vec<LispAtom>,
    value : LispAtom,
}

impl Display for LispStateVariable {
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
        write!(f,"{}", sr)
    }
}

pub type LispFn = Box<fn (Vec<LispValue>) -> Result<LispValue, LispError>>;

#[derive(Clone)]
pub enum LispValue{
    Variable(LispVariable),
    Type(LispType),
    StateFunction(LispStateFunction),
    StateVariable(LispStateVariable),
    Atom(LispAtom),
    String(String),
    SExpr(SExpr),
    LispFn(LispFn),
    None
}

#[derive(Clone)]
pub enum NameTypeLispValue{
    Variable,
    Type,
    StateFunction,
    StateVariable,
    NAtom,
    BAtom,
    SAtom,
    String,
    SExpr,
    LispFn,
    None
}

impl Display for NameTypeLispValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLispValue::Type => "Type",
            NameTypeLispValue::StateFunction => "StateFunction",
            NameTypeLispValue::StateVariable => "StateVariable",
            NameTypeLispValue::NAtom => "Number Atom",
            NameTypeLispValue::BAtom => "Boolean Atom",
            NameTypeLispValue::SAtom => "Symbol Atom",
            NameTypeLispValue::String => "String",
            NameTypeLispValue::SExpr => "SExpr",
            NameTypeLispValue::LispFn => "LispFn",
            NameTypeLispValue::None => "None",
            NameTypeLispValue::Variable => "Variable",
        };
        write!(f, "{}", str)
    }
}

impl PartialEq for NameTypeLispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NameTypeLispValue::String, NameTypeLispValue::String) => true,
            (NameTypeLispValue::SExpr, NameTypeLispValue::SExpr) => true,
            (NameTypeLispValue::BAtom, NameTypeLispValue::BAtom) => true,
            (NameTypeLispValue::NAtom, NameTypeLispValue::NAtom) => true,
            (NameTypeLispValue::SAtom, NameTypeLispValue::SAtom) => true,
            (NameTypeLispValue::LispFn, NameTypeLispValue::LispFn) => true,
            (NameTypeLispValue::None, NameTypeLispValue::None) => true,
            (NameTypeLispValue::StateFunction, NameTypeLispValue::StateFunction) => true,
            (NameTypeLispValue::StateVariable, NameTypeLispValue::StateVariable) => true,
            (NameTypeLispValue::Type, NameTypeLispValue::Type )=> true,
            (_, _) => false
        }
    }
}

impl From<LispValue> for NameTypeLispValue {
    fn from(lv: LispValue) -> Self {
        match lv {
            LispValue::Type(_) => NameTypeLispValue::Type,
            LispValue::StateFunction(_) => NameTypeLispValue::StateFunction,
            LispValue::StateVariable(_) => NameTypeLispValue::StateVariable,
            LispValue::Atom(LispAtom::Number(_))=> NameTypeLispValue::NAtom,
            LispValue::Atom(LispAtom::Bool(_)) =>NameTypeLispValue::BAtom,
            LispValue::Atom(LispAtom::Symbol(_)) => NameTypeLispValue::SAtom,
            LispValue::String(_) => NameTypeLispValue::String,
            LispValue::SExpr(_) => NameTypeLispValue::SExpr,
            LispValue::LispFn(_) => NameTypeLispValue::LispFn,
            LispValue::None => NameTypeLispValue::None,
            LispValue::Variable(_) => NameTypeLispValue::Variable,
        }
    }
}

impl From<LispAtom> for NameTypeLispValue {
    fn from(la: LispAtom) -> Self {
        match la {
            LispAtom::Symbol(_) => NameTypeLispValue::SAtom,
            LispAtom::Number(_) => NameTypeLispValue::NAtom,
            LispAtom::Bool(_) => NameTypeLispValue::BAtom,
        }

    }
}

impl PartialEq for LispAtom {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Int(i2))) => *i1 == *i2,
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Float(f2))) => *f1 == *f2,
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Float(f2))) => *i1 == *f2 as i64,
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Int(i2))) => *f1 as i64 == *i2,
            (LispAtom::Symbol(s1), LispAtom::Symbol(s2)) => *s1 == *s2,
            (LispAtom::Bool(b1), LispAtom::Bool(b2)) => *b1 == *b2,
            _ => false
        }
    }
}

impl PartialOrd for LispAtom {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Int(i2))) => *i1 < *i2,
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Float(f2))) => *f1 < *f2,
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Float(f2))) => *i1 < (*f2 as i64),
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Int(i2))) => (*f1 as i64) < *i2,
            _ => false
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Int(i2))) => *i1 <= *i2,
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Float(f2))) => *f1 <= *f2,
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Float(f2))) => *i1 <= (*f2 as i64),
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Int(i2))) => (*f1 as i64) <= *i2,
            _ => false
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Int(i2))) => *i1 > *i2,
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Float(f2))) => *f1 > *f2,
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Float(f2))) => *i1 > (*f2 as i64),
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Int(i2))) => (*f1 as i64) > *i2,
            _ => false
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Int(i2))) => *i1 >= *i2,
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Float(f2))) => *f1 >= *f2,
            (LispAtom::Number(LispNumber::Int(i1)), LispAtom::Number(LispNumber::Float(f2))) => *i1 >= (*f2 as i64),
            (LispAtom::Number(LispNumber::Float(f1)), LispAtom::Number(LispNumber::Int(i2))) => (*f1 as i64) >= *i2,
            _ => false
        }
    }
}

impl PartialEq for LispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other){
            //bool comparison
            //Number comparison
            (LispValue::Atom(a1), LispValue::Atom(a2)) => a1 == a2,
            //Text comparison
            (LispValue::String(s1), LispValue::String(s2)) => *s1 == *s2,
            //function comparison
            (LispValue::LispFn(fn_1), LispValue::LispFn(fn_2)) => fn_1 == fn_2,

            _ => false
        }
    }

    fn ne(&self, other: &Self) -> bool {
        unimplemented!()
    }
}

impl PartialOrd for LispValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, self) {
            (LispValue::Atom(a1), LispValue::Atom(a2)) => a1 < a2,
            _ => false
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, self) {
            (LispValue::Atom(a1), LispValue::Atom(a2)) => a1 > a2,
            _ => false
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, self) {
            (LispValue::Atom(a1), LispValue::Atom(a2)) => a1 <= a2,
            _ => false
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, self) {
            (LispValue::Atom(a1), LispValue::Atom(a2)) => a1 >= a2,
            _ => false
        }
    }
}

impl Debug for LispValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

impl Display for LispValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LispValue::String(s) => write!(f, "{}",s),
            LispValue::SExpr(s) => write!(f, "{}",s),
            LispValue::LispFn(_) => write!(f, "LispFunction"),
            LispValue::None => write!(f, "None"),
            LispValue::Variable(v) => write!(f, "{}",v),
            LispValue::StateFunction(sf) => write!(f, "{}", sf),
            LispValue::StateVariable(sv) => write!(f, "{}", sv),
            LispValue::Atom(a) => write!(f,"{}", a),
            LispValue::Type(t) => write!(f, "{}", t),
        }
    }
}

impl Add for LispAtom {
    type Output = Result<LispAtom, LispError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LispAtom::Number(LispNumber::Float(f1)),LispAtom::Number(LispNumber::Float(f2))) => Ok(LispAtom::Number(LispNumber::Float(f1+f2))),
            (LispAtom::Number(LispNumber::Int(i1)),LispAtom::Number(LispNumber::Int(i2))) => Ok(LispAtom::Number(LispNumber::Int(i1+i2))),
            (LispAtom::Number(LispNumber::Int(i1)),LispAtom::Number(LispNumber::Float(f2))) => Ok(LispAtom::Number(LispNumber::Float(i1 as f64+f2))),
            (LispAtom::Number(LispNumber::Float(f1)),LispAtom::Number(LispNumber::Int(i2))) => Ok(LispAtom::Number(LispNumber::Float(f1+i2 as f64))),
            (l, LispAtom::Number(_)) => Err(LispError::WrongType(l.into(), NameTypeLispValue::NAtom)),
            (LispAtom::Number(_),l) => Err(LispError::WrongType(l.into(), NameTypeLispValue::NAtom)),
            (l1,l2) => Err(LispError::SpecialError(format!("{} and {} cannot be add",NameTypeLispValue::from(l1), NameTypeLispValue::from(l2))))
        }
    }
}

impl Add for LispValue {
    type Output = Result<LispValue, LispError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LispValue::Atom(a1), LispValue::Atom(a2)) => Ok(LispValue::Atom((a1+a2)?)),
            (LispValue::Atom(_), l) => Err(LispError::WrongType(l.into(), NameTypeLispValue::NAtom)),
            (l, LispValue::Atom(_)) => Err(LispError::WrongType(l.into(), NameTypeLispValue::NAtom)),
            (l1,l2) => Err(LispError::SpecialError(format!("{} and {} cannot be add",NameTypeLispValue::from(l1), NameTypeLispValue::from(l2))))
        }
    }
}