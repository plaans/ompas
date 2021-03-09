use crate::lisp::lisp_language::*;
use aries_planning::parsing::sexpr::SExpr;
use aries_utils::input::{ErrLoc, Sym};
use std::cmp::Ordering;
//use std::collections::HashMap;
use crate::lisp::LEnv;
use im::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Range};
use std::rc::Rc;

pub trait AsCommand {
    fn as_command(&self) -> String;
}

pub enum LError {
    WrongType(String, NameTypeLValue, NameTypeLValue),
    WrongNumerOfArgument(usize, Range<usize>),
    ErrLoc(ErrLoc),
    UndefinedSymbol(String),
    SpecialError(String),
    ConversionError(NameTypeLValue, NameTypeLValue),
}

impl Display for LError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LError::WrongType(s, s1, s2) => write!(f, "{}: Got {}, expected {}", s, s1, s2),
            LError::ErrLoc(e) => write!(f, "{}", e),
            LError::UndefinedSymbol(s) => write!(f, "{} is undefined", s),
            LError::WrongNumerOfArgument(s, r) => write!(f, "Got {}, expected {:?}", s, r),
            LError::SpecialError(s) => write!(f, "{}", s),
            LError::ConversionError(s1, s2) => write!(f, "Cannot convert {} into {}.", s1, s2),
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

impl Into<Sym> for &LNumber {
    fn into(self) -> Sym {
        match self {
            LNumber::Int(i) => i.to_string().into(),
            LNumber::Float(f) => f.to_string().into(),
        }
    }
}

impl Into<Sym> for LNumber {
    fn into(self) -> Sym {
        (&self).into()
    }
}

impl LNumber {
    pub fn get_sym_type(&self) -> Sym {
        match self {
            LNumber::Int(_) => TYPE_INT.into(),
            LNumber::Float(_) => TYPE_FLOAT.into(),
        }
    }
}

impl Display for LNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LNumber::Int(i) => write!(f, "{}", i),
            LNumber::Float(fl) => write!(f, "{}", fl),
        }
    }
}

impl PartialEq for LNumber {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LNumber::Int(i1), LNumber::Int(i2)) => *i1 == *i2,
            (LNumber::Float(f1), LNumber::Float(f2)) => *f1 == *f2,
            (LNumber::Int(i1), LNumber::Float(f2)) => *i1 as f64 == *f2,
            (LNumber::Float(f1), LNumber::Int(i2)) => *f1 == *i2 as f64,
        }
    }
}

impl PartialOrd for LNumber {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LNumber::Int(i1), LNumber::Int(i2)) => *i1 < *i2,
            (LNumber::Float(f1), LNumber::Float(f2)) => *f1 < *f2,
            (LNumber::Int(i1), LNumber::Float(f2)) => (*i1 as f64) < *f2,
            (LNumber::Float(f1), LNumber::Int(i2)) => *f1 < *i2 as f64,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LNumber::Int(i1), LNumber::Int(i2)) => *i1 <= *i2,
            (LNumber::Float(f1), LNumber::Float(f2)) => *f1 <= *f2,
            (LNumber::Int(i1), LNumber::Float(f2)) => (*i1 as f64) <= *f2,
            (LNumber::Float(f1), LNumber::Int(i2)) => *f1 <= *i2 as f64,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LNumber::Int(i1), LNumber::Int(i2)) => *i1 > *i2,
            (LNumber::Float(f1), LNumber::Float(f2)) => *f1 > *f2,
            (LNumber::Int(i1), LNumber::Float(f2)) => (*i1 as f64) > *f2,
            (LNumber::Float(f1), LNumber::Int(i2)) => *f1 > *i2 as f64,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LNumber::Int(i1), LNumber::Int(i2)) => *i1 >= *i2,
            (LNumber::Float(f1), LNumber::Float(f2)) => *f1 >= *f2,
            (LNumber::Int(i1), LNumber::Float(f2)) => (*i1 as f64) >= *f2,
            (LNumber::Float(f1), LNumber::Int(i2)) => *f1 >= *i2 as f64,
        }
    }
}

impl Add for &LNumber {
    type Output = LNumber;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 + *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 + *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 + *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 + *i2 as f64),
        }
    }
}

impl Add for LNumber {
    type Output = LNumber;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl Eq for LNumber {}

#[derive(Clone)]
pub enum LType {
    Int,
    Bool,
    Float,
    Object,
    Symbol(Sym),
}

impl From<&LType> for Sym {
    fn from(lt: &LType) -> Self {
        match lt {
            LType::Int => TYPE_INT.into(),
            LType::Bool => TYPE_BOOL.into(),
            LType::Float => TYPE_FLOAT.into(),
            LType::Symbol(s) => s.clone(),
            LType::Object => TYPE_OBJECT.into(),
        }
    }
}

impl From<LType> for Sym {
    fn from(lt: LType) -> Self {
        (&lt).into()
    }
}

impl Display for LType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LType::Int => write!(f, "int"),
            LType::Bool => write!(f, "bool"),
            LType::Float => write!(f, "float"),
            LType::Symbol(s) => write!(f, "{}", s),
            LType::Object => write!(f, "object"),
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
pub struct LVariable {
    pub v_type: Sym,
    pub value: Sym,
}

impl Display for LVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} - {}", self.value, self.v_type)
    }
}

#[derive(Clone)]
pub struct LStateFunction {
    pub t_params: Vec<Sym>,
    pub t_value: Sym,
}

impl Display for LStateFunction {
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
        write!(f, "{}", sr)
    }
}

#[derive(Clone)]
pub struct LStateVariable {
    params: Vec<Sym>,
    value: Sym,
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
    pub fn new(params: Vec<Sym>, value: Sym) -> Self {
        Self { params, value }
    }

    pub fn get_key_value(&self) -> (Vec<Sym>, Sym) {
        (self.params.clone(), self.value.clone())
    }
}

pub type LFn = Rc<fn(&[LValue], &LEnv) -> Result<LValue, LError>>;

#[derive(Clone)]
pub enum LValue {
    Symbol(Sym),
    Number(LNumber),
    Bool(bool),
    FactBase(LFactBase),
    StateVariable(LStateVariable),
    String(String),
    SExpr(SExpr),
    LFn(LFn),
    SymType(LSymType),
    None,
}

impl LValue {
    pub fn as_sym(&self) -> Result<Sym, LError> {
        match self {
            LValue::Symbol(s) => Ok(s.clone()),
            LValue::Bool(b) => Ok(b.to_string().into()),
            LValue::Number(n) => Ok(n.to_string().into()),
            _ => Err(LError::SpecialError(
                "cannot convert into symbol".to_string(),
            )),
        }
    }

    pub fn as_sym_ref(&self) -> Result<&Sym, LError> {
        match self {
            LValue::Symbol(s) => Ok(s),
            _ => Err(LError::SpecialError(
                "cannot convert into symbol ref".to_string(),
            )),
        }
    }

    pub fn as_int(&self) -> Result<i64, LError> {
        match self {
            LValue::Number(LNumber::Int(i)) => Ok(*i),
            LValue::Number(LNumber::Float(f)) => Ok(*f as i64),
            _ => Err(LError::SpecialError("cannot convert into int".to_string())),
        }
    }

    pub fn as_float(&self) -> Result<f64, LError> {
        match self {
            LValue::Number(LNumber::Int(i)) => Ok(*i as f64),
            LValue::Number(LNumber::Float(f)) => Ok(*f),
            _ => Err(LError::SpecialError(
                "cannot convert into float".to_string(),
            )),
        }
    }

    pub fn as_bool(&self) -> Result<bool, LError> {
        match self {
            LValue::Bool(b) => Ok(*b),
            _ => Err(LError::SpecialError("cannot convert into int".to_string())),
        }
    }
}

#[derive(Clone)]
pub enum LSymType {
    StateFunction(LStateFunction),
    Type(LType),
    Variable(LVariable),
    Object(LType),
}

impl From<&LSymType> for NameTypeLValue {
    fn from(lst: &LSymType) -> Self {
        match lst {
            LSymType::StateFunction(_) => NameTypeLValue::StateFunction,
            LSymType::Type(_) => NameTypeLValue::Type,
            LSymType::Variable(_) => NameTypeLValue::Variable,
            LSymType::Object(_) => NameTypeLValue::Object,
        }
    }
}

impl From<LSymType> for NameTypeLValue {
    fn from(lst: LSymType) -> Self {
        (&lst).into()
    }
}

impl LSymType {
    pub fn as_state_function(&self) -> Result<LStateFunction, LError> {
        match self {
            LSymType::StateFunction(sf) => Ok(sf.clone()),
            //TODO: Implement error
            lst => Err(LError::ConversionError(
                lst.into(),
                NameTypeLValue::StateFunction,
            )),
        }
    }

    pub fn as_variable(&self) -> Result<LVariable, LError> {
        match self {
            LSymType::Variable(v) => Ok(v.clone()),
            //TODO: Implement error
            lst => Err(LError::ConversionError(
                lst.into(),
                NameTypeLValue::Variable,
            )),
        }
    }
}

impl Display for LSymType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LSymType::StateFunction(sf) => write!(f, "{}", sf),
            LSymType::Type(t) => write!(f, "{}", t),
            LSymType::Variable(v) => write!(f, "{}", v),
            LSymType::Object(o) => write!(f, "{}", o),
        }
    }
}

#[derive(Clone)]
pub struct LFactBase {
    facts: HashMap<Vec<Sym>, Sym>,
}

impl Display for LFactBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut s = String::new();
        for (key, value) in self.facts.clone() {
            s.push('(');
            for (i, k) in key.iter().enumerate() {
                s.push_str(format!("{}", k).as_str());
                if i < key.len() - 1 {
                    s.push(',')
                }
            }
            s.push_str(format!(") = {}", value).as_str());
        }
        write!(f, "{}", s)
    }
}

impl LFactBase {
    pub fn new(facts: HashMap<Vec<Sym>, Sym>) -> Self {
        LFactBase { facts }
    }

    pub fn get_facts(&self) -> HashMap<Vec<Sym>, Sym> {
        self.facts.clone()
    }
}

#[derive(Clone)]
pub enum NameTypeLValue {
    Variable,
    Type,
    StateFunction,
    StateVariable,
    Number,
    Bool,
    Symbol,
    String,
    SExpr,
    LFn,
    None,
    FactBase,
    Object,
}

impl Display for NameTypeLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLValue::Type => "Type",
            NameTypeLValue::StateFunction => "StateFunction",
            NameTypeLValue::StateVariable => "StateVariable",
            NameTypeLValue::Number => "Number",
            NameTypeLValue::Bool => "Boolean",
            NameTypeLValue::Symbol => "Symbol",
            NameTypeLValue::String => "String",
            NameTypeLValue::SExpr => "SExpr",
            NameTypeLValue::LFn => "LFn",
            NameTypeLValue::None => "None",
            NameTypeLValue::Variable => "Variable",
            NameTypeLValue::FactBase => "FactBase",
            NameTypeLValue::Object => "Object",
        };
        write!(f, "{}", str)
    }
}

impl PartialEq for NameTypeLValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NameTypeLValue::String, NameTypeLValue::String) => true,
            (NameTypeLValue::SExpr, NameTypeLValue::SExpr) => true,
            (NameTypeLValue::Bool, NameTypeLValue::Bool) => true,
            (NameTypeLValue::Symbol, NameTypeLValue::Symbol) => true,
            (NameTypeLValue::LFn, NameTypeLValue::LFn) => true,
            (NameTypeLValue::None, NameTypeLValue::None) => true,
            (NameTypeLValue::StateFunction, NameTypeLValue::StateFunction) => true,
            (NameTypeLValue::StateVariable, NameTypeLValue::StateVariable) => true,
            (NameTypeLValue::Type, NameTypeLValue::Type) => true,
            (_, _) => false,
        }
    }
}

impl From<&LValue> for NameTypeLValue {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::Bool(_) => NameTypeLValue::Bool,
            LValue::Number(_) => NameTypeLValue::Number,
            LValue::Symbol(_) => NameTypeLValue::Symbol,
            LValue::String(_) => NameTypeLValue::String,
            LValue::SExpr(_) => NameTypeLValue::SExpr,
            LValue::LFn(_) => NameTypeLValue::LFn,
            LValue::None => NameTypeLValue::None,
            LValue::FactBase(_) => NameTypeLValue::FactBase,
            LValue::StateVariable(_) => NameTypeLValue::StateVariable,
            LValue::SymType(st) => st.into(),
        }
    }
}

impl From<LValue> for NameTypeLValue {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

impl PartialEq for LValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            //bool comparison
            //Number comparison
            (LValue::Number(n1), LValue::Number(n2)) => *n1 == *n2,
            (LValue::Symbol(s1), LValue::Symbol(s2)) => *s1 == *s2,
            (LValue::Bool(b1), LValue::Bool(b2)) => *b1 == *b2,
            //Text comparison
            (LValue::String(s1), LValue::String(s2)) => *s1 == *s2,
            //function comparison
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
            (LValue::Number(n1), LValue::Number(n2)) => *n1 < *n2,
            _ => false,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 <= *n2,
            _ => false,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 > *n2,
            _ => false,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 >= *n2,
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
            LValue::FactBase(fb) => write!(f, "fb:\n{}", fb),
            LValue::Symbol(s) => write!(f, "sym: {}", s),
            LValue::Number(n) => write!(f, "number: {}", n),
            LValue::Bool(b) => write!(f, "bool: {}", b),
            LValue::SymType(st) => write!(f, "{}", st),
            LValue::StateVariable(sv) => write!(f, "{}", sv),
        }
    }
}

impl Add for &LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 + n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                l.to_string(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                l.to_string(),
                l.into(),
                NameTypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(format!(
                "{} and {} cannot be add",
                NameTypeLValue::from(l1),
                NameTypeLValue::from(l2)
            ))),
        }
    }
}

impl AsCommand for LSymType {
    fn as_command(&self) -> String {
        match self {
            LSymType::StateFunction(sf) => sf.as_command(),
            LSymType::Type(t) => t.as_command(),
            LSymType::Variable(v) => v.as_command(),
            LSymType::Object(o) => {
                format!("(obj {})", o)
            }
        }
    }
}

impl AsCommand for LType {
    fn as_command(&self) -> String {
        match self {
            LType::Symbol(s) => format!("(type {})\n", s),
            _ => "".to_string(),
        }
    }
}

impl AsCommand for LStateFunction {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str("(sf ");
        for t_param in &self.t_params {
            result.push_str(format!("{} ", t_param.to_string()).as_str());
        }
        result.push_str(format!("{})\n", self.t_value.to_string()).as_str());
        result
    }
}

impl AsCommand for LVariable {
    fn as_command(&self) -> String {
        format!("(var {} {})", self.v_type, self.value)
    }
}

impl AsCommand for LStateVariable {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str("(sv ");
        for param in &self.params {
            result.push_str(format!("{} ", param.to_string()).as_str());
        }
        result.push_str(format!("{})\n", self.value.to_string()).as_str());
        result
    }
}

impl AsCommand for LFactBase {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str("(factbase ");
        for (keys, value) in self.facts.iter() {
            result.push_str("(sv ");
            for key in keys {
                result.push_str(format!("{} ", key).as_str())
            }
            result.push_str(format!(" {})", value).as_str());
        }
        result.push_str(")\n");
        result
    }
}

impl AsCommand for LValue {
    fn as_command(&self) -> String {
        match self {
            LValue::FactBase(fb) => fb.as_command(),
            LValue::SymType(st) => st.as_command(),
            LValue::String(s) => s.to_string(),
            LValue::SExpr(s) => s.to_string(),
            LValue::LFn(_) => "".to_string(),
            LValue::None => "".to_string(),
            LValue::Symbol(s) => s.to_string(),
            LValue::Number(n) => n.to_string(),
            LValue::Bool(b) => b.to_string(),
            LValue::StateVariable(sv) => sv.as_command(),
        }
    }
}
