use crate::lisp::lisp_language::*;
use aries_utils::input::{ErrLoc, Sym};
use std::cmp::Ordering;
//use std::collections::HashMap;
use crate::lisp::lisp_struct::LError::WrongNumberOfArgument;
use crate::lisp::{eval, LEnv};
use im::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Range, Sub};
use std::rc::Rc;

//TODO: define MutLfn

pub enum LError {
    WrongType(LValue, NameTypeLValue, NameTypeLValue),
    NotInListOfExpectedTypes(LValue, NameTypeLValue, Vec<NameTypeLValue>),
    WrongNumberOfArgument(LValue, usize, Range<usize>),
    ErrLoc(ErrLoc),
    UndefinedSymbol(String),
    SpecialError(String),
    ConversionError(NameTypeLValue, NameTypeLValue),
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

impl Hash for LNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LNumber::Int(i) => i.hash(state),
            LNumber::Float(f) => f.to_string().hash(state),
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

impl Sub for &LNumber {
    type Output = LNumber;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 - *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 - *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 - *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 - *i2 as f64),
        }
    }
}

impl Div for &LNumber {
    type Output = LNumber;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 / *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 / *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 / *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 / *i2 as f64),
        }
    }
}

impl Mul for &LNumber {
    type Output = LNumber;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LNumber::Int(i1), LNumber::Int(i2)) => LNumber::Int(*i1 * *i2),
            (LNumber::Float(f1), LNumber::Float(f2)) => LNumber::Float(*f1 * *f2),
            (LNumber::Int(i1), LNumber::Float(f2)) => LNumber::Float(*i1 as f64 * *f2),
            (LNumber::Float(f1), LNumber::Int(i2)) => LNumber::Float(*f1 * *i2 as f64),
        }
    }
}

impl Add for LNumber {
    type Output = LNumber;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}
impl Sub for LNumber {
    type Output = LNumber;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}
impl Mul for LNumber {
    type Output = LNumber;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Div for LNumber {
    type Output = LNumber;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
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

impl From<&str> for LType {
    fn from(s: &str) -> Self {
        match s {
            TYPE_INT => LType::Int,
            TYPE_FLOAT => LType::Float,
            TYPE_OBJECT => LType::Object,
            TYPE_BOOL => LType::Bool,
            other => LType::Symbol(other.into()),
        }
    }
}

impl From<&Sym> for LType {
    fn from(s: &Sym) -> Self {
        s.as_str().into()
    }
}

impl From<Sym> for LType {
    fn from(s: Sym) -> Self {
        (&s).into()
    }
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

#[derive(Clone, PartialEq)]
pub struct LStateFunction {
    pub t_params: Vec<Sym>,
    pub t_value: Sym,
}

#[derive(Clone)]
pub enum LSymType {
    StateFunction(LStateFunction),
    Type(Option<LType>),
    Object(LType),
}

impl LSymType {
    pub fn as_state_function(&self) -> Result<LStateFunction, LError> {
        match self {
            LSymType::StateFunction(sf) => Ok(sf.clone()),
            lst => Err(LError::ConversionError(
                lst.into(),
                NameTypeLValue::StateFunction,
            )),
        }
    }
}

impl PartialEq for &LSymType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LSymType::Object(o1), LSymType::Object(o2)) => o1 == o2,
            (LSymType::Type(t1), LSymType::Type(t2)) => t1 == t2,
            (LSymType::StateFunction(sf1), LSymType::StateFunction(sf2)) => sf1 == sf2,
            _ => false,
        }
    }
}

type PLFn = Rc<fn(&[LValue], &mut LEnv) -> Result<LValue, LError>>;

#[derive(Clone)]
pub struct LFn {
    pub pointer: PLFn,
    pub label: String,
}

#[derive(Clone)]
pub struct LLambda {
    params: Vec<Sym>,
    body: Box<LValue>,
    env: LEnv,
}

impl PartialEq for LLambda {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl LLambda {
    pub fn new(params: Vec<Sym>, body: LValue, env: LEnv) -> Self {
        LLambda {
            params,
            body: Box::new(body),
            env,
        }
    }

    pub fn get_new_env(&self, args: &[LValue], outer: &LEnv) -> Result<LEnv, LError> {
        if self.params.len() != args.len() {
            return Err(WrongNumberOfArgument(
                LValue::List(args.to_vec()),
                args.len(),
                self.params.len()..self.params.len(),
            ));
        }
        let mut env = self.env.clone();
        for (param, arg) in self.params.iter().zip(args) {
            env.symbols.insert(param.to_string(), arg.clone());
        }
        env.outer = Some(Box::new(outer.clone()));
        Ok(env)
    }

    pub fn call(&self, args: &[LValue], outer: &LEnv) -> Result<LValue, LError> {
        let mut new_env = self.get_new_env(args, outer)?;
        eval(&*self.body, &mut new_env)
    }

    pub fn get_body(&self) -> LValue {
        *self.body.clone()
    }
}

#[derive(Clone, PartialOrd, PartialEq, Eq)]
pub enum LCoreOperator {
    Define,
    DefLambda,
    If,
    Quote,
    QuasiQuote,
    UnQuote,
    DefMacro,
    Set,
    Begin,
}

#[derive(Clone)]
pub enum LValue {
    // symbol
    Symbol(Sym),
    // literaux
    Number(LNumber),
    Bool(bool),
    String(String),

    // data structure
    Map(im::HashMap<LValue, LValue>),
    List(Vec<LValue>),
    Quote(Box<LValue>),
    // error
    None,
    LFn(LFn),
    Lambda(LLambda),
    CoreOperator(LCoreOperator),
    SymType(LSymType),
}

impl Hash for LValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValue::Symbol(s) => (*s).hash(state),
            LValue::Number(n) => (*n).hash(state),
            LValue::Bool(b) => (*b).hash(state),
            LValue::String(s) => (*s).hash(state),
            LValue::Map(m) => (*m).hash(state),
            LValue::List(l) => {
                (*l).hash(state);
            }
            LValue::Quote(s) => {
                s.to_string().hash(state);
            }

            LValue::None => "none".hash(state),
            _ => {}
        };
    }
}

impl Eq for LValue {}

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
            _ => Err(LError::SpecialError("cannot convert into bool".to_string())),
        }
    }

    pub fn as_core_operator(&self) -> Result<LCoreOperator, LError> {
        match self {
            LValue::CoreOperator(co) => Ok(co.clone()),
            _ => Err(LError::SpecialError(
                "cannot convert into core operator".to_string(),
            )),
        }
    }

    pub fn as_lambda(&self) -> Result<LLambda, LError> {
        match self {
            LValue::Lambda(l) => Ok(l.clone()),
            _ => Err(LError::SpecialError(
                "cannot convert into lambda".to_string(),
            )),
        }
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
            (LValue::None, LValue::None) => true,
            (LValue::List(l1), LValue::List(l2)) => *l1 == *l2,
            (LValue::Map(m1), LValue::Map(m2)) => *m1 == *m2,
            (LValue::Lambda(l1), LValue::Lambda(l2)) => *l1 == *l2,
            (LValue::Quote(q1), LValue::Quote(q2)) => q1.to_string() == q2.to_string(), //function comparison
            (LValue::LFn(f1), LValue::LFn(f2)) => f1.label == f2.label,
            (LValue::SymType(s1), LValue::SymType(s2)) => s1 == s2,
            (_, _) => false,
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

impl Add for &LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 + n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                l.clone(),
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

impl Sub for &LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 - n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                l.clone(),
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

impl Mul for &LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 * n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                l.clone(),
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

impl Div for &LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 / n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                l.clone(),
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

impl Add for LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}
impl Sub for LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}
impl Mul for LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}
impl Div for LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
    }
}

#[derive(Clone, Debug)]
pub enum NameTypeLValue {
    CoreOperator,
    Atom,
    SymType,
    Object,
    Type,
    StateFunction,
    Number,
    Bool,
    Symbol,
    String,
    SExpr,
    LFn,
    Lambda,
    None,
    FactBase,
    Map,
    List,
    Quote,
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
            (NameTypeLValue::Type, NameTypeLValue::Type) => true,
            (NameTypeLValue::Number, NameTypeLValue::Number) => true,
            (NameTypeLValue::FactBase, NameTypeLValue::FactBase) => true,
            (NameTypeLValue::Object, NameTypeLValue::Object) => true,
            (NameTypeLValue::Map, NameTypeLValue::Map) => true,
            (NameTypeLValue::List, NameTypeLValue::List) => true,
            (NameTypeLValue::Quote, NameTypeLValue::Quote) => true,
            (NameTypeLValue::SymType, NameTypeLValue::SymType) => true,
            (NameTypeLValue::Atom, NameTypeLValue::Atom) => true,
            (NameTypeLValue::CoreOperator, NameTypeLValue::CoreOperator) => true,
            (_, _) => false,
        }
    }
}

/**
FROM IMPLEMENTATION
**/

impl From<LSymType> for NameTypeLValue {
    fn from(lst: LSymType) -> Self {
        (&lst).into()
    }
}

impl From<&LValue> for NameTypeLValue {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::Bool(_) => NameTypeLValue::Bool,
            LValue::Number(_) => NameTypeLValue::Number,
            LValue::Symbol(_) => NameTypeLValue::Symbol,
            LValue::String(_) => NameTypeLValue::String,
            LValue::LFn(_) => NameTypeLValue::LFn,
            LValue::None => NameTypeLValue::None,
            LValue::Lambda(_) => NameTypeLValue::Lambda,
            LValue::Map(_) => NameTypeLValue::Map,
            LValue::List(_) => NameTypeLValue::List,
            LValue::Quote(_) => NameTypeLValue::Quote,
            LValue::SymType(_) => NameTypeLValue::SymType,
            LValue::CoreOperator(_) => NameTypeLValue::CoreOperator,
        }
    }
}

impl From<LValue> for NameTypeLValue {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

impl From<&LSymType> for NameTypeLValue {
    fn from(lst: &LSymType) -> Self {
        match lst {
            LSymType::StateFunction(_) => NameTypeLValue::StateFunction,
            LSymType::Type(_) => NameTypeLValue::Type,
            LSymType::Object(_) => NameTypeLValue::Object,
        }
    }
}

impl From<&Sym> for LValue {
    fn from(s: &Sym) -> Self {
        LValue::Symbol(s.clone())
    }
}

impl From<Sym> for LValue {
    fn from(s: Sym) -> Self {
        LValue::from(&s)
    }
}

impl From<&[LValue]> for LValue {
    fn from(lv: &[LValue]) -> Self {
        LValue::List(lv.to_vec())
    }
}

impl From<&Vec<LValue>> for LValue {
    fn from(vec: &Vec<LValue>) -> Self {
        LValue::List(vec.clone())
    }
}

impl From<Vec<LValue>> for LValue {
    fn from(vec: Vec<LValue>) -> Self {
        (&vec).into()
    }
}

impl From<&LSymType> for LValue {
    fn from(lst: &LSymType) -> Self {
        LValue::SymType(lst.clone())
    }
}

impl From<LSymType> for LValue {
    fn from(lst: LSymType) -> Self {
        (&lst).into()
    }
}

impl From<&LCoreOperator> for LValue {
    fn from(co: &LCoreOperator) -> Self {
        LValue::CoreOperator(co.clone())
    }
}
impl From<LCoreOperator> for LValue {
    fn from(co: LCoreOperator) -> Self {
        (&co).into()
    }
}

/**
** AS COMMAND IMPLEMENTATION
**/

///Transform an object in Lisp command to reconstuct itself.
pub trait AsCommand {
    fn as_command(&self) -> String;
}

impl AsCommand for LSymType {
    fn as_command(&self) -> String {
        match self {
            LSymType::StateFunction(sf) => sf.as_command(),
            LSymType::Type(t) => match t {
                None => "".to_string(),
                Some(st) => {
                    format!("(subtype {})", st)
                }
            },
            LSymType::Object(o) => {
                format!("{}", o)
            }
        }
    }
}

impl AsCommand for LStateFunction {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", STATE_FUNCTION).as_str());
        for t_param in &self.t_params {
            result.push_str(format!("{} ", t_param.to_string()).as_str());
        }
        result.push_str(format!("{})\n", self.t_value.to_string()).as_str());
        result
    }
}

impl AsCommand for Vec<LValue> {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", LIST).as_str());
        for param in self {
            result.push_str(format!("{} ", param.as_command()).as_str());
        }
        result
    }
}

impl AsCommand for HashMap<LValue, LValue> {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", MAP).as_str());
        for (key, value) in self.iter() {
            result.push_str("(list ");
            result.push_str(format!("{} {})", key.as_command(), value.as_command()).as_str())
        }
        result.push_str(")\n");
        result
    }
}

impl AsCommand for LValue {
    fn as_command(&self) -> String {
        match self {
            LValue::List(l) => l.as_command(),
            LValue::SymType(st) => st.as_command(),
            LValue::String(s) => s.to_string(),
            LValue::LFn(f) => f.label.to_string(),
            LValue::None => "none".to_string(),
            LValue::Symbol(s) => s.to_string(),
            LValue::Number(n) => n.to_string(),
            LValue::Bool(b) => b.to_string(),
            LValue::Lambda(_) => "".to_string(),
            LValue::Map(m) => m.as_command(),
            LValue::Quote(q) => q.to_string(),
            LValue::CoreOperator(c) => c.to_string(),
        }
    }
}

/**
DISPLAY IMPLEMENTATION
**/

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

impl Display for LError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LError::WrongType(s, s1, s2) => write!(f, "{}: Got {}, expected {}", s, s1, s2),
            LError::ErrLoc(e) => write!(f, "{}", e),
            LError::UndefinedSymbol(s) => write!(f, "{} is undefined", s),
            LError::WrongNumberOfArgument(s, g, r) => {
                if r.is_empty() {
                    write!(f, "\"{}\": Got {} element(s), expected {}", s, g, r.start)
                } else if r.end == std::usize::MAX {
                    write!(
                        f,
                        "\"{}\": Got {} element(s), expected at least {}",
                        s, g, r.start
                    )
                } else if r.start == std::usize::MIN {
                    write!(
                        f,
                        "\"{}\": Got {} element(s), expected at most {}",
                        s, g, r.end
                    )
                } else {
                    write!(
                        f,
                        "\"{}\": Got {} element(s), expected between {} and {}",
                        s, g, r.start, r.end
                    )
                }
            }
            LError::SpecialError(s) => write!(f, "{}", s),
            LError::ConversionError(s1, s2) => write!(f, "Cannot convert {} into {}.", s1, s2),
            LError::NotInListOfExpectedTypes(lv, typ, list_types) => {
                write!(f, "{}: Got {}, expected {:?}", lv, typ, list_types)
            }
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

impl Display for LSymType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LSymType::StateFunction(sf) => write!(f, "{}", sf),
            LSymType::Type(t) => match t {
                None => write!(f, "root type"),
                Some(_type) => write!(f, "subtype of {}", _type),
            },
            LSymType::Object(o) => write!(f, "{}", o),
        }
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::String(s) => write!(f, "{}", s),
            LValue::LFn(fun) => write!(f, "{}", fun.label),
            LValue::None => write!(f, "None"),
            LValue::Symbol(s) => write!(f, "{}", s),
            LValue::Number(n) => write!(f, "{}", n),
            LValue::Bool(b) => write!(f, "{}", b),
            LValue::SymType(st) => write!(f, "{}", st),
            LValue::List(list) => {
                let mut result = String::new();
                result.push('(');
                for element in list {
                    result.push_str(element.to_string().as_str());
                    result.push(' ');
                }
                result.push(')');
                write!(f, "{}", result)
            }
            LValue::Lambda(l) => write!(f, "{}", l),
            LValue::Map(m) => {
                let mut result = String::new();
                for (key, value) in m.iter() {
                    result.push_str(format!("{}: {}\n", key, value).as_str());
                }
                write!(f, "{}", result)
            }
            LValue::Quote(q) => write!(f, "{}", q),
            LValue::CoreOperator(co) => {
                write!(f, "{}", co)
            }
        }
    }
}

impl Display for LLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} : {}", self.params, self.body)
    }
}

impl Display for NameTypeLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLValue::Type => "Type",
            NameTypeLValue::StateFunction => "StateFunction",
            NameTypeLValue::Number => "Number",
            NameTypeLValue::Bool => "Boolean",
            NameTypeLValue::Symbol => "Symbol",
            NameTypeLValue::String => "String",
            NameTypeLValue::SExpr => "SExpr",
            NameTypeLValue::LFn => "LFn",
            NameTypeLValue::None => "None",
            NameTypeLValue::FactBase => "FactBase",
            NameTypeLValue::Object => "Object",
            NameTypeLValue::Lambda => "lambda",
            NameTypeLValue::Map => "map",
            NameTypeLValue::List => "list",
            NameTypeLValue::Quote => "quote",
            NameTypeLValue::SymType => "symtype",
            NameTypeLValue::Atom => "atom",
            NameTypeLValue::CoreOperator => "core_operator",
        };
        write!(f, "{}", str)
    }
}

impl Display for LCoreOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LCoreOperator::Define => write!(f, "{}", DEFINE.to_string()),
            LCoreOperator::DefLambda => write!(f, "{}", LAMBDA.to_string()),
            LCoreOperator::If => write!(f, "{}", IF.to_string()),
            LCoreOperator::Quote => write!(f, "{}", QUOTE.to_string()),
            LCoreOperator::QuasiQuote => write!(f, "{}", QUASI_QUOTE.to_string()),
            LCoreOperator::UnQuote => write!(f, "{}", UNQUOTE.to_string()),
            LCoreOperator::DefMacro => write!(f, "{}", DEF_MACRO.to_string()),
            LCoreOperator::Set => write!(f, "{}", SET.to_string()),
            LCoreOperator::Begin => write!(f, "{}", BEGIN.to_string()),
        }
    }
}

/**
DEBUG
**/

impl Debug for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

#[cfg(test)]
mod tests {
    use crate::lisp::lisp_struct::LValue;
    use im::HashMap;
    use std::collections::hash_map::{DefaultHasher, RandomState};
    use std::hash::{BuildHasher, Hash, Hasher};

    //#[test]
    pub fn test_hash_list() {
        let mut map: HashMap<LValue, LValue> = HashMap::new();
        let key = LValue::List(vec![LValue::Symbol("a".into()), LValue::Symbol("b".into())]);
        let value = LValue::Bool(true);
        map.insert(key.clone(), value);
        println!("get value: ");
        match map.get(&key) {
            None => println!("None"),
            Some(v) => println!("value: {}", v),
        }
    }

    //#[test]
    pub fn test_hasher() {
        let map: HashMap<LValue, LValue> = HashMap::new();
        let mut hasher1 = map.hasher().build_hasher();
        let mut hasher2 = map.hasher().build_hasher();
        let key = LValue::List(vec![LValue::Symbol("a".into()), LValue::Symbol("b".into())]);
        let value = LValue::Bool(true);
        key.hash(&mut hasher1);
        println!("hash value : {}", hasher1.finish());
        key.clone().hash(&mut hasher2);
        println!("hash value : {}", hasher2.finish());
    }

    #[test]
    pub fn test_hash() {
        let mut map: HashMap<LValue, LValue> = HashMap::new();
        let mut hasher1 = map.hasher().build_hasher();
        let key1 = LValue::List(vec![LValue::Symbol("a".into()), LValue::Symbol("b".into())]);
        let key2 = LValue::Bool(true);
        let value = LValue::Bool(true);

        key2.hash(&mut hasher1);
        println!("hash value : {}", hasher1.finish());
        map.insert(key2.clone(), value.clone());
        let result_value = map.get(&key2.clone()).unwrap_or(&LValue::None);
        println!("value: {}", result_value);
        let mut hasher2 = map.hasher().build_hasher();
        key2.hash(&mut hasher2);
        println!("hash value : {}", hasher2.finish());

        let mut hasher3 = map.hasher().build_hasher();
        key1.hash(&mut hasher3);
        println!("hash value : {}", hasher3.finish());
        map.insert(key1.clone(), value.clone());
        let value = map.get(&key1).unwrap_or(&LValue::None);
        println!("value: {}", value);
        let mut hasher4 = map.hasher().build_hasher();
        key1.hash(&mut hasher4);
        println!("hash value : {}", hasher4.finish());

        println!("hash map:");
        for (key, value) in map.iter() {
            println!("{} = {}", key, value);
        }
    }

    #[test]
    pub fn test_hash_with_vec() {
        let mut map: HashMap<Vec<LValue>, i32> = HashMap::new();
        let key = vec![LValue::Bool(true), LValue::Bool(true)];
        let value = 4;
        println!("insert value: ");
        map.insert(key, value);
        let search_key = vec![LValue::Bool(true), LValue::Bool(true)];
        println!("get value: ");
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)
    }
    #[test]
    pub fn test_hash_with_LValue_List() {
        let mut map: HashMap<LValue, i32> = HashMap::new();
        let key = LValue::List(vec![LValue::Bool(true), LValue::Bool(true)]);
        let value = 4;

        println!("insert value: ");
        map.insert(key, value);
        println!("get value: ");
        let search_key = LValue::List(vec![LValue::Bool(true), LValue::Bool(true)]);
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)
    }
    #[test]
    pub fn test_hash_with_LValue_Bool() {
        let mut map: HashMap<LValue, i32> = HashMap::new();
        let key = LValue::Bool(true);
        let value = 4;

        println!("insert value: ");
        map.insert(key, value);
        println!("get value: ");
        let search_key = LValue::Bool(true);
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)
    }

    /*#[test]
    pub fn test_hash_with_LValue_Quote() {
        let mut map: HashMap<LValue, i32> = HashMap::new();
        let key = LValue::Quote(Box::new(LValue::Bool(true)));
        let value = 4;

        println!("insert value: ");
        map.insert(key, value);
        println!("get value: ");
        let search_key = LValue::Quote(Box::new(LValue::Bool(true)));
        let value = *map.get(&search_key).unwrap_or(&-1);
        assert_eq!(value, 4)
    }*/
}

//TODO: Add tests
