use crate::lisp::lisp_language::*;
use aries_utils::input::{ErrLoc, Sym};
use std::cmp::Ordering;
//use std::collections::HashMap;
use crate::lisp::LEnv;
use im::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Range, Sub, Div, Mul, Deref};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use aries_planning::parsing::sexpr::SExpr;
use std::borrow::Borrow;
use crate::lisp::lisp_struct::LError::WrongNumberOfArgument;
use std::panic::resume_unwind;

//TODO: Finish to implement the new kind in enum LValue

pub enum LError {
    WrongType(String, NameTypeLValue, NameTypeLValue),
    WrongNumberOfArgument(usize, Range<usize>),
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
            //TODO: verify the hash for the float
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

#[derive(Clone)]
pub struct LStateFunction {
    pub t_params: Vec<Sym>,
    pub t_value: Sym,
}

#[derive(Clone)]
pub struct LFactBase {
    facts: HashMap<Vec<Sym>, Sym>,
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
pub enum LSymType {
    StateFunction(LStateFunction),
    Type(LType),
    Variable(LVariable),
    Object(LType),
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

pub type LFn = Rc<fn(&[LValue], &LEnv) -> Result<LValue, LError>>;

#[derive(Clone)]
pub struct LLambda {
    params: Vec<Sym>,
    body: SExpr,
    env: LEnv,
}

impl LLambda {
    pub fn new(params: &[Sym], body: &SExpr, env: &LEnv) -> Self {
        LLambda {
            params: params.to_vec(),
            body : body.clone(),
            env : env.clone()
        }
    }

    pub fn get_new_env(&self, args: &[LValue]) -> Result<LEnv, LError> {
        if self.params.len() != args.len() {
            return Err(WrongNumberOfArgument(args.len(), self.params.len()..self.params.len()))
        }
        let mut env = self.env.clone();
        for arg in self.params.iter().zip(args) {
            env.symbols.insert(arg.0.to_string(), arg.1.clone());
        }
        Ok(env)
    }

    pub fn get_body(&self) -> &SExpr{
        &self.body
    }
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
    Pair(Box<LValue>, Box<LValue>),
    // error
    None,
    LFn(LFn),
    StateVariable(LStateVariable),
    Lambda(LLambda),
    SymType(LSymType),
}

impl Hash for LValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        println!("value to hash: {}", self);
        match self {
            LValue::Symbol(s) => (*s).hash(state),
            LValue::Number(n) => (*n).hash(state),
            LValue::Bool(b) => (*b).hash(state),
            LValue::String(s) => (*s).hash(state),
            LValue::Map(m) => (*m).hash(state),
            LValue::List(l) => {
                (*l).hash(state);
            },
            LValue::Quote(q) => {
                let q = &**q;
                q.hash(state);
            },
            LValue::Pair(a,b) => {
                let a = &**a;
                let b = &**b;
                a.hash(state); b.hash(state);
            }
            lv => panic!("cannot hash {}", lv.to_string())
        };
        println!("value of the hash: {}", state.finish())

    }
}

impl Eq for LValue {

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
            _ => Err(LError::SpecialError("cannot convert into bool".to_string())),
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

impl Sub for &LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 - n2)),
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

impl Mul for &LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 * n2)),
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

impl Div for &LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 / n2)),
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



impl Add for LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        &self+&rhs
    }
}
impl Sub for LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self-&rhs
    }
}
impl Mul for LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        &self*&rhs
    }
}
impl Div for LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        &self/&rhs
    }
}


#[derive(Clone)]
pub enum NameTypeLValue {
    State,
    Variable,
    Type,
    StateFunction,
    Pair,
    Number,
    Bool,
    Symbol,
    String,
    SExpr,
    LFn,
    Lambda,
    None,
    FactBase,
    Object,
    Map,
    List,
    Quote
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
            (NameTypeLValue::State, NameTypeLValue::State) => true,
            (NameTypeLValue::Variable, NameTypeLValue::Variable) => true,
            (NameTypeLValue::Number, NameTypeLValue::Number) => true,
            (NameTypeLValue::FactBase, NameTypeLValue::FactBase) => true,
            (NameTypeLValue::Object, NameTypeLValue::Object) => true,
            (NameTypeLValue::Map, NameTypeLValue::Map) => true,
            (NameTypeLValue::List, NameTypeLValue::List) => true,
            (NameTypeLValue::Quote, NameTypeLValue::Quote) => true,
            (NameTypeLValue::Pair, NameTypeLValue::Pair) => true,
            (_,_) => false,
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
            LValue::SymType(st) => st.into(),
            LValue::Lambda(_) => NameTypeLValue::Lambda,
            LValue::Map(_) => NameTypeLValue::Map,
            LValue::List(_) => NameTypeLValue::List,
            LValue::Quote(_) => NameTypeLValue::Quote,
            LValue::Pair(_, _) => NameTypeLValue::Pair,
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
            LSymType::Variable(_) => NameTypeLValue::Variable,
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


///Transform an object in Lisp command to reconstuct itself.
pub trait AsCommand {
    fn as_command(&self) -> String;
}

impl AsCommand for LSymType {
    fn as_command(&self) -> String {
        match self {
            LSymType::StateFunction(sf) => sf.as_command(),
            LSymType::Type(t) => t.as_command(),
            LSymType::Variable(v) => v.as_command(),
            LSymType::Object(o) => {
                format!("({} {})", OBJECT, o)
            }
        }
    }
}

impl AsCommand for LType {
    fn as_command(&self) -> String {
        match self {
            LType::Symbol(s) => format!("({} {})\n", TYPE, s),
            _ => "".to_string(),
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

impl AsCommand for LVariable {
    fn as_command(&self) -> String {
        format!("({} {} {})", VARIABLE, self.v_type, self.value)
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
            result.push_str("(pair ");
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
            LValue::LFn(_) => "".to_string(),
            LValue::None => "".to_string(),
            LValue::Symbol(s) => s.to_string(),
            LValue::Number(n) => n.to_string(),
            LValue::Bool(b) => b.to_string(),
            LValue::Lambda(l) => "".to_string(),
            LValue::Map(m) => m.as_command(),
            LValue::Quote(q) => q.as_command(),
            LValue::Pair(p, q) => format!("(pair {} {}", p.as_command(), q.as_command())
        }
    }
}


/**
DISPLAY IMPLEMENTATION
**/


impl Display for LError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LError::WrongType(s, s1, s2) => write!(f, "{}: Got {}, expected {}", s, s1, s2),
            LError::ErrLoc(e) => write!(f, "{}", e),
            LError::UndefinedSymbol(s) => write!(f, "{} is undefined", s),
            LError::WrongNumberOfArgument(s, r) => write!(f, "Got {}, expected {:?}", s, r),
            LError::SpecialError(s) => write!(f, "{}", s),
            LError::ConversionError(s1, s2) => write!(f, "Cannot convert {} into {}.", s1, s2),
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

impl Display for LVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} - {}", self.value, self.v_type)
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
            LSymType::StateFunction(sf) => write!(f, "sf: {}", sf),
            LSymType::Type(t) => write!(f, "type: child of {}", t),
            LSymType::Variable(v) => write!(f, "variable: {}", v),
            LSymType::Object(o) => write!(f, "object: type = {}", o),
        }
    }
}

//TODO: impl Display for HashMap<LValue, LValue>
impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::String(s) => write!(f, "{}", s),
            LValue::LFn(_) => write!(f, "LFunction"),
            LValue::None => write!(f, "None"),
            LValue::Symbol(s) => write!(f, "{}", s),
            LValue::Number(n) => write!(f, "{}", n),
            LValue::Bool(b) => write!(f, "{}", b),
            LValue::SymType(st) => write!(f, "{}", st),
            LValue::List(s) => write!(f, "{:?}", s),
            LValue::Lambda(s) => write!(f,"Lambda"),
            LValue::Map(m) => {
                let mut result = String::new();
                for (key, value) in m.iter() {
                    result.push_str(format!("{} = {}", key, value).as_str());
                }
                write!(f, "{}", result)
            }
            LValue::Quote(q) => write!(f, "{}", q),
            LValue::Pair(a, b) => write!(f, "({},{})",a,b )
        }
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
            NameTypeLValue::Variable => "Variable",
            NameTypeLValue::FactBase => "FactBase",
            NameTypeLValue::Object => "Object",
            NameTypeLValue::State => "State",
            NameTypeLValue::Lambda => "lambda",
            NameTypeLValue::Map => "map",
            NameTypeLValue::List => "list",
            NameTypeLValue::Quote => "quote",
            NameTypeLValue::Pair => "pair"
        };
        write!(f, "{}", str)
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

//TODO: Add tests
