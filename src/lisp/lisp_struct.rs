use crate::lisp::lisp_language::*;
use aries_planning::parsing::sexpr::SExpr;
use aries_utils::input::{ErrLoc, Sym};
use std::cmp::Ordering;
//use std::collections::HashMap;
use crate::lisp::LEnv;
use im::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Add, Range, Sub, Div, Mul};
use std::rc::Rc;


pub enum LError {
    WrongType(String, NameTypeLValue, NameTypeLValue),
    WrongNumerOfArgument(usize, Range<usize>),
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
pub struct LStateVariable {
    params: Vec<Sym>,
    value: Sym,
}


impl LStateVariable {
    pub fn new(params: Vec<Sym>, value: Sym) -> Self {
        Self { params, value }
    }

    pub fn as_key_value(&self) -> (Vec<Sym>, Sym) {
        (self.params.clone(), self.value.clone())
    }

    pub fn key(&self) -> Vec<Sym>{
        self.params.clone()
    }

    pub fn value(&self) -> Sym{
        self.value.clone()
    }
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
pub struct LState {
    state_variables: HashMap<Vec<Sym>, Sym>,
}

impl LState {
    pub fn new(state_variables: HashMap<Vec<Sym>, Sym>) -> Self {
        LState { state_variables }
    }

    pub fn get_all_state_variables(&self) -> HashMap<Vec<Sym>, Sym> {
        self.state_variables.clone()
    }

    pub fn get_state_variable(&self, key: &[Sym]) -> Sym {
        match self.state_variables.get(key) {
            None => Sym::from("none"),
            Some(s) => s.clone()
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

pub type LLambda = Rc<Box<dyn Fn(&[LValue], &LEnv) -> Result<LValue, LError>>>;

#[derive(Clone)]
pub enum LValue {
    State(LState),
    Symbol(Sym),
    Number(LNumber),
    Bool(bool),
    FactBase(LFactBase),
    StateVariable(LStateVariable),
    String(String),
    SExpr(SExpr),
    LFn(LFn),
    Lambda(LLambda),
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
            _ => Err(LError::SpecialError("cannot convert into bool".to_string())),
        }
    }

    pub fn as_sexpr(&self) -> Result<SExpr, LError> {
        match self {
            LValue::SExpr(s) => Ok(s.clone()),
            _ => Err(LError::SpecialError("cannot convert into sexpr".to_string())),
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
    StateVariable,
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
            (NameTypeLValue::State, NameTypeLValue::State) => true,
            (NameTypeLValue::Variable, NameTypeLValue::Variable) => true,
            (NameTypeLValue::Number, NameTypeLValue::Number) => true,
            (NameTypeLValue::FactBase, NameTypeLValue::FactBase) => true,
            (NameTypeLValue::Object, NameTypeLValue::Object) => true,
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
            LValue::SExpr(_) => NameTypeLValue::SExpr,
            LValue::LFn(_) => NameTypeLValue::LFn,
            LValue::None => NameTypeLValue::None,
            LValue::FactBase(_) => NameTypeLValue::FactBase,
            LValue::StateVariable(_) => NameTypeLValue::StateVariable,
            LValue::SymType(st) => st.into(),
            LValue::State(_) => NameTypeLValue::State,
            LValue::Lambda(_) => NameTypeLValue::Lambda,
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

impl AsCommand for LStateVariable {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", STATE_VARIABLE).as_str());
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
        result.push_str(format!("({} ", FACTBASE).as_str());
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

impl AsCommand for LState {
    fn as_command(&self) -> String {
        let mut result = String::new();
        result.push_str(format!("({} ", STATE).as_str());
        for (keys, value) in self.state_variables.iter() {
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
            LValue::State(state) => state.as_command(),
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
            LValue::Lambda(l) => "".to_string(),
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
            LError::WrongNumerOfArgument(s, r) => write!(f, "Got {}, expected {:?}", s, r),
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


impl Display for LState {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut result = String::new();
        for (params, value) in self.state_variables.iter() {
            let params = params.as_slice();
            result.push_str(format!("{}(",params.get(0).unwrap()).as_str());
            for (i,param) in params[1..].iter().enumerate() {
                if i != 0 {
                    result.push(',');
                }
                result.push_str(param.as_str());
            }
            result.push_str(format!(")={}\n", value).as_str());
        }

        write!(f, "{}",result)
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
            LValue::State(s) => write!(f, "{}", s),
            LValue::Lambda(s) => write!(f,"Lambda")
        }
    }
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
            NameTypeLValue::State => "State",
            NameTypeLValue::Lambda => "lambda",
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
