use crate::core::{core_macros_and_lambda, eval, ContextCollection, RefLEnv};
use crate::language::*;
use crate::structs::LError::{ConversionError, SpecialError, WrongNumberOfArgument};
use aries_utils::input::ErrLoc;
use im::HashMap;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Range, Sub};
use std::rc::Rc;

#[derive(Debug)]
pub enum LError {
    WrongType(LValue, NameTypeLValue, NameTypeLValue),
    NotInListOfExpectedTypes(LValue, NameTypeLValue, Vec<NameTypeLValue>),
    WrongNumberOfArgument(LValue, usize, Range<usize>),
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

impl From<std::io::Error> for LError {
    fn from(e: std::io::Error) -> Self {
        SpecialError(e.to_string())
    }
}

impl From<ErrLoc> for LError {
    fn from(e: ErrLoc) -> Self {
        LError::ErrLoc(e)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LNumber {
    Int(i64),
    Float(f64),
    Usize(usize),
}

impl Display for LNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LNumber::Int(i) => write!(f, "{}", i),
            LNumber::Float(fl) => write!(f, "{}", fl),
            LNumber::Usize(u) => write!(f, "{}", u),
        }
    }
}

impl From<&LNumber> for String {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => i.to_string(),
            LNumber::Float(f) => f.to_string(),
            LNumber::Usize(u) => u.to_string(),
        }
    }
}

impl From<LNumber> for String {
    fn from(n: LNumber) -> Self {
        (&n).to_string()
    }
}

impl PartialEq for LNumber {
    fn eq(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 == n2
    }
}

impl From<&LNumber> for usize {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => *i as usize,
            LNumber::Float(f) => *f as usize,
            LNumber::Usize(u) => *u,
        }
    }
}

impl From<&LNumber> for f64 {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => *i as f64,
            LNumber::Float(f) => *f,
            LNumber::Usize(u) => *u as f64,
        }
    }
}

impl From<&LNumber> for i64 {
    fn from(n: &LNumber) -> Self {
        match n {
            LNumber::Int(i) => *i,
            LNumber::Float(f) => *f as i64,
            LNumber::Usize(u) => *u as i64,
        }
    }
}

impl From<i64> for LNumber {
    fn from(i: i64) -> Self {
        LNumber::Int(i)
    }
}

impl From<i32> for LNumber {
    fn from(i: i32) -> Self {
        LNumber::Int(i as i64)
    }
}

impl From<f64> for LNumber {
    fn from(f: f64) -> Self {
        LNumber::Float(f)
    }
}

impl From<f32> for LNumber {
    fn from(f: f32) -> Self {
        LNumber::Float(f as f64)
    }
}

impl From<usize> for LNumber {
    fn from(u: usize) -> Self {
        LNumber::Usize(u)
    }
}

impl Hash for LNumber {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LNumber::Int(i) => i.hash(state),
            LNumber::Float(f) => f.to_string().hash(state),
            LNumber::Usize(u) => u.hash(state),
        }
    }
}

impl PartialOrd for LNumber {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 < n2
    }

    fn le(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 <= n2
    }

    fn gt(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 > n2
    }

    fn ge(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 >= n2
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
            (_, _) => unimplemented!(),
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
            (_, _) => unimplemented!(),
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
            (_, _) => unimplemented!(),
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
            (_, _) => unimplemented!(),
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

#[derive(Clone, Debug)]
pub enum LambdaArgs {
    Sym(String),
    List(Vec<String>),
}

impl From<String> for LambdaArgs {
    fn from(s: String) -> Self {
        LambdaArgs::Sym(s)
    }
}

impl From<Vec<String>> for LambdaArgs {
    fn from(vec_sym: Vec<String>) -> Self {
        LambdaArgs::List(vec_sym)
    }
}
#[derive(Clone, Debug)]
pub struct LLambda {
    params: LambdaArgs,
    body: Box<LValue>,
}

impl Display for LLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} : {}", self.params, self.body)
    }
}

impl PartialEq for LLambda {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl LLambda {
    pub fn new(params: LambdaArgs, body: LValue) -> Self {
        LLambda {
            params,
            body: Box::new(body),
        }
    }

    pub fn get_new_env(&self, args: &[LValue], outer: &RefLEnv) -> Result<RefLEnv, LError> {
        let mut env = RefLEnv::empty();

        match &self.params {
            LambdaArgs::Sym(param) => {
                let args = if args.len() == 1 {
                    args[0].clone()
                } else {
                    args.into()
                };
                env.symbols.insert(param.to_string(), args);
            }
            LambdaArgs::List(params) => {
                if params.len() != args.len() {
                    return Err(WrongNumberOfArgument(
                        args.into(),
                        args.len(),
                        params.len()..params.len(),
                    ));
                }
                for (param, arg) in params.iter().zip(args) {
                    env.symbols.insert(param.to_string(), arg.clone());
                }
            }
        };

        env.outer = Some(outer.clone());
        Ok(env)
    }

    pub fn call(
        &self,
        args: &[LValue],
        outer: &RefLEnv,
        ctxs: &mut ContextCollection,
    ) -> Result<LValue, LError> {
        let mut new_env = self.get_new_env(args, outer)?;
        eval(&*self.body, &mut new_env, ctxs)
    }

    pub fn get_body(&self) -> LValue {
        *self.body.clone()
    }
}

pub type NativeFn = dyn Fn(&[LValue], &RefLEnv, &dyn Any) -> Result<LValue, LError>;

#[derive(Clone)]
pub struct LFn {
    pub(crate) fun: Rc<NativeFn>,
    pub(crate) debug_label: &'static str,
    index_mod: Option<usize>,
}

impl Debug for LFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "label: {:?}\nmod : {}",
            self.debug_label,
            match self.index_mod {
                None => "none".to_string(),
                Some(u) => u.to_string(),
            }
        )
    }
}

impl LFn {
    pub fn new<
        T: 'static,
        R: Into<Result<LValue, LError>>,
        F: Fn(&[LValue], &RefLEnv, &T) -> R + 'static,
    >(
        lbd: Box<F>,
        debug_label: &'static str,
    ) -> Self {
        let x = move |args: &[LValue], env: &RefLEnv, ctx: &dyn Any| -> Result<LValue, LError> {
            let ctx: Option<&T> = ctx.downcast_ref::<T>();
            if let Some(ctx) = ctx {
                lbd(args, env, ctx).into()
            } else {
                Err(LError::SpecialError(
                    "Impossible to downcast context".to_string(),
                ))
            }
        };
        LFn {
            fun: Rc::new(x),
            debug_label,
            index_mod: None,
        }
    }

    pub fn call(&self, args: &[LValue], env: &RefLEnv, ctx: &dyn Any) -> Result<LValue, LError> {
        (self.fun)(args, env, ctx)
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn get_label(&self) -> &'static str {
        self.debug_label
    }
}

pub type NativeMutFn = dyn Fn(&[LValue], &mut RefLEnv, &mut dyn Any) -> Result<LValue, LError>;

#[derive(Clone)]
pub struct LMutFn {
    pub(crate) fun: Rc<NativeMutFn>,
    pub(crate) debug_label: &'static str,
    index_mod: Option<usize>,
}

impl Debug for LMutFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "label: {:?}\nmod : {}",
            self.debug_label,
            match self.index_mod {
                None => "none".to_string(),
                Some(u) => u.to_string(),
            }
        )
    }
}

impl LMutFn {
    pub fn new<
        T: 'static,
        R: Into<Result<LValue, LError>>,
        F: Fn(&[LValue], &mut RefLEnv, &mut T) -> R + 'static,
    >(
        lbd: Box<F>,
        debug_label: &'static str,
    ) -> Self {
        let x = move |args: &[LValue],
                      env: &mut RefLEnv,
                      ctx: &mut dyn Any|
              -> Result<LValue, LError> {
            let ctx: Option<&mut T> = ctx.downcast_mut::<T>();
            if let Some(ctx) = ctx {
                lbd(args, env, ctx).into()
            } else {
                Err(LError::SpecialError(
                    "Impossible to downcast context".to_string(),
                ))
            }
        };
        LMutFn {
            fun: Rc::new(x),
            debug_label,
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call(
        &self,
        args: &[LValue],
        env: &mut RefLEnv,
        ctx: &mut dyn Any,
    ) -> Result<LValue, LError> {
        (self.fun)(args, env, ctx)
    }

    pub fn get_label(&self) -> &'static str {
        self.debug_label
    }
}

#[derive(Clone, PartialOrd, PartialEq, Eq, Debug, Serialize, Deserialize)]
#[serde(untagged, rename_all = "lowercase")]
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

impl TryFrom<&str> for LCoreOperator {
    type Error = LError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            DEFINE => Ok(LCoreOperator::Define),
            LAMBDA => Ok(LCoreOperator::DefLambda),
            IF => Ok(LCoreOperator::If),
            QUOTE => Ok(LCoreOperator::Quote),
            QUASI_QUOTE => Ok(LCoreOperator::QuasiQuote),
            UNQUOTE => Ok(LCoreOperator::UnQuote),
            DEF_MACRO => Ok(LCoreOperator::DefMacro),
            SET => Ok(LCoreOperator::Set),
            BEGIN => Ok(LCoreOperator::Begin),
            _ => Err(SpecialError(
                "string does not correspond to core operator".to_string(),
            )),
        }
    }
}

impl From<im::HashMap<LValue, LValue>> for LValue {
    fn from(map: HashMap<LValue, LValue>) -> Self {
        LValue::Map(map)
    }
}

/*

*/

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(untagged, rename_all = "lowercase")]
pub enum LValue {
    // symbol
    Symbol(String),
    // literaux
    Number(LNumber),
    #[serde(skip)]
    Fn(LFn),
    #[serde(skip)]
    MutFn(LMutFn),
    #[serde(skip)]
    Lambda(LLambda),
    #[serde(skip)]
    CoreOperator(LCoreOperator),

    // data structure
    #[serde(skip)]
    //TODO: Implement serde for mirror struct of im::hashmap
    Map(im::HashMap<LValue, LValue>),
    List(Vec<LValue>),
    Quote(Box<LValue>),
    //Refers to boolean 'false and empty list in lisp
    True,
    Nil,
}

/*impl Debug for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let string = match self {
            LValue::Symbol(s) => format!("LValue::Symbol: {:?}", s),
            LValue::Number(n) => format!("Number: {:?}", n),
            LValue::True => "LValue::True".to_string(),
            LValue::Map(map) => format!("LValue::Map: {:?}", map),
            LValue::List(list) => format!("List: {:?}", list),
            LValue::Quote(q) => format!("Quote: {:?}", q),
            LValue::Nil => "LValue::Nil".to_string(),
            LValue::Fn(lfn) => format!("Function: {:?}", lfn),
            LValue::MutFn(mutfn) => format!("Number: {:?}", mutfn),
            LValue::Lambda(l) => format!("Lambda: {:?}", l),
            LValue::CoreOperator(co) => format!("CoreOperator: {:?}", co),
        };
        write!(f, "{}", string)
    }
}*/

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::Fn(fun) => write!(f, "{}", fun.debug_label),
            LValue::MutFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::Nil => write!(f, "nil"),
            LValue::Symbol(s) => write!(f, "{}", s),
            LValue::Number(n) => write!(f, "{}", n),
            LValue::True => write!(f, "true"),
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

impl Hash for LValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValue::Symbol(s) => (*s).hash(state),
            LValue::Number(n) => (*n).hash(state),
            LValue::True => true.hash(state),
            LValue::Map(m) => (*m).hash(state),
            LValue::List(l) => {
                (*l).hash(state);
            }
            LValue::Quote(s) => {
                s.to_string().hash(state);
            }

            LValue::Nil => false.hash(state),
            _ => {}
        };
    }
}

impl Eq for LValue {}

impl TryFrom<&LValue> for String {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Symbol(s) => Ok(s.clone()),
            LValue::True => Ok(TRUE.into()),
            LValue::Nil => Ok(NIL.into()),
            LValue::Number(n) => Ok(n.to_string()),
            LValue::Fn(f) => Ok(f.debug_label.to_string()),
            LValue::MutFn(f) => Ok(f.debug_label.to_string()),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::Symbol)),
        }
    }
}

impl TryFrom<LValue> for String {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for Vec<LValue> {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::List(l) => Ok(l.clone()),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::List)),
        }
    }
}
impl TryFrom<LValue> for Vec<LValue> {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for i64 {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(n.into()),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::Number)),
        }
    }
}

impl TryFrom<LValue> for i64 {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for f64 {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(n.into()),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::Number)),
        }
    }
}

impl TryFrom<LValue> for f64 {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for bool {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::True => Ok(true),
            LValue::Nil => Ok(false),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::Bool)),
        }
    }
}

impl TryFrom<LValue> for bool {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for LCoreOperator {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::CoreOperator(co) => Ok(co.clone()),
            LValue::Symbol(s) => Ok(s.as_str().try_into()?),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::CoreOperator)),
        }
    }
}

impl TryFrom<LValue> for LCoreOperator {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for LLambda {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Lambda(l) => Ok(l.clone()),
            lv => Err(ConversionError(lv.into(), NameTypeLValue::Lambda)),
        }
    }
}

impl TryFrom<LValue> for LLambda {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl PartialEq for LValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            //bool comparison
            //Number comparison
            (LValue::Number(n1), LValue::Number(n2)) => *n1 == *n2,
            (LValue::Symbol(s1), LValue::Symbol(s2)) => *s1 == *s2,
            (LValue::True, LValue::True) => true,
            (LValue::Nil, LValue::Nil) => true,
            //Text comparison
            (LValue::List(l1), LValue::List(l2)) => *l1 == *l2,
            (LValue::Map(m1), LValue::Map(m2)) => *m1 == *m2,
            (LValue::Lambda(l1), LValue::Lambda(l2)) => *l1 == *l2,
            (LValue::Quote(q1), LValue::Quote(q2)) => q1.to_string() == q2.to_string(), //function comparison
            (LValue::Fn(f1), LValue::Fn(f2)) => f1.debug_label == f2.debug_label,
            (LValue::MutFn(mf1), LValue::MutFn(mf2)) => mf1.debug_label == mf2.debug_label,
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

impl From<u32> for LValue {
    fn from(u: u32) -> Self {
        LValue::Number(LNumber::Int(u as i64))
    }
}

impl From<usize> for LValue {
    fn from(u: usize) -> Self {
        LValue::Number(LNumber::Usize(u))
    }
}

impl From<&str> for LValue {
    fn from(s: &str) -> Self {
        LValue::Symbol(s.to_string())
    }
}

impl From<String> for LValue {
    fn from(s: String) -> Self {
        LValue::Symbol(s)
    }
}

impl From<&[LValue]> for LValue {
    fn from(lv: &[LValue]) -> Self {
        if lv.is_empty() {
            LValue::Nil
        } else {
            LValue::List(lv.to_vec())
        }
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

impl From<f64> for LValue {
    fn from(f: f64) -> Self {
        LValue::Number(LNumber::Float(f))
    }
}

impl From<i64> for LValue {
    fn from(i: i64) -> Self {
        LValue::Number(LNumber::Int(i))
    }
}

impl From<f32> for LValue {
    fn from(f: f32) -> Self {
        LValue::Number(LNumber::Float(f as f64))
    }
}

impl From<i32> for LValue {
    fn from(i: i32) -> Self {
        LValue::Number(LNumber::Int(i as i64))
    }
}

impl From<bool> for LValue {
    fn from(b: bool) -> Self {
        match b {
            true => LValue::True,
            false => LValue::Nil,
        }
    }
}

#[derive(Clone, Debug)]
pub enum NameTypeLValue {
    Bool,
    CoreOperator,
    Atom,
    Object,
    Number,
    Int,
    Float,
    Usize,
    True,
    Symbol,
    String,
    SExpr,
    Fn,
    MutFn,
    Lambda,
    Nil,
    Map,
    List,
    Quote,
    Other(String),
}

impl Display for NameTypeLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLValue::Number => "Number",
            NameTypeLValue::True => "True",
            NameTypeLValue::Symbol => "Symbol",
            NameTypeLValue::String => "String",
            NameTypeLValue::SExpr => "SExpr",
            NameTypeLValue::Fn => "Fn",
            NameTypeLValue::Nil => "Nil",
            NameTypeLValue::Object => "Object",
            NameTypeLValue::Lambda => "lambda",
            NameTypeLValue::Map => "map",
            NameTypeLValue::List => "list",
            NameTypeLValue::Quote => "quote",
            NameTypeLValue::Atom => "atom",
            NameTypeLValue::CoreOperator => "core_operator",
            NameTypeLValue::Other(s) => s.as_str(),
            NameTypeLValue::MutFn => "LFn",
            NameTypeLValue::Int => "int",
            NameTypeLValue::Float => "float",
            NameTypeLValue::Usize => "usize",
            NameTypeLValue::Bool => "bool",
        };
        write!(f, "{}", str)
    }
}

impl PartialEq for NameTypeLValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (NameTypeLValue::String, NameTypeLValue::String) => true,
            (NameTypeLValue::SExpr, NameTypeLValue::SExpr) => true,
            (NameTypeLValue::True, NameTypeLValue::True) => true,
            (NameTypeLValue::Symbol, NameTypeLValue::Symbol) => true,
            (NameTypeLValue::Fn, NameTypeLValue::Fn) => true,
            (NameTypeLValue::Nil, NameTypeLValue::Nil) => true,
            (NameTypeLValue::Number, NameTypeLValue::Number) => true,
            (NameTypeLValue::Object, NameTypeLValue::Object) => true,
            (NameTypeLValue::Map, NameTypeLValue::Map) => true,
            (NameTypeLValue::List, NameTypeLValue::List) => true,
            (NameTypeLValue::Quote, NameTypeLValue::Quote) => true,
            (NameTypeLValue::Atom, NameTypeLValue::Atom) => true,
            (NameTypeLValue::CoreOperator, NameTypeLValue::CoreOperator) => true,
            (NameTypeLValue::Int, NameTypeLValue::Int) => true,
            (NameTypeLValue::Float, NameTypeLValue::Float) => true,
            (NameTypeLValue::Usize, NameTypeLValue::Usize) => true,
            (NameTypeLValue::Other(s1), NameTypeLValue::Other(s2)) => *s1 == *s2,
            (_, _) => false,
        }
    }
}

impl From<&LValue> for NameTypeLValue {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::True => NameTypeLValue::True,
            LValue::Number(LNumber::Float(_)) => NameTypeLValue::Float,
            LValue::Number(LNumber::Int(_)) => NameTypeLValue::Int,
            LValue::Number(LNumber::Usize(_)) => NameTypeLValue::Usize,
            LValue::Symbol(_) => NameTypeLValue::Symbol,
            LValue::Fn(_) => NameTypeLValue::Fn,
            LValue::MutFn(_) => NameTypeLValue::MutFn,
            LValue::Nil => NameTypeLValue::Nil,
            LValue::Lambda(_) => NameTypeLValue::Lambda,
            LValue::Map(_) => NameTypeLValue::Map,
            LValue::List(_) => NameTypeLValue::List,
            LValue::Quote(_) => NameTypeLValue::Quote,
            LValue::CoreOperator(_) => NameTypeLValue::CoreOperator,
        }
    }
}

impl From<LValue> for NameTypeLValue {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

#[derive(Default)]
pub struct InitLisp(Vec<&'static str>);

impl From<Vec<&'static str>> for InitLisp {
    fn from(vec: Vec<&'static str>) -> Self {
        InitLisp(vec.clone())
    }
}

impl InitLisp {
    pub fn core() -> InitLisp {
        core_macros_and_lambda()
    }
    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0)
    }

    pub fn begin_lisp(&self) -> String {
        let mut str = "(begin ".to_string();
        self.0.iter().for_each(|&x| str.push_str(x));
        str.push(')');
        str
    }
}

pub struct Module {
    pub ctx: Box<dyn Any>,
    pub prelude: Vec<(String, LValue)>,
    pub raw_lisp: InitLisp,
    pub label: &'static str,
}

impl Module {
    pub fn add_fn_prelude<
        T: 'static,
        R: Into<Result<LValue, LError>>,
        F: Fn(&[LValue], &RefLEnv, &T) -> R + 'static,
    >(
        &mut self,
        label: &'static str,
        fun: Box<F>,
    ) {
        self.prelude
            .push((label.into(), LValue::Fn(LFn::new(fun, label))))
    }

    pub fn add_mut_fn_prelude<
        T: 'static,
        R: Into<Result<LValue, LError>>,
        F: Fn(&[LValue], &mut RefLEnv, &mut T) -> R + 'static,
    >(
        &mut self,
        label: &'static str,
        fun: Box<F>,
    ) {
        self.prelude
            .push((label.into(), LValue::MutFn(LMutFn::new(fun, label))))
    }

    pub fn add_prelude(&mut self, label: &str, lv: LValue) {
        self.prelude.push((label.into(), lv));
    }
}

pub trait GetModule {
    fn get_module(self) -> Module;
}

//TODO: Complete tests writing
#[cfg(test)]
mod tests {
    use super::*;

    mod l_number {
        use super::*;

        fn test_add() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert_eq!(LNumber::Int(8), &i1 + &i2);
            assert_eq!(LNumber::Float(8.0), &i1 + &f2);
            assert_eq!(LNumber::Float(8.0), &f1 + &f2);
        }

        fn test_sub() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert_eq!(LNumber::Int(-2), &i1 - &i2);
            assert_eq!(LNumber::Float(-2.0), &i1 - &f2);
            assert_eq!(LNumber::Float(-2.0), &f1 - &f2);
        }

        fn test_mul() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert_eq!(LNumber::Int(15), &i1 * &i2);
            assert_eq!(LNumber::Float(15.0), &i1 * &f2);
            assert_eq!(LNumber::Float(15.0), &f1 * &f2);
        }

        fn test_div() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert_eq!(LNumber::Int(0), &i1 / &i2);
            assert_eq!(LNumber::Float(0.6), &i1 / &f2);
            assert_eq!(LNumber::Float(0.6), &f1 / &f2);
        }

        #[test]
        fn test_math() {
            test_add();
            test_sub();
            test_div();
            test_mul();
        }

        fn test_gt() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert!(!(&i1 > &i2));
            assert!(&i2 > &i1);
            assert!(!(&i2 > &i2));
            assert!(!(&f1 > &f2));
            assert!(&f2 > &f1);
            assert!(!(&f2 > &f2));
            assert!(&i2 > &f1);
        }

        fn test_lt() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert!(&i1 < &i2);
            assert!(!(&i2 < &i1));
            assert!(!(&i2 < &i2));
            assert!(&f1 < &f2);
            assert!(!(&f2 < &f1));
            assert!(!(&f2 < &f2));
            assert!(&i1 < &f2);
        }

        fn test_ge() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert!(!(&i1 >= &i2));
            assert!(&i2 >= &i1);
            assert!(&i2 >= &i2);
            assert!(!(&f1 >= &f2));
            assert!(&f2 >= &f1);
            assert!(&f2 >= &f2);
            assert!(&i2 >= &f2);
        }

        fn test_le() {
            let i1: LNumber = 3.into();
            let i2: LNumber = 5.into();
            let f1: LNumber = 3.0.into();
            let f2: LNumber = 5.0.into();
            assert!(&i1 <= &i2);
            assert!(!(&i2 <= &i1));
            assert!(&i2 <= &i2);
            assert!(&f1 <= &f2);
            assert!(!(&f2 <= &f1));
            assert!(&f2 <= &f2);
            assert!(&i2 <= &f2);
        }

        #[test]
        fn test_ord() {
            test_gt();
            test_ge();
            test_lt();
            test_le();
        }
    }

    mod l_value {

        use super::*;
        fn test_add() {
            let i1: LValue = 3.into();
            let i2: LValue = 5.into();
            let f1: LValue = 3.0.into();
            let f2: LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(8)), (&i1 + &i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(8.0)), (&i1 + &f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(8.0)), (&f1 + &f2).unwrap());
        }
        fn test_sub() {
            let i1: LValue = 3.into();
            let i2: LValue = 5.into();
            let f1: LValue = 3.0.into();
            let f2: LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(-2)), (&i1 - &i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(-2.0)), (&i1 - &f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(-2.0)), (&f1 - &f2).unwrap());
        }

        fn test_mul() {
            let i1: LValue = 3.into();
            let i2: LValue = 5.into();
            let f1: LValue = 3.0.into();
            let f2: LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(15)), (&i1 * &i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(15.0)), (&i1 * &f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(15.0)), (&f1 * &f2).unwrap());
        }

        fn test_div() {
            let i1: LValue = 3.into();
            let i2: LValue = 5.into();
            let f1: LValue = 3.0.into();
            let f2: LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(0)), (&i1 / &i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(0.6)), (&i1 / &f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(0.6)), (&f1 / &f2).unwrap());
        }

        #[test]
        fn test_math() {
            test_add();
            test_sub();
            test_mul();
            test_div();
        }

        fn test_gt() {
            let i1: LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(!(&i1 > &f2));
            assert!(&f2 > &i1);
            assert!(!(&f2 > &f2));
        }

        fn test_ge() {
            let i1: LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(!(&i1 >= &f2));
            assert!(&f2 >= &i1);
            assert!(&f2 >= &f2);
        }

        fn test_lt() {
            let i1: LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(&i1 < &f2);
            assert!(!(&f2 < &i1));
            assert!(!(&f2 < &f2));
        }

        fn test_le() {
            let i1: LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(&i1 <= &f2);
            assert!(!(&f2 <= &i1));
            assert!(&f2 <= &f2);
        }

        #[test]
        fn test_ord() {
            test_gt();
            test_ge();
            test_lt();
            test_le();
        }
    }

    /*#[test]
    fn test_native_lambda() {

        let get_counter = |args: &[LValue], ctx: &dyn NativeContext| -> Result<LValue, LError> {
            if let Some(cnt) = ctx
                .get_component(TypeId::of::<u32>())
                .and_then(|x| x.downcast_ref::<u32>())
            {
                Ok(LValue::Number(LNumber::Int(*cnt as i64)))
            } else {
                Err(LError::SpecialError("No such component".to_string()))
            }
        };
        let set_counter =
            |args: &[LValue], ctx: &mut dyn NativeContext| -> Result<LValue, LError> {
                if let Some(cnt) = ctx
                    .get_component_mut(TypeId::of::<u32>())
                    .and_then(|x| x.downcast_mut::<u32>())
                {
                    *cnt = match args[0] {
                        LValue::Number(LNumber::Int(x)) => x as u32,
                        _ => panic!("type error"),
                    };
                    Ok(LValue::None)
                } else {
                    Err(LError::SpecialError("No such component".to_string()))
                }
            };

        let getter = LNativeLambda {
            fun: Rc::new(get_counter),
        };
        let getter = LFn::new(Box::new(|args: &[LValue], ctx: &u32| *ctx), "getter".to_string());
        let setter = LMutFn {
            fun: Rc::new(set_counter),
            debug_label: "setter".to_string(),
            index_mod: None
        };

        let mut state = Counter { cnt: 0 };
        assert_eq!(
            getter.call(&[], &state).ok().unwrap(),
            LValue::Number(LNumber::Int(0))
        );
        assert_eq!(
            setter
                .call(&[LValue::Number(LNumber::Int(5))], &mut state)
                .ok()
                .unwrap(),
            LValue::None
        );
        assert_eq!(
            getter.call(&[], &state).ok().unwrap(),
            LValue::Number(LNumber::Int(5))
        );
    }*/
}

/*
Module<()>
Module<Simu>

load_module() {
  add mod.ctx to NativeContext
  declare prelude
}*/
