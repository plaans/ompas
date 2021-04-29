use crate::core::language::*;
use aries_utils::input::{ErrLoc, Sym};
use std::cmp::Ordering;
//use std::collections::HashMap;
use crate::core::structs::LError::WrongNumberOfArgument;
use crate::core::{eval, CtxCollec, RefLEnv};
use std::any::Any;
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

impl From<ErrLoc> for LError {
    fn from(e: ErrLoc) -> Self {
        LError::ErrLoc(e)
    }
}

#[derive(Debug, Clone)]
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

impl Into<Sym> for &LNumber {
    fn into(self) -> Sym {
        match self {
            LNumber::Int(i) => i.to_string().into(),
            LNumber::Float(f) => f.to_string().into(),
            LNumber::Usize(u) => u.to_string().into(),
        }
    }
}

impl Into<Sym> for LNumber {
    fn into(self) -> Sym {
        (&self).into()
    }
}

impl PartialEq for LNumber {
    fn eq(&self, other: &Self) -> bool {
        let n1: f64 = self.into();
        let n2: f64 = other.into();
        n1 == n2
    }
}

impl Into<usize> for &LNumber {
    fn into(self) -> usize {
        match self {
            LNumber::Int(i) => *i as usize,
            LNumber::Float(f) => *f as usize,
            LNumber::Usize(u) => *u,
        }
    }
}

impl Into<f64> for &LNumber {
    fn into(self) -> f64 {
        match self {
            LNumber::Int(i) => *i as f64,
            LNumber::Float(f) => *f,
            LNumber::Usize(u) => *u as f64,
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

impl Into<i64> for &LNumber {
    fn into(self) -> i64 {
        match self {
            LNumber::Int(i) => *i,
            LNumber::Float(f) => *f as i64,
            LNumber::Usize(u) => *u as i64,
        }
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
pub struct LLambda {
    params: Vec<Sym>,
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
    pub fn new(params: Vec<Sym>, body: LValue) -> Self {
        LLambda {
            params,
            body: Box::new(body),
        }
    }

    pub fn get_new_env(&self, args: &[LValue], outer: &RefLEnv) -> Result<RefLEnv, LError> {
        if self.params.len() != args.len() {
            return Err(WrongNumberOfArgument(
                LValue::List(args.to_vec()),
                args.len(),
                self.params.len()..self.params.len(),
            ));
        }
        let mut env = RefLEnv::empty();
        for (param, arg) in self.params.iter().zip(args) {
            env.symbols.insert(param.to_string(), arg.clone());
        }
        env.outer = Some(outer.clone());
        Ok(env)
    }

    pub fn call(
        &self,
        args: &[LValue],
        outer: &RefLEnv,
        ctxs: &mut CtxCollec,
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
    pub(crate) debug_label: String,
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
        debug_label: String,
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

    pub fn call(
        &self,
        args: &[LValue],
        env: &mut RefLEnv,
        ctx: &dyn Any,
    ) -> Result<LValue, LError> {
        (self.fun)(args, env, ctx)
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }
}

pub type NativeMutFn = dyn Fn(&[LValue], &mut RefLEnv, &mut dyn Any) -> Result<LValue, LError>;

#[derive(Clone)]
pub struct LMutFn {
    pub(crate) fun: Rc<NativeMutFn>,
    pub(crate) debug_label: String,
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
        debug_label: String,
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
}

#[derive(Clone, PartialOrd, PartialEq, Eq, Debug)]
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
    Fn(LFn),
    MutFn(LMutFn),
    Lambda(LLambda),
    CoreOperator(LCoreOperator),
}

impl Debug for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let string = match self {
            LValue::Symbol(s) => format!("Symbol: {:?}", s),
            LValue::Number(n) => format!("Number: {:?}", n),
            LValue::Bool(b) => format!("Boolean: {:?}", b),
            LValue::String(s) => format!("String: {:?}", s),
            LValue::Map(map) => format!("Map: {:?}", map),
            LValue::List(list) => format!("List: {:?}", list),
            LValue::Quote(q) => format!("Quote: {:?}", q),
            LValue::None => "LValue::None".to_string(),
            LValue::Fn(lfn) => format!("Function: {:?}", lfn),
            LValue::MutFn(mutfn) => format!("Number: {:?}", mutfn),
            LValue::Lambda(l) => format!("Lambda: {:?}", l),
            LValue::CoreOperator(co) => format!("CoreOperator: {:?}", co),
        };
        write!(f, "{}", string)
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::String(s) => write!(f, "{}", s),
            LValue::Fn(fun) => write!(f, "{}", fun.debug_label),
            LValue::MutFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::None => write!(f, "None"),
            LValue::Symbol(s) => write!(f, "{}", s),
            LValue::Number(n) => write!(f, "{}", n),
            LValue::Bool(b) => write!(f, "{}", b),
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
        LValue::Bool(b)
    }
}

impl From<String> for LValue {
    fn from(s: String) -> Self {
        LValue::String(s)
    }
}

#[derive(Clone, Debug)]
pub enum NameTypeLValue {
    CoreOperator,
    Atom,
    Object,
    Number,
    Int,
    Float,
    Usize,
    Bool,
    Symbol,
    String,
    SExpr,
    Fn,
    MutFn,
    Lambda,
    None,
    Map,
    List,
    Quote,
    Other(String),
}

impl Display for NameTypeLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            NameTypeLValue::Number => "Number",
            NameTypeLValue::Bool => "Boolean",
            NameTypeLValue::Symbol => "Symbol",
            NameTypeLValue::String => "String",
            NameTypeLValue::SExpr => "SExpr",
            NameTypeLValue::Fn => "Fn",
            NameTypeLValue::None => "None",
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
            (NameTypeLValue::Fn, NameTypeLValue::Fn) => true,
            (NameTypeLValue::None, NameTypeLValue::None) => true,
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
            LValue::Bool(_) => NameTypeLValue::Bool,
            LValue::Number(LNumber::Float(_)) => NameTypeLValue::Float,
            LValue::Number(LNumber::Int(_)) => NameTypeLValue::Int,
            LValue::Number(LNumber::Usize(_)) => NameTypeLValue::Usize,
            LValue::Symbol(_) => NameTypeLValue::Symbol,
            LValue::String(_) => NameTypeLValue::String,
            LValue::Fn(_) => NameTypeLValue::Fn,
            LValue::MutFn(_) => NameTypeLValue::MutFn,
            LValue::None => NameTypeLValue::None,
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

pub struct Module {
    pub ctx: Box<dyn Any>,
    pub prelude: Vec<(Sym, LValue)>,
}

pub trait AsModule {
    fn get_module() -> Module;
}

//TODO: Complete tests writing
#[cfg(test)]
mod tests {
    use super::*;

    mod l_number {
        use super::*;

        fn test_add() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert_eq!(LNumber::Int(8), &i1+&i2);
            assert_eq!(LNumber::Float(8.0), &i1+&f2);
            assert_eq!(LNumber::Float(8.0), &f1+&f2);
        }

        fn test_sub() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert_eq!(LNumber::Int(-2), &i1-&i2);
            assert_eq!(LNumber::Float(-2.0), &i1-&f2);
            assert_eq!(LNumber::Float(-2.0), &f1-&f2);
        }

        fn test_mul() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert_eq!(LNumber::Int(15), &i1*&i2);
            assert_eq!(LNumber::Float(15.0), &i1*&f2);
            assert_eq!(LNumber::Float(15.0), &f1*&f2);
        }

        fn test_div() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert_eq!(LNumber::Int(0), &i1/&i2);
            assert_eq!(LNumber::Float(0.6), &i1/&f2);
            assert_eq!(LNumber::Float(0.6), &f1/&f2);
        }

        #[test]
        fn test_math() {
            test_add();
            test_sub();
            test_div();
            test_mul();
        }

        fn test_gt() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert!(! (&i1 > &i2));
            assert!( &i2 > &i1);
            assert!( ! (&i2 > &i2));
            assert!(! (&f1 > &f2));
            assert!( &f2 > &f1);
            assert!( ! (&f2 > &f2));
            assert!( &i2 > &f1);
        }

        fn test_lt() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert!(&i1 < &i2);
            assert!( !(&i2 < &i1));
            assert!( !(&i2 < &i2));
            assert!( &f1 < &f2);
            assert!( !(&f2 < &f1));
            assert!( !(&f2 < &f2));
            assert!( &i1 < &f2);
        }

        fn test_ge() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert!(! (&i1 >= &i2));
            assert!( &i2 >= &i1);
            assert!( &i2 >= &i2);
            assert!( !(&f1 >= &f2));
            assert!( &f2 >= &f1);
            assert!( &f2 >= &f2);
            assert!( &i2 >= &f2);
        }

        fn test_le() {
            let i1:LNumber = 3.into();
            let i2:LNumber = 5.into();
            let f1:LNumber = 3.0.into();
            let f2:LNumber = 5.0.into();
            assert!(&i1 <= &i2);
            assert!( !(&i2 <= &i1));
            assert!( &i2 <= &i2);
            assert!( &f1 <= &f2);
            assert!( !(&f2 <= &f1));
            assert!( &f2 <= &f2);
            assert!( &i2 <= &f2);
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
            let i1:LValue = 3.into();
            let i2:LValue = 5.into();
            let f1:LValue = 3.0.into();
            let f2:LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(8)), (&i1+&i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(8.0)), (&i1+&f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(8.0)), (&f1+&f2).unwrap());
        }
        fn test_sub() {
            let i1:LValue = 3.into();
            let i2:LValue = 5.into();
            let f1:LValue = 3.0.into();
            let f2:LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(-2)), (&i1-&i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(-2.0)), (&i1-&f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(-2.0)), (&f1-&f2).unwrap());
        }

        fn test_mul() {
            let i1:LValue = 3.into();
            let i2:LValue = 5.into();
            let f1:LValue = 3.0.into();
            let f2:LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(15)), (&i1*&i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(15.0)), (&i1*&f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(15.0)), (&f1*&f2).unwrap());
        }

        fn test_div() {
            let i1:LValue = 3.into();
            let i2:LValue = 5.into();
            let f1:LValue = 3.0.into();
            let f2:LValue = 5.0.into();
            assert_eq!(LValue::Number(LNumber::Int(0)), (&i1/&i2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(0.6)), (&i1/&f2).unwrap());
            assert_eq!(LValue::Number(LNumber::Float(0.6)), (&f1/&f2).unwrap());
        }

        #[test]
        fn test_math() {
            test_add();
            test_sub();
            test_mul();
            test_div();
        }

        fn test_gt() {
            let i1:LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(!(&i1 > &f2));
            assert!( &f2 > &i1);
            assert!(!(&f2 > &f2));
        }

        fn test_ge() {
            let i1: LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(!(&i1 >= &f2));
            assert!( &f2 >= &i1);
            assert!(&f2 >= &f2);
        }

        fn test_lt() {
            let i1:LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(&i1 < &f2);
            assert!( !(&f2 < &i1));
            assert!(!(&f2 < &f2));
        }

        fn test_le() {
            let i1:LValue = 3.into();
            let f2: LValue = 5.0.into();
            assert!(&i1 <= &f2);
            assert!( !(&f2 <= &i1));
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