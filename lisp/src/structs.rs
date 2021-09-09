use crate::core::{eval, ContextCollection, LEnv};
use crate::language::scheme_primitives::*;
use crate::structs::LError::{ConversionError, SpecialError, WrongNumberOfArgument};
use im::HashMap;
use serde::{Deserialize, Serialize};
use std::any::Any;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Deref, Div, Mul, Range, Sub};
use std::sync::Arc;

/// Error struct for Scheme
/// Different kinds of errors are proposed, to have better explanation when one occurs:
/// - WrongType: the LValue kind is not the one expected
/// - NotInListOfExpectedTypes: a list of kind of LValue was expected.
/// - WrongNumberOfArgument: the number of args is not in expected range
/// - SpecialError: Other types of errors
/// - ConversionError: Error when trying a conversion that could fail.
/// # Example:
/// ```
/// use ompas_lisp::structs::LValue;
/// use std::convert::TryInto;
/// //The conversion will success if lv is of kind LValue::Map
/// let lv = LValue::Map(Default::default());
/// let map: im::HashMap<LValue, LValue>  = lv.try_into().expect("Could not convert LValue into HashMap");
///
/// ```
/// # Note:
/// The first argument of each kind is supposed to be an explanation of where the error occurred.
/// It can be the name of the function.
#[derive(Debug, Clone)]
pub enum LError {
    WrongType(&'static str, LValue, NameTypeLValue, NameTypeLValue),
    NotInListOfExpectedTypes(&'static str, LValue, NameTypeLValue, Vec<NameTypeLValue>),
    WrongNumberOfArgument(&'static str, LValue, usize, Range<usize>),
    //ErrLoc(ErrLoc),
    UndefinedSymbol(&'static str, String),
    SpecialError(&'static str, String),
    ConversionError(&'static str, NameTypeLValue, NameTypeLValue),
}

impl Display for LError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LError::WrongType(f_name, s, s1, s2) => {
                write!(f, "In {}, {}: Got {}, expected {}", f_name, s, s1, s2)
            }
            //LError::ErrLoc(e) => write!(f, "{}",e),
            LError::UndefinedSymbol(f_name, s) => write!(f, "In {}: {} is undefined", f_name, s),
            LError::WrongNumberOfArgument(f_name, s, g, r) => {
                if r.is_empty() {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected {}",
                        f_name, s, g, r.start
                    )
                } else if r.end == std::usize::MAX {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected at least {}",
                        f_name, s, g, r.start
                    )
                } else if r.start == std::usize::MIN {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected at most {}",
                        f_name, s, g, r.end
                    )
                } else {
                    write!(
                        f,
                        "In {}, \"{}\": Got {} element(s), expected between {} and {}",
                        f_name, s, g, r.start, r.end
                    )
                }
            }
            LError::SpecialError(f_name, s) => write!(f, "In {}, {}", f_name, s),
            LError::ConversionError(f_name, s1, s2) => {
                write!(f, "In {}, Cannot convert {} into {}.", f_name, s1, s2)
            }
            LError::NotInListOfExpectedTypes(f_name, lv, typ, list_types) => {
                write!(
                    f,
                    "In {}, {}: Got {}, expected {:?}",
                    f_name, lv, typ, list_types
                )
            }
        }
    }
}

impl From<std::io::Error> for LError {
    fn from(e: std::io::Error) -> Self {
        SpecialError("io", e.to_string())
    }
}

/*impl From<ErrLoc> for LError {
    fn from(e: ErrLoc) -> Self {
        LError::ErrLoc(e)
    }
}*/

/// Representation of numbers il LValue:
/// - Int(i64)
/// - Float(f64)
/// - Usize(usize)
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
            (n1, LNumber::Float(f2)) => LNumber::Float(f64::from(n1) / *f2),
            (LNumber::Float(f1), n2) => LNumber::Float(*f1 / f64::from(n2)),
            (n1, LNumber::Int(i2)) => LNumber::Int(i64::from(n1) / *i2),
            (LNumber::Int(i1), n2) => LNumber::Int(*i1 / i64::from(n2)),
            (n1, n2) => panic!("attempted rare case of division with {:?} and {:?}", n1, n2),
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

/// Kinds of args a LValue::LLambda could receive.
/// The parameters of a lambda function can be defined in two ways:
/// - a unique symbol considered as a list:
/// ``` lisp
/// (lambda args <body>)
/// ;args will be considered as a LValue::List
/// ;This lambda is expected to receive a list of arbitrary length.
/// ```
/// - a list of bound symbols :
/// ``` lisp
/// (lambda (x y z) <body>)
/// ;here each symbol will be bound to a LValue.
/// ;This lambda is expected to receive exactly three arguments.
#[derive(Clone, Debug)]
pub enum LambdaArgs {
    Sym(String),
    List(Vec<String>),
    Nil,
}

impl Display for LambdaArgs {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LambdaArgs::Sym(x) => write!(f, "{}", x),
            LambdaArgs::List(l) => {
                let mut s = String::from("(");
                for (i, e) in l.iter().enumerate() {
                    s.push_str(e);
                    if i != l.len() - 1 {
                        s.push(' ');
                    } else {
                        s.push(')');
                    }
                }
                write!(f, "{}", s)
            }
            LambdaArgs::Nil => write!(f, "nil"),
        }
    }
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

/// Struct to define a lambda in Scheme.
#[derive(Clone)]
pub struct LLambda {
    params: LambdaArgs,
    body: Box<LValue>,
    env: LEnv,
}

impl Debug for LLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?} : {:?}", self.params, self.body)
    }
}

impl Display for LLambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "(lambda {} {})", self.params, self.body)
    }
}

impl PartialEq for LLambda {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl LLambda {
    ///Constructs a new lambda, capturing the environment in which it has been created.
    pub fn new(params: LambdaArgs, body: LValue, env: LEnv) -> Self {
        LLambda {
            params,
            body: Box::new(body),
            env,
        }
    }

    /// Returns a new env containing the environment of the lambda and the current environment in which the lambda is called.
    pub fn get_new_env(&self, args: &[LValue], outer: LEnv) -> Result<LEnv, LError> {
        let mut env = self.env.clone();
        env.set_outer(outer);

        match &self.params {
            LambdaArgs::Sym(param) => {
                let arg = if args.len() == 1 {
                    match &args[0] {
                        LValue::List(_) | LValue::Nil => args[0].clone(),
                        _ => vec![args[0].clone()].into(),
                    }
                } else {
                    args.into()
                };
                env.insert(param.to_string(), arg);
            }
            LambdaArgs::List(params) => {
                if params.len() != args.len() {
                    return Err(WrongNumberOfArgument(
                        "get_new_env",
                        args.into(),
                        args.len(),
                        params.len()..params.len(),
                    ));
                }
                for (param, arg) in params.iter().zip(args) {
                    env.insert(param.to_string(), arg.clone());
                }
            }
            LambdaArgs::Nil => {
                if !args.is_empty() {
                    return Err(SpecialError(
                        "Lambda.get_env",
                        "Lambda was expecting no args.".to_string(),
                    ));
                }
            }
        };
        Ok(env)
    }

    /// Method to call a lambda and execute it.
    pub async fn call(
        &self,
        args: &[LValue],
        env: &LEnv,
        ctxs: &mut ContextCollection,
    ) -> Result<LValue, LError> {
        let mut new_env = self.get_new_env(args, env.clone())?;
        eval(&*self.body, &mut new_env, ctxs).await
    }

    /// Returns the body of the lambda
    pub fn get_body(&self) -> LValue {
        *self.body.clone()
    }
}

impl From<&LLambda> for LValue {
    fn from(l: &LLambda) -> Self {
        LValue::Lambda(l.clone())
    }
}

impl From<LLambda> for LValue {
    fn from(l: LLambda) -> Self {
        (&l).into()
    }
}

pub type NativeFn<T> = fn(&[LValue], &LEnv, &T) -> Result<LValue, LError>;
pub type DowncastCall =
    fn(&[LValue], &LEnv, &dyn Any, &Arc<dyn Any + Send + Sync>) -> Result<LValue, LError>;

/// Struct to define a pointer to a function.
/// Contains attributes used to downcast the pointer to the right type.
#[derive(Clone)]
pub struct LFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<DowncastCall>,
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
    /// Constructs a new LFn from a pointer to a function
    pub fn new<T: 'static + Sync + Send>(lbd: NativeFn<T>, debug_label: String) -> Self {
        /*let x = move |args: &[LValue], env: &LEnv, ctx: &dyn Any| -> Result<LValue, LError> {
            let ctx: Option<&T> = ctx.downcast_ref::<T>();
            if let Some(ctx) = ctx {
                lbd(args, env, ctx).into()
            } else {
                Err(LError::SpecialError(
                    "Impossible to downcast context".to_string(),
                ))
            }
        };*/
        let downcast_call = |args: &[LValue],
                             env: &LEnv,
                             ctx: &dyn Any,
                             fun: &Arc<dyn Any + Send + Sync>|
         -> Result<LValue, LError> {
            let ctx: &T = ctx.downcast_ref::<T>().ok_or_else(|| {
                LError::SpecialError("LFn::new", "Impossible to downcast context".to_string())
            })?;
            let fun: &NativeFn<T> = fun.downcast_ref::<NativeFn<T>>().ok_or_else(|| {
                LError::SpecialError("LFn::new", "Impossible to downcast function".to_string())
            })?;
            fun(args, env, ctx)
        };
        LFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    ///Calls the function
    pub fn call(&self, args: &[LValue], env: &LEnv, ctx: &dyn Any) -> Result<LValue, LError> {
        (self.downcast)(args, env, ctx, &self.fun)

        //(self.fun)(args, env, ctx)
    }

    /// Set the index of the module that is used by the function.
    /// Mandatory to use the right context when called.
    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    /// Return the index_mod of the function.
    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    /// Returns the label of the function
    pub fn get_label(&self) -> &'_ str {
        self.debug_label.as_str()
    }
}

pub type NativeMutFn<T> = fn(&[LValue], &LEnv, &mut T) -> Result<LValue, LError>;
pub type DowncastCallMut = fn(
    &[LValue],
    &LEnv,
    &mut dyn Any,
    &Arc<dyn Any + 'static + Send + Sync>,
) -> Result<LValue, LError>;

/// Struct wrapping a pointer to a function that can mutate its context.
#[derive(Clone)]
pub struct LMutFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<DowncastCallMut>,
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
    pub fn new<T: 'static>(lbd: NativeMutFn<T>, debug_label: String) -> Self {
        let downcast_call = |args: &[LValue],
                             env: &LEnv,
                             ctx: &mut dyn Any,
                             fun: &Arc<dyn Any + Send + Sync>|
         -> Result<LValue, LError> {
            let ctx: &mut T = ctx.downcast_mut::<T>().ok_or_else(|| {
                LError::SpecialError("LMutFn::new", "Impossible to downcast context".to_string())
            })?;
            let fun: &NativeMutFn<T> = fun.downcast_ref::<NativeMutFn<T>>().ok_or_else(|| {
                LError::SpecialError("LMutFn::new", "Impossible to downcast function".to_string())
            })?;

            fun(args, env, ctx)
        };
        LMutFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call(&self, args: &[LValue], env: &LEnv, ctx: &mut dyn Any) -> Result<LValue, LError> {
        (self.downcast)(args, env, ctx, &self.fun)
    }

    pub fn get_label(&self) -> &'_ str {
        self.debug_label.as_str()
    }
}

pub type DynFut<'a> =
    ::std::pin::Pin<Box<dyn 'a + Send + ::std::future::Future<Output = Result<LValue, LError>>>>;
pub type AsyncNativeFn<T> = for<'a> fn(&'a [LValue], &'a LEnv, &'a T) -> DynFut<'a>;
pub type AsyncDowncastCall =
    for<'a> fn(&'a [LValue], &'a LEnv, &'a dyn Any, &'a Arc<dyn Any + Send + Sync>) -> DynFut<'a>;

#[derive(Clone)]
pub struct LAsyncFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<AsyncDowncastCall>,
    index_mod: Option<usize>,
}

impl Debug for LAsyncFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LAsyncFn: {}\nindex_mod: {:?}\n",
            self.debug_label, self.index_mod
        )
    }
}

impl LAsyncFn {
    pub fn new<T: 'static>(lbd: AsyncNativeFn<T>, debug_label: String) -> Self {
        let downcast_call: AsyncDowncastCall = |args: &[LValue],
                                                env: &LEnv,
                                                ctx: &dyn Any,
                                                fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &T = ctx.downcast_ref::<T>().expect("could not downcast ctx");
            let fun: &AsyncNativeFn<T> = fun
                .downcast_ref::<AsyncNativeFn<T>>()
                .expect("could not downcast ctx");

            fun(args, env, ctx)
        };
        LAsyncFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call<'a>(
        &'a self,
        args: &'a [LValue],
        env: &'a LEnv,
        ctx: &'a AsyncLTrait,
    ) -> DynFut<'a> {
        (self.downcast)(args, env, ctx, &self.fun)
    }

    pub fn get_label(&self) -> &str {
        self.debug_label.as_str()
    }
}

pub type AsyncNativeMutFn<T> = for<'a> fn(&'a [LValue], &'a LEnv, &'a mut T) -> DynFut<'a>;
pub type AsyncDowncastCallMut = for<'a> fn(
    &'a [LValue],
    &'a LEnv,
    &'a mut dyn Any,
    &'a Arc<dyn Any + Send + Sync>,
) -> DynFut<'a>;

#[derive(Clone)]
pub struct LAsyncMutFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<AsyncDowncastCallMut>,
    index_mod: Option<usize>,
}

impl Debug for LAsyncMutFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LAsyncFn: {}\nindex_mod: {:?}\n",
            self.debug_label, self.index_mod
        )
    }
}

impl LAsyncMutFn {
    pub fn new<T: 'static>(lbd: AsyncNativeMutFn<T>, debug_label: String) -> Self {
        let downcast_call: AsyncDowncastCallMut = |args: &[LValue],
                                                   env: &LEnv,
                                                   ctx: &mut dyn Any,
                                                   fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &mut T = ctx.downcast_mut::<T>().expect("could not downcast mut ctx");
            let fun: &AsyncNativeMutFn<T> = fun
                .downcast_ref::<AsyncNativeMutFn<T>>()
                .expect("could not downcast asyncNativeMutFn");

            fun(args, env, ctx)
        };
        LAsyncMutFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call<'a>(
        &'a self,
        args: &'a [LValue],
        env: &'a LEnv,
        ctx: &'a mut AsyncLTrait,
    ) -> DynFut<'a> {
        (self.downcast)(args, env, ctx, &self.fun)
    }

    pub fn get_label(&self) -> &'_ str {
        self.debug_label.as_str()
    }
}

#[cfg(test)]
mod test_async {
    use crate::core::LEnv;
    use crate::structs::LError::{WrongNumberOfArgument, WrongType};
    use crate::structs::{
        AsyncDowncastCall, AsyncLTrait, AsyncNativeFn, DynFut, LAsyncFn, LAsyncMutFn, LError,
        LValue, NameTypeLValue,
    };
    use ::macro_rules_attribute::macro_rules_attribute;
    use ompas_utils::dyn_async;
    use std::any::Any;
    use std::sync::Arc;

    fn fun(i: u32) -> u32 {
        i * i
    }

    #[test]
    fn test_downcast() {
        let val: u32 = 5;

        let arc2: Arc<dyn Any> = Arc::new(val);

        let val_2 = arc2.downcast_ref::<u32>().unwrap();

        assert_eq!(val, *val_2)
    }

    #[test]
    fn test_downcast_fn() {
        let p_fun = fun;

        assert_eq!(p_fun(5), 25);
        let arc: Arc<dyn Any + 'static> = Arc::new(fun as fn(u32) -> u32);

        let p_fun: &fn(u32) -> u32 = arc.downcast_ref::<fn(u32) -> u32>().unwrap();

        assert_eq!(p_fun(5), 25)
    }

    fn test<'a>(_: &'a [LValue], _: &'a LEnv, _: &'a ()) -> DynFut<'a> {
        Box::pin(async move { Ok(LValue::Nil) })
    }
    #[macro_rules_attribute(dyn_async!)]
    async fn test_2<'a>(_: &'a [LValue], _: &'a LEnv, _: &'a ()) -> Result<LValue, LError> {
        Ok(LValue::Nil)
    }

    #[macro_rules_attribute(dyn_async!)]
    async fn test_computation_square<'a>(
        args: &'a [LValue],
        _: &'a LEnv,
        _: &'a (),
    ) -> Result<LValue, LError> {
        if args.len() != 1 {
            return Err(WrongNumberOfArgument(
                "test_computation_square",
                args.into(),
                args.len(),
                1..1,
            ));
        }

        if let LValue::Number(n) = &args[0] {
            Ok((n * n).into())
        } else {
            Err(WrongType(
                "test_computation_square",
                args[0].clone(),
                (&args[0]).into(),
                NameTypeLValue::Number,
            ))
        }
    }

    #[macro_rules_attribute(dyn_async!)]
    async fn test_computation_square_with_mut_ctx<'a>(
        args: &'a [LValue],
        _: &'a LEnv,
        _: &'a mut (),
    ) -> Result<LValue, LError> {
        if args.len() != 1 {
            return Err(WrongNumberOfArgument(
                "test_computation_square",
                args.into(),
                args.len(),
                1..1,
            ));
        }

        if let LValue::Number(n) = &args[0] {
            Ok((n * n).into())
        } else {
            Err(WrongType(
                "test_computation_square",
                args[0].clone(),
                (&args[0]).into(),
                NameTypeLValue::Number,
            ))
        }
    }

    #[tokio::test]
    async fn test_pointer_async() -> Result<(), LError> {
        let p_test = test;
        let result = p_test(&[LValue::Nil], &LEnv::empty(), &()).await?;

        assert_eq!(result, LValue::Nil);

        Ok(())
    }

    #[tokio::test]
    async fn test_arc_downcast_pointer_async() -> Result<(), LError> {
        let fun: Arc<dyn Any + 'static + Send + Sync> = Arc::new(test as AsyncNativeFn<()>);

        let fun: &AsyncNativeFn<()> = fun.downcast_ref::<AsyncNativeFn<()>>().unwrap();

        let env = &LEnv::empty();
        let args = &[LValue::Nil];
        let ctx = &();

        let result = fun(args, env, ctx).await?;

        assert_eq!(result, LValue::Nil);

        Ok(())
    }

    #[tokio::test]
    async fn test_arc_downcast_pointer_async_with_macro_def() -> Result<(), LError> {
        let fun: Arc<dyn Any + 'static + Send + Sync> = Arc::new(test_2 as AsyncNativeFn<()>);

        let fun: &AsyncNativeFn<()> = fun.downcast_ref::<AsyncNativeFn<()>>().unwrap();

        let env = &LEnv::empty();
        let args = &[LValue::Nil];
        let ctx = &();

        let result = fun(args, env, ctx).await?;

        assert_eq!(result, LValue::Nil);

        Ok(())
    }

    #[tokio::test]
    async fn test_downcast_call() {
        let fun: Arc<dyn Any + 'static + Send + Sync> = Arc::new(test_2 as AsyncNativeFn<()>);

        let downcast_call: AsyncDowncastCall = |args: &[LValue],
                                                env: &LEnv,
                                                ctx: &dyn Any,
                                                fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &() = ctx.downcast_ref::<()>().expect("could not downcast ctx");
            let fun: &AsyncNativeFn<()> = fun
                .downcast_ref::<AsyncNativeFn<()>>()
                .expect("could not downcast ctx");

            fun(args, env, ctx)
        };

        let env = &LEnv::empty();
        let args = &[LValue::Nil];
        let ctx = &();

        let result = downcast_call(args, env, ctx, &fun).await;
        if let Ok(result) = result {
            assert_eq!(result, LValue::Nil);
        }
    }

    #[tokio::test]
    async fn test_downcast_call_with_computation() {
        let fun: Arc<dyn Any + 'static + Send + Sync> =
            Arc::new(test_computation_square as AsyncNativeFn<()>);

        let downcast_call: AsyncDowncastCall = |args: &[LValue],
                                                env: &LEnv,
                                                ctx: &dyn Any,
                                                fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &() = ctx.downcast_ref::<()>().expect("could not downcast ctx");
            let fun: &AsyncNativeFn<()> = fun
                .downcast_ref::<AsyncNativeFn<()>>()
                .expect("could not downcast ctx");

            fun(args, env, ctx)
        };

        let env = &LEnv::empty();
        let args: &[LValue] = &[5.into()];
        let ctx = &();

        let result = downcast_call(args, env, ctx, &fun).await;
        if let Ok(result) = result {
            println!("result: {}", result);
            assert_eq!(result, LValue::from(25));
        }
    }

    #[tokio::test]
    async fn test_l_async_fn() {
        let fun = LAsyncFn::new(
            test_computation_square,
            "test_computation_square".to_string(),
        );

        let env = &LEnv::empty();
        let args: &[LValue] = &[5.into()];
        let ctx: &AsyncLTrait = &();

        let result = fun.call(args, env, ctx).await;
        if let Ok(result) = result {
            println!("result: {}", result);
            assert_eq!(result, LValue::from(25));
        }
    }

    #[tokio::test]
    async fn test_async_mut_fn() {
        let fun = LAsyncMutFn::new(
            test_computation_square_with_mut_ctx,
            "test_computation_square",
        );

        let env = &LEnv::empty();
        let args: &[LValue] = &[5.into()];
        let ctx: &mut AsyncLTrait = &mut ();

        let result = fun.call(args, env, ctx).await;
        if let Ok(result) = result {
            println!("result: {}", result);
            assert_eq!(result, LValue::from(25));
        }
    }
}

/// The core operators are Scheme operators that can modify the environment directly,
/// or have special behaviour that could not be done inside classical Scheme functions.
/// - Define: insert a new entry in the environment. A symbol can only be defined once.
/// - DefLambda: creates a new lambda object.
/// - If : basic conditional structure
/// - Quote : operator preventing from evaluating an expression
/// - QuasiQuote: operator preventing from evaluating an expression except unquote expression inside of it.
/// - Unquote : used only inside a QuasiQuote block to still evaluate an expression.
/// - DefMacro: insert a new macro in the environment. A macro must be a lambda.
/// - Set: modify the value of an entry in the environment. Cannot set an undefined symbol
/// - Begin: block that evaluates a list of expression and returns the last result.
/// - Async: Evaluates in an asynchronous task a LValue.
/// - Await: Wait on a pid the result of an async.
/// - Eval: Evaluates an expression.
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
    Async,
    Await,
    Eval,
}

impl Display for LCoreOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LCoreOperator::Define => write!(f, "{}", DEFINE),
            LCoreOperator::DefLambda => write!(f, "{}", LAMBDA),
            LCoreOperator::If => write!(f, "{}", IF),
            LCoreOperator::Quote => write!(f, "{}", QUOTE),
            LCoreOperator::QuasiQuote => write!(f, "{}", QUASI_QUOTE),
            LCoreOperator::UnQuote => write!(f, "{}", UNQUOTE),
            LCoreOperator::DefMacro => write!(f, "{}", DEF_MACRO),
            LCoreOperator::Set => write!(f, "{}", SET),
            LCoreOperator::Begin => write!(f, "{}", BEGIN),
            LCoreOperator::Async => write!(f, "{}", ASYNC),
            LCoreOperator::Await => write!(f, "{}", AWAIT),
            LCoreOperator::Eval => write!(f, "{}", EVAL),
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
            ASYNC => Ok(LCoreOperator::Async),
            AWAIT => Ok(LCoreOperator::Await),
            EVAL => Ok(LCoreOperator::Eval),
            _ => Err(SpecialError(
                "LCoreOperator::TryFrom<str>",
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

type Sym = String;
/// Object used in Scheme for every kind.
/// todo: complete documentation
#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(untagged, rename_all = "lowercase")]
pub enum LValue {
    // symbol
    Symbol(Sym),
    // literaux
    String(String),
    Character(char),
    Number(LNumber),
    #[serde(skip)]
    Fn(LFn),
    #[serde(skip)]
    MutFn(LMutFn),
    #[serde(skip)]
    AsyncFn(LAsyncFn),
    #[serde(skip)]
    AsyncMutFn(LAsyncMutFn),
    #[serde(skip)]
    Lambda(LLambda),
    #[serde(skip)]
    CoreOperator(LCoreOperator),

    // data structure
    #[serde(skip)]
    Map(im::HashMap<LValue, LValue>),
    List(Vec<LValue>),
    Quote(Box<LValue>),
    //Refers to boolean 'false and empty list in lisp
    True,
    Nil,
}
const TAB_SIZE: usize = 3;
const MAX_LENGTH: usize = 80;

impl LValue {
    fn pretty_print_list_aligned(keyword: &str, list: &[LValue], mut indent: usize) -> String {
        let mut string: String = format!("({}", keyword);
        indent += string.len() + 1;
        let mut first = true;
        for element in list {
            if first {
                first = false;
                string.push(' ');
                string.push_str(element.pretty_print(indent).as_str());
            } else {
                string.push_str(
                    format!("\n{}{}", " ".repeat(indent), element.pretty_print(indent)).as_str(),
                );
            }
        }
        string.push(')');
        string
    }

    fn pretty_print_list(list: &[LValue], indent: usize) -> String {
        let mut string = '('.to_string();

        let mut global_size = 0;
        let mut vec_pretty_printed = vec![];
        for element in list {
            let pretty_printed = element.pretty_print(indent + TAB_SIZE);
            global_size += pretty_printed.len();
            vec_pretty_printed.push(element.pretty_print(indent + TAB_SIZE));
        }

        if global_size < MAX_LENGTH {
            for (i, element) in vec_pretty_printed.iter().enumerate() {
                if i > 0 {
                    string.push(' ');
                }
                string.push_str(element.as_str());
            }
        } else {
            for (i, element) in vec_pretty_printed.iter().enumerate() {
                match i {
                    0 => {
                        string.push_str(element.as_str());
                    }
                    1 => {
                        string.push(' ');
                        string.push_str(element.as_str());
                    }
                    _ => string.push_str(
                        format!("\n{}{}", " ".repeat(indent + TAB_SIZE), element).as_str(),
                    ),
                }
            }
        }

        string.push(')');
        string
    }

    pub fn pretty_print(&self, indent: usize) -> String {
        match self {
            LValue::Lambda(l) => {
                format!(
                    "(lambda {}\n{}{}",
                    l.params,
                    " ".repeat(indent + TAB_SIZE),
                    l.body.pretty_print(indent + TAB_SIZE)
                )
            }
            LValue::List(list) => {
                if !list.is_empty() {
                    match &list[0] {
                        LValue::CoreOperator(LCoreOperator::Begin) => {
                            let indent = indent + TAB_SIZE;
                            let mut string = "(begin".to_string();
                            for element in &list[1..] {
                                string.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        element.pretty_print(indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                );
                            }
                            string.push(')');
                            string
                        }
                        LValue::CoreOperator(LCoreOperator::If) => {
                            LValue::pretty_print_list_aligned(IF, &list[1..], indent)
                        }
                        LValue::Symbol(s) => match s.as_str() {
                            LET | LET_STAR => {
                                let (mut string, indent) = match s.as_str() {
                                    LET =>  ("(let ".to_string(), indent + 5),
                                    LET_STAR => ("(let* ".to_string(), indent + 6),
                                    _ => unreachable!("The value of the atom has been checked before and should be let or let*.")
                                };
                                let bindings = &list[1];
                                let body = &list[2];

                                if let LValue::List(bindings) = bindings {
                                    string.push('(');
                                    for (i, binding) in bindings.iter().enumerate() {
                                        if i == 0 {
                                            string.push_str(
                                                binding
                                                    .pretty_print(indent + 1 + TAB_SIZE)
                                                    .as_str(),
                                            );
                                        } else {
                                            string.push_str(
                                                format!(
                                                    "\n{}{}",
                                                    " ".repeat(indent + 1),
                                                    binding.pretty_print(indent + 1 + TAB_SIZE)
                                                )
                                                .as_str(),
                                            );
                                        }
                                    }
                                    string.push(')')
                                } else {
                                    panic!("should be a list")
                                }
                                string.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        body.pretty_print(indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                );
                                string.push(')');
                                string
                            }
                            LAMBDA => {
                                let args = &list[1];
                                let body = &list[2];

                                format!(
                                    "(lambda {}\n{}{}",
                                    args.pretty_print(indent + TAB_SIZE),
                                    " ".repeat(indent + TAB_SIZE),
                                    body.pretty_print(indent + TAB_SIZE)
                                )
                            }
                            COND => LValue::pretty_print_list_aligned(COND, &list[1..], indent),
                            _ => LValue::pretty_print_list(list.as_slice(), indent),
                        },
                        _ => LValue::pretty_print_list(list.as_slice(), indent),
                    }
                } else {
                    NIL.to_string()
                }
            }
            LValue::Quote(lv) => format!("'{}", lv.pretty_print(indent)),
            lv => lv.to_string(),
        }
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::Fn(fun) => write!(f, "{}", fun.debug_label),
            LValue::MutFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::Nil => write!(f, "nil"),
            LValue::Symbol(s) | LValue::String(s) => write!(f, "{}", s),
            LValue::Number(n) => write!(f, "{}", n),
            LValue::True => write!(f, "true"),
            LValue::List(list) => {
                let mut result = String::new();
                result.push('(');
                for (i, element) in list.iter().enumerate() {
                    result.push_str(element.to_string().as_str());
                    if i != list.len() - 1 {
                        result.push(' ');
                    }
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
            LValue::Character(c) => write!(f, "{}", c),
            LValue::AsyncFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::AsyncMutFn(fun) => write!(f, "{}", fun.debug_label),
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

impl TryFrom<&LValue> for im::HashMap<LValue, LValue> {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Map(m) => Ok(m.clone()),
            _ => Err(ConversionError(
                "hashmap::tryfrom<&LValue>",
                value.into(),
                NameTypeLValue::Map,
            )),
        }
    }
}

impl TryFrom<LValue> for im::HashMap<LValue, LValue> {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

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
            lv => Err(ConversionError(
                "String::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::Symbol,
            )),
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
            lv => Err(ConversionError(
                "Vec<LValue>::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::List,
            )),
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
            lv => Err(ConversionError(
                "i64::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::Number,
            )),
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
            lv => Err(ConversionError(
                "f64::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::Number,
            )),
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
            lv => Err(ConversionError(
                "bool::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::Bool,
            )),
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
            lv => Err(ConversionError(
                "LCoreOperator::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::CoreOperator,
            )),
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
            lv => Err(ConversionError(
                "LLambda::tryfrom<&LValue>",
                lv.into(),
                NameTypeLValue::Lambda,
            )),
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
                "LValue::Add",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::Add",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::Add",
                format!(
                    "{} and {} cannot be add",
                    NameTypeLValue::from(l1),
                    NameTypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Sub for &LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 - n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::sub",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::sub",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::sub",
                format!(
                    "{} and {} cannot be add",
                    NameTypeLValue::from(l1),
                    NameTypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Mul for &LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 * n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::mul",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::mul",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::mul",
                format!(
                    "{} and {} cannot be add",
                    NameTypeLValue::from(l1),
                    NameTypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Div for &LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 / n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::div",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::div",
                l.clone(),
                l.into(),
                NameTypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::div",
                format!(
                    "{} and {} cannot be add",
                    NameTypeLValue::from(l1),
                    NameTypeLValue::from(l2)
                ),
            )),
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

impl From<&LNumber> for LValue {
    fn from(n: &LNumber) -> Self {
        LValue::Number(n.clone())
    }
}

impl From<LNumber> for LValue {
    fn from(n: LNumber) -> Self {
        (&n).into()
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
        (&s).into()
    }
}

impl From<&String> for LValue {
    fn from(s: &String) -> Self {
        LValue::Symbol(s.clone())
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

impl<T: Clone + Into<LValue>> From<&Vec<T>> for LValue {
    fn from(vec: &Vec<T>) -> Self {
        LValue::List(vec.iter().map(|x| x.clone().into()).collect())
    }
}

impl<T: Clone + Into<LValue>> From<Vec<T>> for LValue {
    fn from(vec: Vec<T>) -> Self {
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

impl Hash for LValueS {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValueS::Symbol(s) => (*s).hash(state),
            LValueS::Int(i) => (*i).hash(state),
            LValueS::Float(f) => (*f).to_string().hash(state),
            LValueS::Bool(b) => b.hash(state),
            LValueS::Map(m) => (*m).hash(state),
            LValueS::List(l) => {
                (*l).hash(state);
            }
        };
    }
}

impl PartialEq for LValueS {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LValueS::Int(i1), LValueS::Int(i2)) => *i1 == *i2,
            (LValueS::Symbol(s1), LValueS::Symbol(s2)) => *s1 == *s2,
            (LValueS::Bool(b1), LValueS::Bool(b2)) => b1 == b2,
            (LValueS::Float(f1), LValueS::Float(f2)) => *f1 == *f2,
            (LValueS::List(l1), LValueS::List(l2)) => *l1 == *l2,
            (LValueS::Map(m1), LValueS::Map(m2)) => *m1 == *m2,
            (_, _) => false,
        }
    }
}

impl Eq for LValueS {}

impl From<&LValue> for LValueS {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::Symbol(s) => LValueS::Symbol(s.clone()),
            LValue::Number(n) => match n {
                LNumber::Int(i) => LValueS::Int(*i),
                LNumber::Float(f) => LValueS::Float(*f),
                LNumber::Usize(u) => LValueS::Int(*u as i64),
            },
            LValue::Fn(f) => LValueS::Symbol(f.get_label().to_string()),
            LValue::MutFn(f) => LValueS::Symbol(f.get_label().to_string()),
            LValue::Lambda(_) => LValue::Nil.into(),
            LValue::CoreOperator(co) => LValueS::Symbol(co.to_string()),
            LValue::Map(m) => LValueS::Map(m.iter().map(|(k, v)| (k.into(), v.into())).collect()),
            LValue::List(l) => LValueS::List(l.iter().map(|lv| lv.into()).collect()),
            LValue::Quote(l) => l.deref().into(),
            LValue::True => LValueS::Symbol("true".to_string()),
            LValue::Nil => LValueS::Symbol("nil".to_string()),
            LValue::String(s) => LValueS::Symbol(s.clone()),
            LValue::Character(c) => LValueS::Symbol(c.to_string()),
            LValue::AsyncFn(fun) => LValueS::Symbol(fun.debug_label.to_string()),
            LValue::AsyncMutFn(fun) => LValueS::Symbol(fun.debug_label.to_string()),
        }
    }
}

impl From<LValue> for LValueS {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

impl From<&LValueS> for LValue {
    fn from(lvs: &LValueS) -> Self {
        match lvs {
            LValueS::Symbol(s) => LValue::Symbol(s.clone()),
            LValueS::Int(i) => LValue::Number(LNumber::Int(*i)),
            LValueS::Float(f) => LValue::Number(LNumber::Float(*f)),
            LValueS::Bool(b) => match b {
                true => LValue::True,
                false => LValue::Nil,
            },
            LValueS::List(l) => {
                if l.is_empty() {
                    LValue::Nil
                } else {
                    LValue::List(l.iter().map(|x| x.into()).collect())
                }
            }
            LValueS::Map(m) => {
                if m.is_empty() {
                    LValue::Nil
                } else {
                    let mut map: im::HashMap<LValue, LValue> = Default::default();
                    for (k, v) in m {
                        map.insert(k.into(), v.into());
                    }
                    LValue::Map(map)
                }
            }
        }
    }
}

impl From<LValueS> for LValue {
    fn from(lvs: LValueS) -> Self {
        (&lvs).into()
    }
}

impl Display for LValueS {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValueS::Symbol(s) => write!(f, "{}", s),
            LValueS::Int(i) => write!(f, "{}", *i),
            LValueS::Float(fl) => write!(f, "{}", fl),
            LValueS::Bool(b) => {
                write!(f, "{}", *b)
            }
            LValueS::List(l) => {
                let mut str = String::from("(");
                for e in l {
                    str.push_str(format!("{} ", e).as_str())
                }
                str.push(')');
                write!(f, "{}", str)
            }
            LValueS::Map(m) => {
                let mut str = String::from("(");
                for e in m {
                    str.push_str(format!("{} . {} ", e.0, e.1).as_str())
                }
                str.push(')');
                write!(f, "{}", str)
            }
        }
    }
}

/// Enum used to serialize LValue.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum LValueS {
    Symbol(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Vec<LValueS>),
    Map(Vec<(LValueS, LValueS)>),
}

/// Enum of kinds of LValue
/// Mainly used for debug and errors.
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
    Character,
    SExpr,
    Fn,
    MutFn,
    AsyncFn,
    AsyncMutFn,
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
            NameTypeLValue::MutFn => "MutFn",
            NameTypeLValue::Int => "int",
            NameTypeLValue::Float => "float",
            NameTypeLValue::Usize => "usize",
            NameTypeLValue::Bool => "bool",
            NameTypeLValue::Character => "character",
            NameTypeLValue::AsyncFn => "AsyncFn",
            NameTypeLValue::AsyncMutFn => "AsyncMutFn",
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
            (NameTypeLValue::AsyncFn, NameTypeLValue::AsyncFn) => true,
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
            LValue::String(_) => NameTypeLValue::String,
            LValue::Character(_) => NameTypeLValue::Character,
            LValue::AsyncFn(_) => NameTypeLValue::AsyncFn,
            LValue::AsyncMutFn(_) => NameTypeLValue::AsyncMutFn,
        }
    }
}

impl From<LValue> for NameTypeLValue {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}

#[derive(Default, Clone)]
pub struct InitLisp(Vec<String>);

//TODO: simplify to use only Vec<&'static str> instead of custom struct
impl<T: ToString> From<Vec<T>> for InitLisp {
    fn from(vec: Vec<T>) -> Self {
        InitLisp(vec.iter().map(|x| x.to_string()).collect())
    }
}

impl InitLisp {
    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0)
    }

    pub fn inner(&self) -> Vec<&str> {
        self.0.iter().map(|x| x.as_str()).collect()
    }

    pub fn begin_lisp(&self) -> String {
        let mut str = String::new(); //"(begin ".to_string();
        self.0.iter().for_each(|x| {
            str.push_str(x.as_str());
            str.push('\n');
        });
        str
    }
}

pub type AsyncLTrait = dyn Any + Send + Sync;

/// Struct to define a Module, Library that will be loaded inside the Scheme Environment.
pub struct Module {
    pub ctx: Arc<AsyncLTrait>,
    pub prelude: Vec<(String, LValue)>,
    pub raw_lisp: InitLisp,
    pub label: String,
}

impl Module {
    /// Add a function to the module.
    pub fn add_fn_prelude<T: 'static + Send + Sync, L: ToString>(
        &mut self,
        label: L,
        fun: NativeFn<T>,
    ) {
        self.prelude.push((
            label.to_string(),
            LValue::Fn(LFn::new(fun, label.to_string())),
        ))
    }

    /// Add a mutate function to the module.
    pub fn add_mut_fn_prelude<
        T: 'static,
        //R: Into<Result<LValue, LError>>,
        //F: Fn(&[LValue], &LEnv, &mut T) -> R + 'static,
        L: ToString,
    >(
        &mut self,
        label: L,
        fun: NativeMutFn<T>,
    ) {
        self.prelude.push((
            label.to_string(),
            LValue::MutFn(LMutFn::new(fun, label.to_string())),
        ))
    }

    pub fn add_async_fn_prelude<
        T: 'static,
        //R: Into<Result<LValue, LError>>,
        //F: Fn(&[LValue], &LEnv, &mut T) -> R + 'static,
        L: ToString,
    >(
        &mut self,
        label: L,
        fun: AsyncNativeFn<T>,
    ) {
        self.prelude.push((
            label.to_string(),
            LValue::AsyncFn(LAsyncFn::new(fun, label.to_string())),
        ))
    }
    pub fn add_async_mut_fn_prelude<
        T: 'static,
        //R: Into<Result<LValue, LError>>,
        //F: Fn(&[LValue], &LEnv, &mut T) -> R + 'static,
        L: ToString,
    >(
        &mut self,
        label: L,
        fun: AsyncNativeMutFn<T>,
    ) {
        self.prelude.push((
            label.to_string(),
            LValue::AsyncMutFn(LAsyncMutFn::new(fun, label.to_string())),
        ))
    }

    /// Add a LValue to the prelude.
    pub fn add_prelude(&mut self, label: &str, lv: LValue) {
        self.prelude.push((label.into(), lv));
    }
}

/// Trait that must be implemented by a context to build a Module object
/// that will be loaded into the LEnv and ContextCollection.
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

    /*#[tests]
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
