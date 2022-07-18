use crate::lnumber::LNumber;
use crate::lvalue::LValue;
use sompas_language::*;
use std::fmt::{Display, Formatter};

/// Enum of kinds of LValue
/// Mainly used for debug and errors.
#[derive(Clone, Debug)]
pub enum KindLValue {
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
    AsyncFn,
    Lambda,
    Nil,
    Map,
    List,
    Quote,
    Other(String),
    Handler,
    Err,
}

impl Display for KindLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            KindLValue::Number => NUMBER,
            KindLValue::True => TRUE,
            KindLValue::Symbol => SYMBOL,
            KindLValue::String => STRING,
            KindLValue::SExpr => SEXPR,
            KindLValue::Fn => FN,
            KindLValue::Nil => NIL,
            KindLValue::Object => OBJECT,
            KindLValue::Lambda => LAMBDA,
            KindLValue::Map => MAP,
            KindLValue::List => LIST,
            KindLValue::Quote => QUOTE,
            KindLValue::Atom => ATOM,
            KindLValue::CoreOperator => CORE_OPERATOR,
            KindLValue::Other(s) => s.as_str(),
            KindLValue::Int => INT,
            KindLValue::Float => FLOAT,
            KindLValue::Usize => USIZE,
            KindLValue::Bool => BOOL,
            KindLValue::AsyncFn => ASYNC_FN,
            KindLValue::Handler => HANDLER,
            KindLValue::Err => ERR,
        };
        write!(f, "{}", str)
    }
}

impl PartialEq for KindLValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (KindLValue::String, KindLValue::String) => true,
            (KindLValue::SExpr, KindLValue::SExpr) => true,
            (KindLValue::True, KindLValue::True) => true,
            (KindLValue::Symbol, KindLValue::Symbol) => true,
            (KindLValue::Fn, KindLValue::Fn) => true,
            (KindLValue::Nil, KindLValue::Nil) => true,
            (KindLValue::Number, KindLValue::Number) => true,
            (KindLValue::Object, KindLValue::Object) => true,
            (KindLValue::Map, KindLValue::Map) => true,
            (KindLValue::List, KindLValue::List) => true,
            (KindLValue::Quote, KindLValue::Quote) => true,
            (KindLValue::Atom, KindLValue::Atom) => true,
            (KindLValue::CoreOperator, KindLValue::CoreOperator) => true,
            (KindLValue::Int, KindLValue::Int) => true,
            (KindLValue::Float, KindLValue::Float) => true,
            (KindLValue::Usize, KindLValue::Usize) => true,
            (KindLValue::AsyncFn, KindLValue::AsyncFn) => true,
            (KindLValue::Other(s1), KindLValue::Other(s2)) => *s1 == *s2,
            (KindLValue::Err, KindLValue::Err) => true,
            (_, _) => false,
        }
    }
}

impl From<&LValue> for KindLValue {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::True => KindLValue::True,
            LValue::Number(LNumber::Float(_)) => KindLValue::Float,
            LValue::Number(LNumber::Int(_)) => KindLValue::Int,
            LValue::Symbol(_) => KindLValue::Symbol,
            LValue::Fn(_) => KindLValue::Fn,
            LValue::Nil => KindLValue::Nil,
            LValue::Lambda(_) => KindLValue::Lambda,
            LValue::Map(_) => KindLValue::Map,
            LValue::List(_) => KindLValue::List,
            LValue::CoreOperator(_) => KindLValue::CoreOperator,
            LValue::String(_) => KindLValue::String,
            LValue::AsyncFn(_) => KindLValue::AsyncFn,
            LValue::Handler(_) => KindLValue::Handler,
            LValue::Err(_) => KindLValue::Err,
        }
    }
}

impl From<LValue> for KindLValue {
    fn from(lv: LValue) -> Self {
        lv.into()
    }
}
