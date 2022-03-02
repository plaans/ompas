use crate::core::language::*;
use crate::core::structs::lcoreoperator::language::{LAMBDA, QUOTE};
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

/// Enum of kinds of LValue
/// Mainly used for debug and errors.
#[derive(Clone, Debug)]
pub enum TypeLValue {
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
    AsyncFn,
    Lambda,
    Nil,
    Map,
    List,
    Quote,
    Other(String),
    Future,
    Err,
}

impl Display for TypeLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let str = match self {
            TypeLValue::Number => NUMBER,
            TypeLValue::True => TRUE,
            TypeLValue::Symbol => SYMBOL,
            TypeLValue::String => STRING,
            TypeLValue::SExpr => SEXPR,
            TypeLValue::Fn => FN,
            TypeLValue::Nil => NIL,
            TypeLValue::Object => OBJECT,
            TypeLValue::Lambda => LAMBDA,
            TypeLValue::Map => MAP,
            TypeLValue::List => TYPE_LIST,
            TypeLValue::Quote => QUOTE,
            TypeLValue::Atom => ATOM,
            TypeLValue::CoreOperator => CORE_OPERATOR,
            TypeLValue::Other(s) => s.as_str(),
            TypeLValue::Int => INT,
            TypeLValue::Float => FLOAT,
            TypeLValue::Usize => USIZE,
            TypeLValue::Bool => BOOL,
            TypeLValue::Character => CHARACTER,
            TypeLValue::AsyncFn => ASYNC_FN,
            TypeLValue::Future => FUTURE,
            TypeLValue::Err => ERR,
        };
        write!(f, "{}", str)
    }
}

impl PartialEq for TypeLValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeLValue::String, TypeLValue::String) => true,
            (TypeLValue::SExpr, TypeLValue::SExpr) => true,
            (TypeLValue::True, TypeLValue::True) => true,
            (TypeLValue::Symbol, TypeLValue::Symbol) => true,
            (TypeLValue::Fn, TypeLValue::Fn) => true,
            (TypeLValue::Nil, TypeLValue::Nil) => true,
            (TypeLValue::Number, TypeLValue::Number) => true,
            (TypeLValue::Object, TypeLValue::Object) => true,
            (TypeLValue::Map, TypeLValue::Map) => true,
            (TypeLValue::List, TypeLValue::List) => true,
            (TypeLValue::Quote, TypeLValue::Quote) => true,
            (TypeLValue::Atom, TypeLValue::Atom) => true,
            (TypeLValue::CoreOperator, TypeLValue::CoreOperator) => true,
            (TypeLValue::Int, TypeLValue::Int) => true,
            (TypeLValue::Float, TypeLValue::Float) => true,
            (TypeLValue::Usize, TypeLValue::Usize) => true,
            (TypeLValue::AsyncFn, TypeLValue::AsyncFn) => true,
            (TypeLValue::Other(s1), TypeLValue::Other(s2)) => *s1 == *s2,
            (TypeLValue::Err, TypeLValue::Err) => true,
            (_, _) => false,
        }
    }
}

impl From<&LValue> for TypeLValue {
    fn from(lv: &LValue) -> Self {
        match lv {
            LValue::True => TypeLValue::True,
            LValue::Number(LNumber::Float(_)) => TypeLValue::Float,
            LValue::Number(LNumber::Int(_)) => TypeLValue::Int,
            LValue::Number(LNumber::Usize(_)) => TypeLValue::Usize,
            LValue::Symbol(_) => TypeLValue::Symbol,
            LValue::Fn(_) => TypeLValue::Fn,
            LValue::Nil => TypeLValue::Nil,
            LValue::Lambda(_) => TypeLValue::Lambda,
            LValue::Map(_) => TypeLValue::Map,
            LValue::List(_) => TypeLValue::List,
            LValue::CoreOperator(_) => TypeLValue::CoreOperator,
            LValue::String(_) => TypeLValue::String,
            LValue::Character(_) => TypeLValue::Character,
            LValue::AsyncFn(_) => TypeLValue::AsyncFn,
            LValue::Future(_) => TypeLValue::Future,
            LValue::Err(_) => TypeLValue::Err,
        }
    }
}

impl From<LValue> for TypeLValue {
    fn from(lv: LValue) -> Self {
        (&lv).into()
    }
}
