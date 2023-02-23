use crate::sym_table::domain::cst::Cst::*;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

impl Eq for Cst {}

#[derive(Debug, Clone)]
pub enum Cst {
    Int(i64),
    Float(f64),
    Symbol(String),
    Bool(bool),
}

impl From<Cst> for LValue {
    fn from(value: Cst) -> Self {
        match value {
            Cst::Int(i) => i.into(),
            Cst::Float(f) => f.into(),
            Cst::Symbol(s) => s.into(),
            Cst::Bool(b) => b.into(),
        }
    }
}

impl From<Cst> for LValueS {
    fn from(value: Cst) -> Self {
        match value {
            Cst::Int(i) => i.into(),
            Cst::Float(f) => f.into(),
            Cst::Symbol(s) => s.into(),
            Cst::Bool(b) => b.into(),
        }
    }
}

impl Cst {
    pub fn as_int(&self) -> Option<i64> {
        if let Self::Int(i) = self {
            Some(*i)
        } else {
            None
        }
    }

    pub fn as_float(&self) -> Option<f64> {
        if let Self::Float(f) = self {
            Some(*f)
        } else {
            None
        }
    }

    pub fn as_symbol(&self) -> Option<&str> {
        if let Self::Symbol(s) = self {
            Some(s.as_str())
        } else {
            None
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        if let Self::Bool(b) = self {
            Some(*b)
        } else {
            None
        }
    }
}

impl PartialEq for Cst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Int(i1), Int(i2)) => i1 == i2,
            (Float(f1), Float(f2)) => f1 == f2,
            (Symbol(s1), Symbol(s2)) => s1 == s2,
            (Bool(b1), Bool(b2)) => b1 == b2,
            _ => false,
        }
    }
}

impl Hash for Cst {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Int(i) => i.hash(state),
            Float(f) => f.to_string().hash(state),
            Symbol(s) => s.hash(state),
            Bool(b) => b.hash(state),
        }
    }
}

impl Display for Cst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Int(i) => write!(f, "{i}"),
            Float(fl) => write!(f, "{fl}"),
            Symbol(s) => write!(f, "{s}"),
            Bool(b) => write!(f, "{b}"),
        }
    }
}
