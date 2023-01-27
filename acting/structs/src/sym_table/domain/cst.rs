use crate::sym_table::domain::cst::Cst::*;
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
