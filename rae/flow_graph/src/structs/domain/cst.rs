use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

impl Eq for Cst {}

#[derive(Debug, Clone, PartialEq)]
pub enum Cst {
    Int(i64),
    Float(f64),
    Boolean(bool),
    Symbol(String),
}

impl Hash for Cst {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Cst::Int(i) => i.hash(state),
            Cst::Float(f) => f.to_string().hash(state),
            Cst::Boolean(b) => b.hash(state),
            Cst::Symbol(s) => s.hash(state),
        }
    }
}

impl Display for Cst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Cst::Int(i) => write!(f, "{i}"),
            Cst::Float(fl) => write!(f, "{fl}"),
            Cst::Boolean(b) => write!(f, "{b}"),
            Cst::Symbol(s) => write!(f, "{s}"),
        }
    }
}
