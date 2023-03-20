use crate::model::sym_domain::basic_type::BasicType;
use std::fmt::{Display, Formatter};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum SimpleType {
    Basic(BasicType),
    New(String),
}

impl From<BasicType> for SimpleType {
    fn from(r: BasicType) -> Self {
        Self::Basic(r)
    }
}

impl From<&str> for SimpleType {
    fn from(s: &str) -> Self {
        Self::New(s.to_string())
    }
}

impl From<String> for SimpleType {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

impl Display for SimpleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleType::Basic(rt) => write!(f, "{}", rt),
            SimpleType::New(s) => write!(f, "{}", s),
        }
    }
}
