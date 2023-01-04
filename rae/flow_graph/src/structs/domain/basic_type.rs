use crate::structs::domain::root_type::RootType;
use std::fmt::{Display, Formatter};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum BasicType {
    RootType(RootType),
    New(String),
}

impl From<RootType> for BasicType {
    fn from(r: RootType) -> Self {
        Self::RootType(r)
    }
}

impl From<&str> for BasicType {
    fn from(s: &str) -> Self {
        Self::New(s.to_string())
    }
}

impl From<String> for BasicType {
    fn from(s: String) -> Self {
        s.as_str().into()
    }
}

impl Display for BasicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicType::RootType(rt) => write!(f, "{}", rt),
            BasicType::New(s) => write!(f, "{}", s),
        }
    }
}
