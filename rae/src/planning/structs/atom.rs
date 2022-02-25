use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::ConversionError;
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Atom {
    Bool(bool),
    Number(LNumber),
    Sym(Sym),
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Bool(true) => write!(f, "true"),
            Atom::Bool(false) => write!(f, "nil"),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::Sym(s) => write!(f, "{}", s),
        }
    }
}

impl TryFrom<&Atom> for Sym {
    type Error = LError;

    fn try_from(value: &Atom) -> Result<Self, Self::Error> {
        if let Atom::Sym(s) = value {
            Ok(s.clone())
        } else {
            Err(ConversionError(
                "Sym::TryFrom<Atom>",
                TypeLValue::Other(match value {
                    Atom::Bool(_) => "Atom::Bool".to_string(),
                    Atom::Number(_) => "Atom::Number".to_string(),
                    Atom::Sym(_) => "Atom::Sym".to_string(),
                }),
                TypeLValue::Other("Atom::Sym".to_string()),
            ))
        }
    }
}

impl Default for Atom {
    fn default() -> Self {
        Self::Bool(false)
    }
}

impl From<&str> for Atom {
    fn from(s: &str) -> Self {
        Self::Sym(s.into())
    }
}

impl From<bool> for Atom {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<&LNumber> for Atom {
    fn from(n: &LNumber) -> Self {
        Self::Number(n.clone())
    }
}

impl From<LNumber> for Atom {
    fn from(n: LNumber) -> Self {
        (&n).into()
    }
}

impl From<&Sym> for Atom {
    fn from(sym: &Sym) -> Self {
        Self::Sym(sym.clone())
    }
}

impl From<Sym> for Atom {
    fn from(sym: Sym) -> Self {
        (&sym).into()
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum Sym {
    Unique(String),
    Several(String, usize),
}

impl Default for Sym {
    fn default() -> Self {
        Self::Unique("".to_string())
    }
}

impl Sym {
    pub fn get_string(&self) -> &String {
        match self {
            Sym::Unique(s) => s,
            Sym::Several(s, _) => s,
        }
    }
}

impl From<String> for Sym {
    fn from(s: String) -> Self {
        Self::Unique(s)
    }
}

impl From<&str> for Sym {
    fn from(s: &str) -> Self {
        Self::Unique(s.to_string())
    }
}

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unique(s) => write!(f, "{}", s),
            Self::Several(s, i) => write!(f, "{}_{}", s, i),
        }
    }
}

#[derive(Clone, Copy, PartialOrd, PartialEq, Eq, Ord, Hash)]
pub enum AtomType {
    Number,
    Boolean,
    Timepoint,
    Result,
    Symbol,
    Variable,
    Action,
    StateFunction,
    Method,
    Task,
    Function,
    Lambda,
}

impl Display for AtomType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AtomType::Timepoint => write!(f, "timepoint"),
            AtomType::Result => write!(f, "return"),
            AtomType::Action => write!(f, "action"),
            AtomType::StateFunction => write!(f, "state-function"),
            AtomType::Method => write!(f, "method"),
            AtomType::Task => write!(f, "task"),
            AtomType::Function => write!(f, "function"),
            AtomType::Number => write!(f, "number"),
            AtomType::Boolean => write!(f, "boolean"),
            AtomType::Symbol => write!(f, "symbol"),
            AtomType::Lambda => write!(f, "lambda"),
            AtomType::Variable => write!(f, "variable"),
        }
    }
}
