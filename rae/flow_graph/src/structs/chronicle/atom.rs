use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::FormatWithSymTable;
use crate::structs::flow_graph::graph::{IF_PREFIX, RESULT_PREFIX, TIMEPOINT_PREFIX};
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::borrow::Borrow;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Atom {
    Bool(bool),
    Number(LNumber),
    Variable(Variable),
    Symbol(Symbol),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Symbol {
    Literal(String),
    SyntheticTask(SyntheticTask),
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Literal(l) => write!(f, "{}", l),
            Symbol::SyntheticTask(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum SyntheticTask {
    If(usize),
}

impl Display for SyntheticTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntheticTask::If(i) => write!(f, "_{}{}_", IF_PREFIX, i),
        }
    }
}

impl Atom {
    pub fn is_constant(&self) -> bool {
        !self.is_variable()
    }

    pub fn is_parameter(&self) -> bool {
        if let Self::Variable(Variable::Parameter(_)) = self {
            true
        } else {
            false
        }
    }

    pub fn is_variable(&self) -> bool {
        if let Self::Variable(_) = self {
            true
        } else {
            false
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Bool(bool) => write!(f, "{}", bool),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::Symbol(s) => write!(f, "{}", s),
            Atom::Variable(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Variable {
    Timepoint(usize),
    Result(usize),
    Parameter(String),
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Timepoint(n) => write!(f, "_{}{}_", TIMEPOINT_PREFIX, n),
            Variable::Result(n) => write!(f, "_{}{}_", RESULT_PREFIX, n),
            Variable::Parameter(p) => write!(f, "{}", p),
        }
    }
}

impl TryFrom<&Atom> for String {
    type Error = LRuntimeError;

    fn try_from(value: &Atom) -> Result<Self, Self::Error> {
        if let Atom::Symbol(s) = value {
            Ok(s.to_string())
        } else {
            Err(lruntimeerror!(
                "Sym::TryFrom<Atom>",
                format!(
                    "{}, expected {}",
                    KindLValue::Other(match value {
                        Atom::Bool(_) => "Atom::Bool".to_string(),
                        Atom::Number(_) => "Atom::Number".to_string(),
                        Atom::Symbol(_) => "Atom::Sym".to_string(),
                        Atom::Variable(_) => "Atom::Variable".to_string(),
                        //Atom::Type(_) => "Atom::Type".to_string(),
                    }),
                    KindLValue::Other("Atom::Sym".to_string()),
                )
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
        Self::Symbol(Symbol::Literal(s.to_string()))
    }
}

impl From<bool> for Atom {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl From<&LNumber> for Atom {
    fn from(n: &LNumber) -> Self {
        Self::Number(*n)
    }
}

impl From<LNumber> for Atom {
    fn from(n: LNumber) -> Self {
        n.borrow().into()
    }
}

impl From<i64> for Atom {
    fn from(i: i64) -> Self {
        Self::Number(i.into())
    }
}

impl From<f64> for Atom {
    fn from(f: f64) -> Self {
        Self::Number(f.into())
    }
}

impl FormatWithSymTable for Atom {
    fn format(&self, _: &RefSymTable, _: bool) -> String {
        self.to_string()
    }
}
