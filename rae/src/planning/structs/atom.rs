use crate::planning::structs::symbol_table::{SymTable, TypeId};
use crate::planning::structs::traits::FormatWithSymTable;
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
    //Type(AtomType),
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Bool(true) => write!(f, "true"),
            Atom::Bool(false) => write!(f, "nil"),
            Atom::Number(n) => write!(f, "{}", n),
            Atom::Sym(s) => write!(f, "{}", s),
            //Atom::Type(t) => write!(f, "{}", t),
        }
    }
}

/*impl From<&AtomType> for Atom {
    fn from(at: &AtomType) -> Self {
        Self::Type(at.clone())
    }
}*/

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
                    //Atom::Type(_) => "Atom::Type".to_string(),
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
#[derive(Copy, Clone, Debug, Hash, PartialEq, PartialOrd, Eq)]
pub enum PlanningAtomType {
    Action,
    StateFunction,
    Method,
    Task,
    Timepoint,
    Int,
    Float,
    Bool,
    Symbol,
    Function,
    Lambda,
}

impl Display for PlanningAtomType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PlanningAtomType::Action => "action",
                PlanningAtomType::StateFunction => "state-function",
                PlanningAtomType::Method => "method",
                PlanningAtomType::Task => "task",
                PlanningAtomType::Timepoint => "timepoint",
                PlanningAtomType::Int => "int",
                PlanningAtomType::Float => "float",
                PlanningAtomType::Bool => "boolean",
                PlanningAtomType::Symbol => "symbol",
                PlanningAtomType::Function => "function",
                PlanningAtomType::Lambda => "lambda",
            }
        )
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum AtomKind {
    Type,
    Constant,
    Result,
    Variable,
}

impl Display for AtomKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AtomKind::Type => "type",
                AtomKind::Result => "result",
                AtomKind::Constant => "constant",
                AtomKind::Variable => "variable",
            }
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct AtomType {
    pub parent_type: Option<TypeId>,
    pub kind: AtomKind,
}
impl FormatWithSymTable for AtomType {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{}{}",
            match &self.parent_type {
                Some(s) => format!("{} ", st.get_sym(s)),
                None => "".to_string(),
            },
            self.kind
        )
    }
}
