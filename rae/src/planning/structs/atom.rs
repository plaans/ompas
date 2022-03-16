use crate::planning::structs::symbol_table::SymTable;
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
    pub fn get_sym(&self) -> &String {
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

impl FormatWithSymTable for Atom {
    fn format_with_sym_table(&self, _: &SymTable, sym_version: bool) -> String {
        if let Self::Sym(s) = self {
            match sym_version {
                true => s.to_string(),
                false => s.get_sym().clone(),
            }
        } else {
            self.to_string()
        }
    }
}
