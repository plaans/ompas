use crate::model::chronicle::lit::Lit;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::model::sym_table::VarId;
use im::HashSet;
use std::fmt::Write;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Computation {
    Add(Vec<Lit>),
    Sub(Vec<Lit>),
    Mul(Vec<Lit>),
    Div(Vec<Lit>),
}

impl Computation {
    pub fn add(mut vec: Vec<impl Into<Lit>>) -> Self {
        Self::Add(vec.drain(..).map(|e| e.into()).collect())
    }

    pub fn sub(mut vec: Vec<impl Into<Lit>>) -> Self {
        Self::Sub(vec.drain(..).map(|e| e.into()).collect())
    }

    pub fn mul(mut vec: Vec<impl Into<Lit>>) -> Self {
        Self::Mul(vec.drain(..).map(|e| e.into()).collect())
    }

    pub fn div(mut vec: Vec<impl Into<Lit>>) -> Self {
        Self::Div(vec.drain(..).map(|e| e.into()).collect())
    }
}

impl FormatWithSymTable for Computation {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let (symbol, lit) = match self {
            Computation::Add(add) => ("+", add),
            Computation::Sub(sub) => ("-", sub),
            Computation::Mul(mul) => ("*", mul),
            Computation::Div(div) => ("/", div),
        };

        let mut str = "".to_string();
        for (i, e) in lit.iter().enumerate() {
            if i != 0 {
                write!(str, " {} ", symbol).unwrap();
            }
            write!(str, "{}", e.format(st, sym_version)).unwrap();
        }
        str
    }
}

impl GetVariables for Computation {
    fn get_variables(&self) -> HashSet<VarId> {
        match self {
            Self::Add(vec) | Self::Sub(vec) | Self::Mul(vec) | Self::Div(vec) => {
                let mut set: HashSet<VarId> = Default::default();
                for e in vec {
                    set = set.union(e.get_variables())
                }
                set
            }
        }
    }
}

impl FlatBindings for Computation {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Self::Add(vec) | Self::Sub(vec) | Self::Mul(vec) | Self::Div(vec) => {
                for e in vec {
                    e.flat_bindings(st)
                }
            }
        }
    }
}

impl Replace for Computation {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        match self {
            Self::Add(vec) | Self::Sub(vec) | Self::Mul(vec) | Self::Div(vec) => {
                for e in vec {
                    e.replace(old, new)
                }
            }
        }
    }
}
