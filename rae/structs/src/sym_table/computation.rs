use crate::sym_table::domain::Domain;
use crate::sym_table::lit::Lit;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::sym_table::VarId;
use im::HashSet;
use std::fmt::Write;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Computation {
    Add(Vec<Lit>),
    Sub(Vec<Lit>),
}

impl Computation {
    pub fn add(mut vec: Vec<impl Into<Lit>>) -> Self {
        Self::Add(vec.drain(..).map(|e| e.into()).collect())
    }

    pub fn sub(mut vec: Vec<impl Into<Lit>>) -> Self {
        Self::Sub(vec.drain(..).map(|e| e.into()).collect())
    }
}

impl FormatWithSymTable for Computation {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        match self {
            Computation::Add(add) => {
                let mut str = "".to_string();
                for (i, e) in add.iter().enumerate() {
                    if i != 0 {
                        write!(str, " + ").unwrap();
                    }
                    write!(str, "{}", e.format(st, sym_version)).unwrap();
                }
                str
            }
            Computation::Sub(sub) => {
                let mut str = "".to_string();
                for (i, e) in sub.iter().enumerate() {
                    if i != 0 {
                        write!(str, " - ").unwrap();
                    }
                    write!(str, "{}", e.format(st, sym_version)).unwrap();
                }
                str
            }
        }
    }
}

impl GetVariables for Computation {
    fn get_variables(&self) -> HashSet<VarId> {
        match self {
            Self::Add(vec) | Self::Sub(vec) => {
                let mut set: HashSet<VarId> = Default::default();
                for e in vec {
                    set = set.union(e.get_variables())
                }
                set
            }
        }
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
            .cloned()
            .collect()
    }
}

impl FlatBindings for Computation {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            Self::Add(vec) | Self::Sub(vec) => {
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
            Self::Add(vec) | Self::Sub(vec) => {
                for e in vec {
                    e.replace(old, new)
                }
            }
        }
    }
}
