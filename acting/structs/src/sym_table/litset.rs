use crate::sym_table::domain::Domain;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::sym_table::VarId;
use im::{hashset, HashSet};
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LitSet {
    Finite(Vec<VarId>),
    Domain(VarId),
}

impl FormatWithSymTable for LitSet {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        match self {
            LitSet::Finite(set) => {
                let mut str = "{".to_string();
                for (i, e) in set.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    write!(str, "{}", e.format(st, sym_version)).unwrap();
                }

                str.push('}');
                str
            }
            LitSet::Domain(d) => {
                format!("{{{}}}", d.format(st, sym_version))
            }
        }
    }
}

impl FlatBindings for LitSet {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        match self {
            LitSet::Finite(vec) => {
                vec.flat_bindings(st);
            }
            LitSet::Domain(d) => d.flat_bindings(st),
        }
    }
}

impl GetVariables for LitSet {
    fn get_variables(&self) -> HashSet<VarId> {
        match self {
            LitSet::Finite(set) => set.iter().cloned().collect(),
            LitSet::Domain(d) => {
                hashset![*d]
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

impl Replace for LitSet {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        match self {
            LitSet::Finite(set) => set.replace(old, new),
            LitSet::Domain(d) => d.replace(old, new),
        }
    }
}
