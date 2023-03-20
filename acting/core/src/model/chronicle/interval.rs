use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::model::sym_table::VarId;
use im::{hashset, HashSet};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Interval {
    start: VarId,
    end: VarId,
}

impl Interval {
    pub fn new(start: VarId, end: VarId) -> Self {
        Self { start, end }
    }

    pub fn new_instantaneous(t: VarId) -> Self {
        Self { start: t, end: t }
    }

    pub fn get_start(&self) -> VarId {
        self.start
    }

    pub fn get_end(&self) -> VarId {
        self.end
    }

    pub fn set_end(&mut self, end: VarId) {
        self.end = end;
    }

    pub fn is_instantaneous(&self) -> bool {
        self.start == self.end
    }
}

impl FormatWithSymTable for Interval {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        if self.start == self.end {
            format!("[{}]", self.start.format(st, sym_version))
        } else {
            format!(
                "[{},{}]",
                self.start.format(st, sym_version),
                self.end.format(st, sym_version)
            )
        }
    }
}

impl FlatBindings for Interval {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.start.flat_bindings(st);
        self.end.flat_bindings(st);
    }
}

impl GetVariables for Interval {
    fn get_variables(&self) -> HashSet<VarId> {
        hashset![self.start, self.end]
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
            .cloned()
            .collect()
    }
}

impl Replace for Interval {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.end.replace(old, new);
        self.start.replace(old, new);
    }
}
