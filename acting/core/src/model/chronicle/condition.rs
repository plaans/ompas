use crate::model::chronicle::interval::Interval;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::model::sym_table::VarId;
use im::HashSet;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Condition {
    pub interval: Interval,
    pub sv: Vec<VarId>,
    pub value: VarId,
}

impl Condition {
    pub fn get_start(&self) -> VarId {
        self.interval.get_start()
    }

    pub fn get_end(&self) -> VarId {
        self.interval.get_end()
    }
}

impl FormatWithSymTable for Condition {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let sf = &self.sv[0];
        let params = &self.sv[1..];
        format!(
            "{} {}{} = {}",
            self.interval.format(st, sym_version),
            sf.format(st, sym_version),
            params.format(st, sym_version),
            self.value.format(st, sym_version),
        )
    }
}

impl FlatBindings for Condition {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.interval.flat_bindings(st);
        self.sv.flat_bindings(st);
        self.value.flat_bindings(st);
    }
}

impl GetVariables for Condition {
    fn get_variables(&self) -> HashSet<VarId> {
        let mut hashset = self.interval.get_variables();
        hashset.insert(self.value);
        self.sv.iter().for_each(|a| {
            hashset.insert(*a);
        });
        hashset
    }
}

impl Replace for Condition {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.sv.replace(old, new);
        self.value.replace(old, new);
        self.interval.replace(old, new);
    }
}
