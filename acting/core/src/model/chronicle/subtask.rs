use crate::model::chronicle::interval::Interval;
use crate::model::process_ref::Label;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::model::sym_table::VarId;
use im::HashSet;

#[derive(Clone, Debug)]
pub struct SubTask {
    pub interval: Interval,
    pub name: Vec<VarId>,
    pub result: VarId,
    pub label: Option<Label>,
}

impl FormatWithSymTable for SubTask {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        format!(
            "{} {} <- {}",
            self.interval.format(st, sym_version),
            self.result.format(st, sym_version),
            self.name.format(st, sym_version)
        )
    }
}

impl FlatBindings for SubTask {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.interval.flat_bindings(st);
        self.result.flat_bindings(st);
        self.name.flat_bindings(st);
    }
}

impl GetVariables for SubTask {
    fn get_variables(&self) -> HashSet<VarId> {
        let mut hashet = self.interval.get_variables();
        hashet.insert(self.result);
        hashet.union(self.name.iter().cloned().collect())
    }
}

impl Replace for SubTask {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.interval.replace(old, new);
        self.name.replace(old, new);
        self.result.replace(old, new);
    }
}
