use crate::conversion::chronicle::interval::Interval;
use crate::supervisor::process::process_ref::Label;
use crate::sym_table::domain::Domain;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::sym_table::VarId;
use im::HashSet;

#[derive(Clone)]
pub struct SubTask {
    pub interval: Interval,
    pub task: Vec<VarId>,
    pub result: VarId,
    pub label: Option<Label>,
}

impl FormatWithSymTable for SubTask {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        format!(
            "{} {} <- {}",
            self.interval.format(st, sym_version),
            self.result.format(st, sym_version),
            self.task.format(st, sym_version)
        )
    }
}

impl FlatBindings for SubTask {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.interval.flat_bindings(st);
        self.result.flat_bindings(st);
        self.task.flat_bindings(st);
    }
}

impl GetVariables for SubTask {
    fn get_variables(&self) -> HashSet<VarId> {
        let mut hashet = self.interval.get_variables();
        hashet.insert(self.result);
        hashet.union(self.task.iter().cloned().collect())
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
            .cloned()
            .collect()
    }
}

impl Replace for SubTask {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.interval.replace(old, new);
        self.task.replace(old, new);
        self.result.replace(old, new);
    }
}
