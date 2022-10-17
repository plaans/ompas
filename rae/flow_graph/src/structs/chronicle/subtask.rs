use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable, GetVariables, Replace};
use im::HashSet;

#[derive(Clone)]
pub struct SubTask {
    pub interval: Interval,
    pub lit: Lit,
}

impl FormatWithSymTable for SubTask {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        format!(
            "{} {}",
            self.interval.format(st, sym_version),
            self.lit.format(st, sym_version)
        )
    }
}

impl FlatBindings for SubTask {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.interval.flat_bindings(st);
        self.lit.flat_bindings(st);
    }
}

impl GetVariables for SubTask {
    fn get_variables(&self) -> HashSet<AtomId> {
        let hashet = self.interval.get_variables();
        hashet.union(self.lit.get_variables())
    }

    fn get_variables_of_type(
        &self,
        sym_table: &RefSymTable,
        atom_type: &AtomType,
    ) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v) == *atom_type)
            .cloned()
            .collect()
    }
}

impl Replace for SubTask {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        self.interval.replace(old, new);
        self.lit.replace(old, new)
    }
}
