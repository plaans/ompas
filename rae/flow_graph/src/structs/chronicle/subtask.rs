use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::sym_table::SymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, FormatWithParent, FormatWithSymTable, GetVariables};
use im::HashSet;

#[derive(Clone)]
pub struct SubTask {
    pub interval: Interval,
    pub lit: Lit,
}

impl FormatWithSymTable for SubTask {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{} {}",
            self.interval.format(st, sym_version),
            self.lit.format(st, sym_version)
        )
    }
}

impl FormatWithParent for SubTask {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.interval.format_with_parent(st);
        self.lit.format_with_parent(st);
    }
}

impl GetVariables for SubTask {
    fn get_variables(&self) -> HashSet<AtomId> {
        let hashet = self.interval.get_variables();
        hashet.union(self.lit.get_variables())
    }

    fn get_variables_of_type(&self, sym_table: &SymTable, atom_type: &AtomType) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap() == atom_type)
            .cloned()
            .collect()
    }
}
