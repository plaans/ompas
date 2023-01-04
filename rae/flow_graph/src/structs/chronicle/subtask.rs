use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::structs::domain::Domain;
use crate::structs::sym_table::r#ref::RefSymTable;
use im::HashSet;

#[derive(Clone)]
pub struct SubTask {
    pub interval: Interval,
    pub lit: Lit,
    pub result: AtomId,
}

impl FormatWithSymTable for SubTask {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        format!(
            "{} {} <- {}",
            self.interval.format(st, sym_version),
            self.result.format(st, sym_version),
            self.lit.format(st, sym_version)
        )
    }
}

impl FlatBindings for SubTask {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.interval.flat_bindings(st);
        self.result.flat_bindings(st);
        self.lit.flat_bindings(st);
    }
}

impl GetVariables for SubTask {
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut hashet = self.interval.get_variables();
        hashet.insert(self.result);
        hashet.union(self.lit.get_variables())
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| {
                sym_table.contained_in_domain(&sym_table.get_domain(v, true).unwrap(), domain)
            })
            .cloned()
            .collect()
    }
}

impl Replace for SubTask {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        self.interval.replace(old, new);
        self.lit.replace(old, new);
        self.result.replace(old, new);
    }
}
