use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::structs::domain::Domain;
use crate::structs::sym_table::r#ref::RefSymTable;
use im::HashSet;

#[derive(Clone, Eq, PartialEq)]
pub struct Condition {
    pub interval: Interval,
    pub sv: Vec<AtomId>,
    pub value: AtomId,
}

impl Condition {
    pub fn get_start(&self) -> &AtomId {
        self.interval.get_start()
    }

    pub fn get_end(&self) -> &AtomId {
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
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut hashset = self.interval.get_variables();
        hashset.insert(self.value);
        self.sv.iter().for_each(|a| {
            hashset.insert(*a);
        });
        hashset
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| {
                sym_table.contained_in_domain(&sym_table.get_domain(v, true).unwrap(), &domain)
            })
            .cloned()
            .collect()
    }
}

impl Replace for Condition {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        self.sv.replace(old, new);
        self.value.replace(old, new);
        self.interval.replace(old, new);
    }
}
