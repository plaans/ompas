use crate::conversion::chronicle::interval::Interval;
use crate::sym_table::domain::Domain;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::sym_table::VarId;
use im::HashSet;

#[derive(Clone)]
pub struct Effect {
    pub interval: Interval,
    pub sv: Vec<VarId>,
    pub value: VarId,
}

impl Effect {
    pub fn get_start(&self) -> VarId {
        self.interval.get_start()
    }

    pub fn get_end(&self) -> VarId {
        self.interval.get_end()
    }
}

impl FormatWithSymTable for Effect {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let sf = &self.sv[0];
        let params = &self.sv[1..];
        format!(
            "{} {}{} <- {}",
            self.interval.format(st, sym_version),
            sf.format(st, sym_version),
            params.format(st, sym_version),
            self.value.format(st, sym_version),
        )
    }
}

impl FlatBindings for Effect {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.interval.flat_bindings(st);
        self.sv.flat_bindings(st);
        self.value.flat_bindings(st);
    }
}

impl GetVariables for Effect {
    fn get_variables(&self) -> HashSet<VarId> {
        let mut union = self.interval.get_variables();
        self.sv.iter().for_each(|a| {
            union.insert(*a);
        });
        union.insert(self.value);
        union
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
            .cloned()
            .collect()
    }
}

impl Replace for Effect {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.sv.replace(old, new);
        self.value.replace(old, new);
        self.interval.replace(old, new);
    }
}
