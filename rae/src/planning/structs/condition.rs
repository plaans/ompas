use crate::planning::structs::interval::Interval;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::PlanningAtomType;
use im::HashSet;

#[derive(Clone, PartialEq)]
pub struct Condition {
    pub interval: Interval,
    pub sv: Vec<AtomId>,
    pub value: AtomId,
}

impl Condition {
    pub fn get_start(&self) -> &AtomId {
        self.interval.start()
    }

    pub fn get_end(&self) -> &AtomId {
        self.interval.end()
    }
}

impl FormatWithSymTable for Condition {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{} {} = {}",
            self.interval.format(st, sym_version),
            self.sv.format(st, sym_version),
            self.value.format(st, sym_version),
        )
    }
}

impl FormatWithParent for Condition {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.interval.format_with_parent(st);
        self.sv.format_with_parent(st);
        self.value.format_with_parent(st);
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

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}
