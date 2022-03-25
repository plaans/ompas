use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::PlanningAtomType;
use im::{hashset, HashSet};

#[derive(Copy, Clone, PartialEq)]
pub struct Interval {
    start: AtomId,
    end: AtomId,
}

impl Interval {
    pub fn new(start: &AtomId, end: &AtomId) -> Self {
        Self {
            start: *start,
            end: *end,
        }
    }

    pub fn new_instantaneous(t: &AtomId) -> Self {
        Self { start: *t, end: *t }
    }
}

impl Interval {
    pub fn start(&self) -> &AtomId {
        &self.start
    }

    pub fn end(&self) -> &AtomId {
        &self.end
    }
}

impl FormatWithSymTable for Interval {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "[{},{}]",
            self.start.format(st, sym_version),
            self.end.format(st, sym_version),
        )
    }
}

impl FormatWithParent for Interval {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.start.format_with_parent(st);
        self.end.format_with_parent(st);
    }
}

impl GetVariables for Interval {
    fn get_variables(&self) -> HashSet<AtomId> {
        hashset![self.start, self.end]
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
