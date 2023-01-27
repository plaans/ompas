use crate::aries::structs::symbol_table::{SymTable, VarId};
use crate::aries::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::aries::structs::type_table::PlanningAtomType;
use im::{hashset, HashSet};

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Interval {
    start: VarId,
    end: VarId,
}

impl Interval {
    pub fn new(start: &VarId, end: &VarId) -> Self {
        Self {
            start: *start,
            end: *end,
        }
    }

    pub fn new_instantaneous(t: &VarId) -> Self {
        Self { start: *t, end: *t }
    }
}

impl Interval {
    pub fn start(&self) -> &VarId {
        &self.start
    }

    pub fn end(&self) -> &VarId {
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
    fn get_variables(&self) -> HashSet<VarId> {
        hashset![self.start, self.end]
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}
