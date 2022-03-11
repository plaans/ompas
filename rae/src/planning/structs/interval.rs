use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::PlanningAtomType;
use im::{hashset, HashSet};

#[derive(Copy, Clone)]
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
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!("[{},{}]", st.get_sym(&self.start), st.get_sym(&self.end),)
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
