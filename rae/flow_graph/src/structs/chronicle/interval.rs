use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable, GetVariables, Replace};
use im::{hashset, HashSet};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

    pub fn get_start(&self) -> &AtomId {
        &self.start
    }

    pub fn get_end(&self) -> &AtomId {
        &self.end
    }

    pub fn set_end(&mut self, end: &AtomId) {
        self.end = *end;
    }

    pub fn is_instantaneous(&self) -> bool {
        self.start == self.end
    }
}

impl FormatWithSymTable for Interval {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        if self.start == self.end {
            format!("[{}]", self.start.format(st, sym_version))
        } else {
            format!(
                "[{},{}]",
                self.start.format(st, sym_version),
                self.end.format(st, sym_version)
            )
        }
    }
}

impl FlatBindings for Interval {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.start.flat_bindings(st);
        self.end.flat_bindings(st);
    }
}

impl GetVariables for Interval {
    fn get_variables(&self) -> HashSet<AtomId> {
        hashset![self.start, self.end]
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

impl Replace for Interval {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        self.end.replace(old, new);
        self.start.replace(old, new);
    }
}
