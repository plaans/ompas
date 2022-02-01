use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithSymTable, GetVariables};
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
}

impl Interval {
    pub fn start(&self) -> AtomId {
        self.start
    }

    pub fn end(&self) -> AtomId {
        self.end
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
}
