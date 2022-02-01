use crate::structs::interval::Interval;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithSymTable, GetVariables};
use crate::structs::transition::Transition;
use im::HashSet;

#[derive(Clone)]
pub struct Effect {
    pub interval: Interval,
    pub transition: Transition,
}

impl FormatWithSymTable for Effect {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.transition.format_with_sym_table(st)
        )
    }
}

impl GetVariables for Effect {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.interval
            .get_variables()
            .union(self.transition.get_variables())
    }
}
