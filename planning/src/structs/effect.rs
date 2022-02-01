use crate::structs::interval::Interval;
use crate::structs::symbol_table::SymTable;
use crate::structs::traits::FormatWithSymTable;
use crate::structs::transition::Transition;

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
