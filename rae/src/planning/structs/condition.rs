use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::interval::Interval;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{FormatWithSymTable, GetVariables};
use im::HashSet;

#[derive(Clone)]
pub struct Condition {
    pub interval: Interval,
    pub constraint: Constraint,
}

impl FormatWithSymTable for Condition {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.constraint.format_with_sym_table(st)
        )
    }
}

impl GetVariables for Condition {
    fn get_variables(&self) -> HashSet<AtomId> {
        let hashset = self.interval.get_variables();
        hashset.union(self.constraint.get_variables())
    }
}
