use crate::structs::interval::Interval;
use crate::structs::lit::Lit;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithSymTable, GetVariables};
use im::HashSet;

#[derive(Clone)]
pub struct Expression {
    pub interval: Interval,
    pub lit: Lit,
}

impl FormatWithSymTable for Expression {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {}",
            self.interval.format_with_sym_table(st),
            self.lit.format_with_sym_table(st)
        )
    }
}

impl GetVariables for Expression {
    fn get_variables(&self) -> HashSet<AtomId> {
        let hashet = self.interval.get_variables();
        hashet.union(self.lit.get_variables())
    }
}
