use crate::structs::interval::Interval;
use crate::structs::lit::Lit;
use crate::structs::symbol_table::SymTable;
use crate::structs::traits::FormatWithSymTable;

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
