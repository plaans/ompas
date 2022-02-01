use crate::structs::lit::Lit;
use crate::structs::symbol_table::SymTable;
use crate::structs::traits::FormatWithSymTable;

#[derive(Clone)]
pub struct Transition {
    variable: Lit,
    value: Lit,
}

impl Transition {
    pub fn new(var: Lit, val: Lit) -> Self {
        Self {
            variable: var,
            value: val,
        }
    }
}

impl FormatWithSymTable for Transition {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} <- {}",
            self.variable.format_with_sym_table(st),
            self.value.format_with_sym_table(st)
        )
    }
}
