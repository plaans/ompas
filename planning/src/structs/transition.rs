use crate::structs::lit::Lit;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{FormatWithSymTable, GetVariables};
use im::HashSet;

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

impl GetVariables for Transition {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.variable
            .get_variables()
            .union(self.value.get_variables())
    }
}
