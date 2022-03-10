use crate::planning::structs::interval::Interval;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable, TypeId};
use crate::planning::structs::traits::{FormatWithSymTable, GetVariables};
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

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<TypeId>,
    ) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().parent_type == *atom_type)
            .cloned()
            .collect()
    }
}
