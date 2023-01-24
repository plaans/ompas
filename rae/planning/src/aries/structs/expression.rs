use crate::aries::structs::interval::Interval;
use crate::aries::structs::lit::Lit;
use crate::aries::structs::symbol_table::{SymTable, VarId};
use crate::aries::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::aries::structs::type_table::PlanningAtomType;
use im::HashSet;

#[derive(Clone)]
pub struct Expression {
    pub interval: Interval,
    pub lit: Lit,
}

impl FormatWithSymTable for Expression {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{} {}",
            self.interval.format(st, sym_version),
            self.lit.format(st, sym_version)
        )
    }
}

impl FormatWithParent for Expression {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.interval.format_with_parent(st);
        self.lit.format_with_parent(st);
    }
}

impl GetVariables for Expression {
    fn get_variables(&self) -> HashSet<VarId> {
        let hashet = self.interval.get_variables();
        hashet.union(self.lit.get_variables())
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}
