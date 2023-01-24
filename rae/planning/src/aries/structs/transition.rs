use crate::aries::structs::lit::Lit;
use crate::aries::structs::symbol_table::{SymTable, VarId};
use crate::aries::structs::traits::{FormatWithParent, FormatWithSymTable, GetVariables};
use crate::aries::structs::type_table::PlanningAtomType;
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
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{} <- {}",
            self.variable.format(st, sym_version),
            self.value.format(st, sym_version)
        )
    }
}

impl FormatWithParent for Transition {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.variable.format_with_parent(st);
        self.value.format_with_parent(st)
    }
}

impl GetVariables for Transition {
    fn get_variables(&self) -> HashSet<VarId> {
        self.variable
            .get_variables()
            .union(self.value.get_variables())
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
