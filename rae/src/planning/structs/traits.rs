use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::type_table::PlanningAtomType;

pub trait Absorb {
    fn absorb(&mut self, other: Self);
}

pub trait FormatWithSymTable {
    fn format_with_sym_table(&self, st: &SymTable) -> String;
}

pub trait GetVariables {
    fn get_variables(&self) -> im::HashSet<AtomId>;

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> im::HashSet<AtomId>;
}
