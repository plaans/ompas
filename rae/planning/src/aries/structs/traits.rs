use crate::aries::structs::symbol_table::{AtomId, SymTable};
use crate::aries::structs::type_table::PlanningAtomType;

pub trait Absorb {
    fn absorb(&mut self, other: Self);
}

pub trait FormatWithSymTable {
    fn format(&self, st: &SymTable, sym_version: bool) -> String;
}

pub trait GetVariables {
    fn get_variables(&self) -> im::HashSet<AtomId>;

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> im::HashSet<AtomId>;
}

/*
Transforms all literals by replacing all atomid by the atomid of their parents.
 */
pub trait FormatWithParent {
    fn format_with_parent(&mut self, st: &SymTable);
}

impl<T> FormatWithParent for Vec<T>
where
    T: FormatWithParent,
{
    fn format_with_parent(&mut self, st: &SymTable) {
        self.iter_mut().for_each(|e| e.format_with_parent(st))
    }
}
