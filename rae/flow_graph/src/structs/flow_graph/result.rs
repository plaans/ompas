use crate::structs::chronicle::GetVariables;
use crate::structs::domain::Domain;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
use im::{hashset, HashSet};

#[derive(Clone)]
pub struct FlowResult {
    pub result: AtomId,
}

impl GetVariables for FlowResult {
    fn get_variables(&self) -> HashSet<AtomId> {
        hashset![self.result]
    }

    fn get_variables_in_domain(&self, _: &RefSymTable, _: &Domain) -> HashSet<AtomId> {
        todo!()
    }
}
