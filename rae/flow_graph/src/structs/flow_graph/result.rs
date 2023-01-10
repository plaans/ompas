use crate::structs::chronicle::GetVariables;
use crate::structs::domain::Domain;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::VarId;
use im::{hashset, HashSet};

#[derive(Clone)]
pub struct FlowResult {
    pub result: VarId,
}

impl GetVariables for FlowResult {
    fn get_variables(&self) -> HashSet<VarId> {
        hashset![self.result]
    }

    fn get_variables_in_domain(&self, _: &RefSymTable, _: &Domain) -> HashSet<VarId> {
        todo!()
    }
}
