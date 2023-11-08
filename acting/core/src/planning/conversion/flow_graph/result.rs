use crate::model::sym_table::r#trait::GetVariables;
use crate::model::sym_table::VarId;
use map_macro::hash_set;

#[derive(Clone)]
pub struct FlowResult {
    pub result: VarId,
}

impl GetVariables for FlowResult {
    fn get_variables(&self) -> std::collections::HashSet<VarId> {
        hash_set![self.result]
    }
}
