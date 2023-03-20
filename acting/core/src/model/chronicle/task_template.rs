use crate::model::chronicle::Chronicle;
use crate::model::sym_table::VarId;

#[derive(Clone)]
pub struct TaskTemplate {
    pub name: Vec<VarId>,
    pub methods: Vec<Chronicle>,
}
