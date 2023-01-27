use crate::conversion::chronicle::template::ChronicleTemplate;
use crate::sym_table::VarId;

#[derive(Clone)]
pub struct TaskTemplate {
    pub name: Vec<VarId>,
    pub methods: Vec<ChronicleTemplate>,
}
