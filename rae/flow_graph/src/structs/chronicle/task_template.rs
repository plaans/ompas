use crate::structs::chronicle::template::ChronicleTemplate;
use crate::structs::chronicle::VarId;

pub struct TaskTemplate {
    pub name: Vec<VarId>,
    pub methods: Vec<ChronicleTemplate>,
}
