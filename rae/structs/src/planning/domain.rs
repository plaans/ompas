use crate::acting_domain::command::Command;
use crate::acting_domain::method::Method;
use crate::acting_domain::state_function::StateFunction;
use crate::acting_domain::task::Task;
use crate::conversion::chronicle::template::ChronicleTemplate;
use crate::sym_table::domain::TypeId;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::VarId;
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct PlanningDomain {
    pub sf: Vec<StateFunction>,
    pub tasks: Vec<TaskChronicle>,
    pub methods: Vec<MethodChronicle>,
    pub commands: Vec<CommandChronicle>,
    pub new_types: Vec<TypeId>,
    pub st: RefSymTable,
}

impl Display for PlanningDomain {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
#[derive(Clone)]

pub struct TaskChronicle {
    pub task: Task,
    pub convert: Vec<VarId>,
}
#[derive(Clone)]

pub struct MethodChronicle {
    pub method: Method,
    pub template: ChronicleTemplate,
}
#[derive(Clone)]

pub struct CommandChronicle {
    pub command: Command,
    pub template: ChronicleTemplate,
}
