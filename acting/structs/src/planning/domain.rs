use crate::acting_domain::command::Command;
use crate::acting_domain::method::Method;
use crate::acting_domain::state_function::StateFunction;
use crate::acting_domain::task::Task;
use crate::acting_manager::operational_model::ActingModel;
use crate::conversion::chronicle::Chronicle;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::VarId;
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct PlanningDomain {
    /// State functions of the domain.
    pub sf: Vec<StateFunction>,
    /// List of method symbols
    pub methods: Vec<String>,
    ///List of task symbols
    pub tasks: Vec<String>,
    ///List of command symbols
    pub commands: Vec<String>,
    /// All available templates
    pub templates: Vec<ActingModel>,
    /// Symbol table
    pub st: RefSymTable,
}

impl Display for PlanningDomain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "# DOMAIN:\n\n")?;
        //tasks
        write!(f, "# TASKS: {:?}", self.tasks)?;

        //methods
        write!(f, "# METHODS: {:?}\n\n", self.methods)?;

        //commands
        write!(f, "# COMMANDS: {:?}", self.commands)?;

        write!(f, "# STATE FUNCTIONS: \n\n")?;
        for sf in &self.sf {
            write!(f, "{}", sf)?;
        }
        Ok(())

        //f.push_str(self.sym_table.to_string().as_str());
    }
}
#[derive(Clone)]

pub struct TaskChronicle {
    pub task: Task,
    pub convert: Vec<VarId>,
    pub template: Option<Chronicle>,
}
#[derive(Clone)]

pub struct MethodChronicle {
    pub method: Method,
    pub template: Chronicle,
}
#[derive(Clone)]

pub struct CommandChronicle {
    pub command: Command,
    pub template: Chronicle,
}
