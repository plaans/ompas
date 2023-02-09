use crate::acting_domain::command::Command;
use crate::acting_domain::method::Method;
use crate::acting_domain::state_function::StateFunction;
use crate::acting_domain::task::Task;
use crate::conversion::chronicle::template::ChronicleTemplate;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::VarId;
use std::fmt::{Display, Formatter};
#[derive(Clone)]
pub struct PlanningDomain {
    pub sf: Vec<StateFunction>,
    pub methods: Vec<String>,
    pub tasks: Vec<String>,
    pub commands: Vec<String>,
    pub templates: Vec<ChronicleTemplate>,
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
    pub template: Option<ChronicleTemplate>,
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
