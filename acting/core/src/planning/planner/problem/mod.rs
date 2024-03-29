use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::acting_domain::task::Task;
use crate::model::process_ref::ProcessRef;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::state::world_state::WorldStateSnapshot;
use aries_planning::chronicles::{Chronicle, ChronicleOrigin};
use aries_planning::parsing::pddl::Method;
use sompas_structs::lvalues::LValueS;
use std::fmt::{Display, Formatter};

pub struct PlanningProblem {
    pub domain: PlanningDomain,
    pub instance: PlanningInstance,
    pub st: RefSymTable,
}

pub struct PlanningInstance {
    pub state: WorldStateSnapshot,
    pub tasks: Vec<LValueS>,
    pub instances: Vec<ChronicleInstance>,
}

pub struct ChronicleInstance {
    pub generated: bool,
    pub origin: ChronicleOrigin,
    pub am: ActingModel,
    pub pr: ProcessRef,
}

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
