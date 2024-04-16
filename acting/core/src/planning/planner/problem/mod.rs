use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::model::{ActingModel, Event, Goal, NewTask};
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::acting_domain::task::Task;
use crate::model::chronicle::Chronicle;
use crate::model::process_ref::{Label, MethodLabel, ProcessRef, RefinementLabel};
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use aries_planning::chronicles::ChronicleOrigin;
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

#[derive(Clone)]
pub struct ChronicleInstance {
    pub instantiated_chronicle: Chronicle,
    pub generated: bool,
    pub origin: ChronicleOrigin,
    pub am: ActingModel,
    pub pr: ProcessRef,
    pub refinement_label: RefinementLabel,
}

pub async fn new_problem_chronicle_instance(
    st: &RefSymTable,
    mut tasks: Vec<NewTask>,
    goals: Vec<Goal>,
    events: Vec<Event>,
) -> ChronicleInstance {
    let mut am = ActingModel::root(st);
    for (i, task) in tasks.drain(..).enumerate() {
        am.add_new_task(task, Label::Task(i));
    }
    for goal in goals {
        am.add_goal(goal);
    }
    for event in events {
        am.add_event(event);
    }

    let instantiated_chronicle = am.get_instantiated_chronicle().unwrap();

    ChronicleInstance {
        instantiated_chronicle,
        generated: false,
        origin: ChronicleOrigin::Original,
        am,
        pr: ProcessRef::Id(0),
        refinement_label: RefinementLabel {
            refinement_id: 0,
            method_label: MethodLabel::Possibility(0),
        },
    }
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
