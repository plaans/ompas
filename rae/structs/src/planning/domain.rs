use crate::acting_domain::command::Command;
use crate::acting_domain::method::Method;
use crate::acting_domain::state_function::StateFunction;
use crate::acting_domain::task::Task;
use crate::conversion::chronicle::template::ChronicleTemplate;
use crate::sym_table::domain::TypeId;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::FormatWithSymTable;
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let st = &self.st;

        write!(f, "# DOMAIN:\n\n")?;

        //actions
        write!(f, "# METHOD TEMPLATES: \n\n")?;
        for (id, method) in self.methods.iter().enumerate() {
            write!(
                f,
                "#{}\n\
                {}\n\n\n",
                id,
                method.template.format(false)
            )?;
        }

        //tasks
        write!(f, "# TASKS: \n\n")?;
        for task in &self.tasks {
            write!(f, "{}\n\n\n", task.convert.format(st, true))?;
        }

        write!(f, "# COMMAND TEMPLATES: \n\n")?;
        for (id, command) in self.methods.iter().enumerate() {
            write!(
                f,
                "#{}\n\
                    {}\n\n\n",
                id,
                command.template.format(false)
            )?;
        }
        Ok(())

        //f.push_str(self.sym_table.to_string().as_str());
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
