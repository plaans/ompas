use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct CommandProcess {
    pub abstract_model: Option<ActingProcessId>,
    pub args: Vec<ActingVarRef<Cst>>,
}

impl CommandProcess {
    pub fn new(args: Vec<ActingVarRef<Cst>>) -> Self {
        Self {
            abstract_model: None,
            args,
        }
    }

    pub fn get_args(&self) -> Vec<ActingVarRef<Cst>> {
        self.args.clone()
    }

    pub fn add_abstract_model(&mut self, id: &ActingProcessId) {
        self.abstract_model = Some(*id)
    }

    pub fn get_abstract_model(&self) -> Option<&ActingProcessId> {
        self.abstract_model.as_ref()
    }
}

impl From<CommandProcess> for ActingProcessInner {
    fn from(value: CommandProcess) -> Self {
        Self::Command(value)
    }
}

impl Display for CommandProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
