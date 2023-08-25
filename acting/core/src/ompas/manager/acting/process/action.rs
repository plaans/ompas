use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct ActionProcess {
    pub abstract_model: Option<ActingProcessId>,
    pub args: Vec<ActingVarRef<Cst>>,
    pub refinements: Vec<ActingProcessId>,
}

impl ActionProcess {
    pub fn new(args: Vec<ActingVarRef<Cst>>) -> Self {
        Self {
            abstract_model: None,
            args,
            refinements: vec![],
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

    pub fn add_refinement(&mut self, refinement: ActingProcessId) {
        self.refinements.push(refinement)
    }

    pub fn update_last_refinement(&mut self, refinement: ActingProcessId) {
        *self.refinements.last_mut().unwrap() = refinement;
    }

    pub fn get_refinements(&self) -> &Vec<ActingProcessId> {
        &self.refinements
    }
}

impl From<ActionProcess> for ActingProcessInner {
    fn from(value: ActionProcess) -> Self {
        Self::Action(value)
    }
}

impl Display for ActionProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
