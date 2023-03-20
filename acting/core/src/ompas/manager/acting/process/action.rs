use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::acting::acting_var::ExecutionVar;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::{AMId, ActingProcessId};
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct ActionProcess {
    pub abstract_am_id: Option<AMId>,
    pub args: Vec<ExecutionVar<Cst>>,
    pub refinements: Vec<ActingProcessId>,
}

impl ActionProcess {
    pub fn new(args: Vec<ExecutionVar<Cst>>) -> Self {
        Self {
            abstract_am_id: None,
            args,
            refinements: vec![],
        }
    }

    pub fn get_args(&self) -> Vec<Cst> {
        let mut args = vec![];
        for arg in &self.args {
            if let Some(val) = &arg.val {
                args.push(val.clone())
            } else {
                panic!("ActionProcess::get_args: val is not a cst")
            }
        }
        args
    }

    pub fn add_abstract_am_id(&mut self, am_id: &AMId) {
        self.abstract_am_id = Some(*am_id)
    }

    pub fn get_abstract_am_id(&self) -> Option<&AMId> {
        self.abstract_am_id.as_ref()
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
