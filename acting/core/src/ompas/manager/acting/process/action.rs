use crate::model::process_ref::MethodId;
use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use std::fmt::{Display, Formatter};

#[derive(Default, Clone, Debug)]
pub struct Refinement {
    pub(crate) possibilities: Vec<ActingProcessId>,
    pub(crate) chosen: ActingProcessId,
}

impl Refinement {
    pub fn get_method(&self, method_id: &usize) -> &ActingProcessId {
        &self.possibilities[*method_id]
    }

    pub fn get_chosen(&self) -> &ActingProcessId {
        &self.chosen
    }
}

#[derive(Clone, Debug)]
pub struct ActionProcess {
    pub abstract_model: Option<ActingProcessId>,
    pub args: Vec<ActingVarRef<Cst>>,
    pub refinements: Vec<Refinement>,
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

    pub fn get_refinement(&self, refinement_id: usize) -> Option<&Refinement> {
        self.refinements.get(refinement_id)
    }

    pub fn new_refinement(&mut self, refinement_id: Option<usize>) -> usize {
        let id = self.refinements.len();
        self.refinements.push(Refinement::default());
        if let Some(refinement_id) = refinement_id {
            assert_eq!(id, refinement_id);
        } else {
        }
        id
    }

    pub fn add_method_to_refinement(
        &mut self,
        refinement_id: usize,
        method_id: Option<usize>,
        method: ActingProcessId,
    ) -> MethodId {
        let possibilities = &mut self.refinements[refinement_id].possibilities;
        let id = possibilities.len();
        if let Some(method_id) = method_id {
            assert_eq!(id, method_id);
        }
        possibilities.push(method);
        MethodId {
            refinement_id,
            method_number: id,
        }
    }

    pub fn set_chosen(&mut self, method_id: &MethodId) {
        let refinement = &mut self.refinements[method_id.refinement_id];
        refinement.chosen = refinement.possibilities[method_id.method_number];
    }

    /*pub fn update_last_refinement(&mut self, refinement: ActingProcessId) {
        *self.refinements.last_mut().unwrap() = refinement;
    }*/

    pub fn get_refinements(&self) -> &Vec<Refinement> {
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
