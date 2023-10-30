use crate::model::process_ref::{MethodLabel, RefinementLabel};
use crate::model::sym_domain::cst::Cst;
use crate::ompas::interface::select_mode::SelectMode;
use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Default, Clone, Debug)]
pub struct Refinement {
    pub(crate) methods: HashMap<MethodLabel, ActingProcessId>,
    pub(crate) refinement_trace: Option<RefinementTrace>,
}

impl Refinement {
    pub fn get_method(&self, label: &MethodLabel) -> Option<&ActingProcessId> {
        self.methods.get(label)
    }

    pub fn get_suggested(&self) -> Option<&ActingProcessId> {
        self.methods.get(&MethodLabel::Suggested)
    }

    pub fn get_executed(&self) -> Option<&ActingProcessId> {
        self.methods.get(&MethodLabel::Executed)
    }

    pub fn get_possibilities(&self) -> Vec<&ActingProcessId> {
        self.methods
            .iter()
            .filter_map(|(k, v)| {
                if let MethodLabel::Possibility(_) = k {
                    Some(v)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn set_suggested(&mut self, method: &ActingProcessId) {
        self.methods.insert(MethodLabel::Suggested, *method);
    }

    pub fn set_executed(&mut self, method: &ActingProcessId, refinement_trace: RefinementTrace) {
        self.methods.insert(MethodLabel::Executed, *method);
        if self.refinement_trace.is_none() {
            self.refinement_trace = Some(refinement_trace)
        } else {
            unreachable!()
        }
    }

    pub fn add_method(&mut self, label: MethodLabel, method: &ActingProcessId) {
        self.methods.insert(label, *method);
    }
}

#[derive(Clone, Debug)]
pub struct TaskProcess {
    pub abstract_model: Option<ActingProcessId>,
    pub args: Vec<ActingVarRef<Cst>>,
    pub refinements: Vec<Refinement>,
}

impl TaskProcess {
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

    pub fn new_refinement(&mut self) -> usize {
        let id = self.refinements.len();
        self.refinements.push(Default::default());
        id
    }

    pub fn add_method(&mut self, refinement_label: RefinementLabel, method: &ActingProcessId) {
        self.refinements[refinement_label.refinement_id]
            .add_method(refinement_label.method_label, method)
    }

    pub fn set_suggested(&mut self, refinement_id: usize, method: &ActingProcessId) {
        self.refinements
            .get_mut(refinement_id)
            .unwrap()
            .set_suggested(method)
    }

    pub fn set_executed(
        &mut self,
        refinement_id: usize,
        method: &ActingProcessId,
        refinement_trace: RefinementTrace,
    ) {
        self.refinements
            .get_mut(refinement_id)
            .unwrap()
            .set_executed(method, refinement_trace);
    }

    pub fn get_refinements(&self) -> &Vec<Refinement> {
        &self.refinements
    }
}

impl From<TaskProcess> for ActingProcessInner {
    fn from(value: TaskProcess) -> Self {
        Self::Task(value)
    }
}

impl Display for TaskProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Clone, Debug)]
pub struct RefinementTrace {
    pub selected: Selected,
    pub candidates: Vec<LValue>,
    pub interval: Interval,
}

#[derive(Clone, Debug)]
pub enum Selected {
    Anticipated(ActingProcessId),
    Generated(LValue, SelectMode),
}

impl Display for Selected {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Selected::Anticipated(a) => {
                write!(f, "Anticipated({a})")
            }
            Selected::Generated(m, algo) => {
                write!(f, "Generated(method = {}, algo = {})", m, algo)
            }
        }
    }
}
