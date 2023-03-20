use crate::model::sym_domain::cst;
use crate::ompas::interface::select_mode::SelectMode;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::acting::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct TaskProcess {
    pub args: Vec<cst::Cst>,
    pub refinements: Vec<ActingProcessId>,
}

impl TaskProcess {
    pub fn new(args: Vec<cst::Cst>) -> Self {
        Self {
            args,
            refinements: vec![],
        }
    }

    pub fn get_args(&self) -> &Vec<cst::Cst> {
        &self.args
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

impl Display for TaskProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Clone, Debug)]
pub struct Refinement {
    pub method_id: ActingProcessId,
    pub inner: RefinementInner,
}

#[derive(Clone, Debug)]
pub struct RefinementInner {
    pub task_value: LValue,
    pub method_value: LValue,
    pub tried: Vec<LValue>,
    pub possibilities: Vec<LValue>,
    pub interval: Interval,
    pub select: SelectTrace,
}

#[derive(Copy, Clone, Debug)]
pub enum SelectTrace {
    ContinuousPlanning,
    RealTime(RTSelect),
}

#[derive(Copy, Clone, Debug)]
pub struct RTSelect {
    pub refinement_type: SelectMode,
}

impl Display for RTSelect {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "refinement mode: {}", self.refinement_type)
    }
}
