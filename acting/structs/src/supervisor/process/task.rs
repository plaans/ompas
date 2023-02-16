use crate::interface::select_mode::SelectMode;
use crate::planning::plan::Plan;
use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct TaskProcess {
    pub id: ActingProcessId,
    pub parent: ActingProcessId,
    pub status: ActionStatus,
    pub value: LValue,
    pub refinements: Vec<Refinement>,
    pub current_method: Option<usize>,
    pub interval: Option<Interval>,
}

#[derive(Clone, Debug)]
pub struct Refinement {
    pub method: ActingProcessId,
    pub trace: RefinementTrace,
}

impl TaskProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        value: LValue,
        start: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            status: ActionStatus::Pending,
            value,
            refinements: vec![],
            current_method: None,
            interval: start.map(|start| Interval::new(start, None)),
        }
    }

    pub fn add_refinement(&mut self, refinement: Refinement) {
        self.refinements.push(refinement)
    }

    pub fn set_start(&mut self, start: Timepoint) {
        self.interval = Some(Interval::new(start, None))
    }

    pub fn set_end(&mut self, end: Timepoint) {
        if let Some(interval) = &mut self.interval {
            interval.end = Some(end)
        }
    }

    pub fn set_status(&mut self, status: ActionStatus) {
        self.status = status
    }

    pub fn get_executed_method(&self) -> Option<ActingProcessId> {
        self.refinements.last().map(|r| r.method)
    }

    pub fn get_tried_method(&self) -> Vec<ActingProcessId> {
        self.refinements.iter().map(|r| r.method).collect()
    }
}

impl From<TaskProcess> for ActingProcessInner {
    fn from(value: TaskProcess) -> Self {
        Self::Task(value)
    }
}

#[derive(Clone, Debug)]
pub struct RefinementTrace {
    pub refinement_type: SelectMode,
    pub applicable_methods: Vec<LValue>,
    pub choosed: LValue,
    pub plan: Option<Plan>,
    pub interval: Interval,
}

impl Display for RefinementTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "refinement mode: {}\
            choosed: {}\
        applicable: {}\
        time: {}\n",
            self.refinement_type,
            self.choosed,
            LValue::from(&self.applicable_methods),
            self.interval.duration()
        )
    }
}
