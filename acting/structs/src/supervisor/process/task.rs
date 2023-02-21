use crate::interface::select_mode::SelectMode;
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
    pub current_refinement: Option<usize>,
    pub interval: Option<Interval>,
}

impl TaskProcess {
    pub fn get_planned_refinement(&self) -> Option<&Refinement> {
        match self.current_refinement {
            Some(n) => {
                if n == self.refinements.len() - 1 {
                    None
                } else {
                    self.refinements.last()
                }
            }
            None => self.refinements.last(),
        }
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
            current_refinement: None,
            interval: start.map(|start| Interval::new(start, None::<Timepoint>)),
        }
    }

    pub fn add_refinement(&mut self, refinement: Refinement) {
        self.refinements.push(refinement)
    }

    pub fn update_last_refinement(&mut self, refinement: Refinement) {
        *self.refinements.last_mut().unwrap() = refinement;
    }

    pub fn get_id_current_method(&self) -> Option<ActingProcessId> {
        self.current_refinement
            .map(|n| self.refinements[n].method_id)
    }

    pub fn set_start(&mut self, start: Timepoint) {
        self.interval = Some(Interval::new(start, None::<Timepoint>))
    }

    pub fn set_end(&mut self, end: Timepoint) {
        if let Some(interval) = &mut self.interval {
            interval.end = Some(end)
        }
    }

    pub fn set_status(&mut self, status: ActionStatus) {
        self.status = status
    }

    pub fn get_tried_methods(&self) -> Vec<LValue> {
        self.refinements
            .iter()
            .map(|r| r.inner.method_value.clone())
            .collect()
    }
}

impl From<TaskProcess> for ActingProcessInner {
    fn from(value: TaskProcess) -> Self {
        Self::Task(value)
    }
}

impl Display for TaskProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let interval = match &self.interval {
            None => "[..]".to_string(),
            Some(interval) => interval.to_string(),
        };
        write!(
            f,
            "({}) {}{}({})",
            self.id, interval, self.value, self.status
        )
    }
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
