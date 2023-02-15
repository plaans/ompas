use crate::state::action_status::ActionStatus;
use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;

#[derive(Clone, Debug)]
pub struct TaskProcess {
    pub id: ActingProcessId,
    pub parent: ActingProcessId,
    pub status: ActionStatus,
    pub debug: String,
    pub methods: Vec<ActingProcessId>,
    pub interval: Option<Interval>,
}

impl TaskProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        debug: String,
        start: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            status: ActionStatus::Pending,
            debug,
            methods: vec![],
            interval: start.map(|start| Interval::new(start, None)),
        }
    }

    pub fn add_method(&mut self, method: ActingProcessId) {
        self.methods.push(method)
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
        self.methods.last().copied()
    }
}

impl From<TaskProcess> for ActingProcessInner {
    fn from(value: TaskProcess) -> Self {
        Self::Task(value)
    }
}
