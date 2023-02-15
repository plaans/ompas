use crate::state::action_status::ActionStatus;
use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use tokio::sync::watch;

#[derive(Debug)]
pub struct CommandProcess {
    id: ActingProcessId,
    parent: ActingProcessId,
    value: LValue,
    status: ActionStatus,
    interval: Option<Interval>,
    sender_to_watcher: Option<watch::Sender<ActionStatus>>,
}

impl CommandProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        value: LValue,
        start: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            value,
            status: ActionStatus::Pending,
            interval: start.map(|s| Interval::new(s, None)),
            sender_to_watcher: None,
        }
    }

    pub fn set_watch(&mut self, watch: watch::Sender<ActionStatus>) {
        self.sender_to_watcher = Some(watch)
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
}

impl From<CommandProcess> for ActingProcessInner {
    fn from(value: CommandProcess) -> Self {
        Self::Command(value)
    }
}
