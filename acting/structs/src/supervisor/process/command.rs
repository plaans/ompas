use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};
use tokio::sync::watch;

#[derive(Debug)]
pub struct CommandProcess {
    id: ActingProcessId,
    _parent: ActingProcessId,
    value: LValue,
    status: ActionStatus,
    interval: Option<Interval>,
    sender_to_watcher: Option<watch::Sender<ActionStatus>>,
}

impl CommandProcess {
    pub fn new(
        id: ActingProcessId,
        _parent: ActingProcessId,
        value: LValue,
        start: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            _parent,
            value,
            status: ActionStatus::Pending,
            interval: start.map(|s| Interval::new(s, None::<Timepoint>)),
            sender_to_watcher: None,
        }
    }

    pub fn set_watch(&mut self, watch: watch::Sender<ActionStatus>) {
        self.sender_to_watcher = Some(watch)
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
        self.status = status;
        if let Some(tx) = &self.sender_to_watcher {
            if tx.send(self.status).is_err() {
                self.sender_to_watcher = None;
            }
        }
    }
}

impl From<CommandProcess> for ActingProcessInner {
    fn from(value: CommandProcess) -> Self {
        Self::Command(value)
    }
}

impl Display for CommandProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let interval = match &self.interval {
            None => "[..]".to_string(),
            Some(interval) => interval.to_string(),
        };
        write!(
            f,
            "({}) {} {}({})",
            self.id, interval, self.value, self.status
        )
    }
}
