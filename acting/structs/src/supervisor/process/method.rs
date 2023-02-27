use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::process_ref::Label;
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct MethodProcess {
    pub id: ActingProcessId,
    pub parent: ActingProcessId,
    pub status: ActionStatus,
    pub process_set: HashMap<Label, ActingProcessId>,
    pub debug: String,
    pub interval: Option<Interval>,
    pub value: LValue,
    pub expanded: LValue,
}

impl MethodProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        debug: String,
        value: LValue,
        start: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            status: ActionStatus::Pending,
            process_set: Default::default(),
            debug,
            interval: start.map(|s| Interval::new(s, None::<Timepoint>)),
            value,
            expanded: Default::default(),
        }
    }

    pub fn add_process(&mut self, label: Label, id: ActingProcessId) {
        self.process_set.insert(label, id);
    }

    pub fn get_process(&mut self, label: Label) -> Option<ActingProcessId> {
        self.process_set.get(&label).copied()
    }

    pub fn set_expanded(&mut self, expanded: LValue) {
        self.expanded = expanded
    }

    pub fn get_expanded(&self) -> &LValue {
        &self.expanded
    }

    pub fn set_start(&mut self, timepoint: Timepoint) {
        match &mut self.interval {
            Some(interval) => interval.start = timepoint,
            None => self.interval = Some(Interval::new_instant(timepoint)),
        }
    }

    pub fn set_status(&mut self, status: ActionStatus) {
        self.status = status
    }

    pub fn set_end(&mut self, timepoint: Timepoint) {
        if let Some(interval) = &mut self.interval {
            interval.set_end(timepoint)
        }
    }
}

impl From<MethodProcess> for ActingProcessInner {
    fn from(value: MethodProcess) -> Self {
        Self::Method(value)
    }
}

impl Display for MethodProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let interval = match &self.interval {
            None => "[..]".to_string(),
            Some(interval) => interval.to_string(),
        };

        write!(f, "({}) {} {}", self.id, interval, self.debug)
    }
}
