use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::process_ref::MethodLabel;
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;

pub struct MethodProcess {
    pub id: ActingProcessId,
    pub parent: ActingProcessId,
    pub process_set: HashMap<MethodLabel, ActingProcessId>,
    pub debug: Option<String>,
    pub interval: Option<Interval>,
    pub value: LValue,
}

impl MethodProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        value: LValue,
        start: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            process_set: Default::default(),
            debug: None,
            interval: start.map(|s| Interval::new(s, None)),
            value,
        }
    }

    pub fn add_process(&mut self, label: MethodLabel, id: ActingProcessId) {
        self.process_set.insert(label, id);
    }

    pub fn get_process(&mut self, label: MethodLabel) -> Option<ActingProcessId> {
        self.process_set.get(&label).copied()
    }

    pub fn set_debug(&mut self, debug: String) {
        self.debug = Some(debug)
    }
}

impl From<MethodProcess> for ActingProcessInner {
    fn from(value: MethodProcess) -> Self {
        Self::Method(value)
    }
}
