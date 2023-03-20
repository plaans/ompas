use crate::model::process_ref::Label;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct RefinementProcess {
    pub childs: HashMap<Label, ActingProcessId>,
}

impl RefinementProcess {
    pub fn new() -> Self {
        Self {
            childs: Default::default(),
        }
    }

    pub fn add_process(&mut self, label: Label, id: ActingProcessId) {
        self.childs.insert(label, id);
    }

    pub fn get_process(&mut self, label: Label) -> Option<ActingProcessId> {
        self.childs.get(&label).copied()
    }
}

impl From<RefinementProcess> for ActingProcessInner {
    fn from(value: RefinementProcess) -> Self {
        Self::Method(value)
    }
}

impl Display for RefinementProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
