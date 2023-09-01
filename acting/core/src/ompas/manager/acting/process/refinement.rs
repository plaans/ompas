use crate::model::process_ref::{Label, RefinementLabel};
use crate::model::sym_domain::cst::Cst;
use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct RefinementProcess {
    pub label: RefinementLabel,
    pub args: Vec<ActingVarRef<Cst>>,
    pub childs: HashMap<Label, ActingProcessId>,
}

impl RefinementProcess {
    pub fn new(label: RefinementLabel, args: Vec<ActingVarRef<Cst>>) -> Self {
        Self {
            label,
            args,
            childs: Default::default(),
        }
    }

    pub fn add_process(&mut self, label: Label, id: ActingProcessId) {
        self.childs.insert(label, id);
    }

    pub fn get_process(&mut self, label: Label) -> Option<ActingProcessId> {
        self.childs.get(&label).copied()
    }

    pub fn get_actions(&self) -> Vec<ActingProcessId> {
        self.childs
            .iter()
            .filter_map(|(k, v)| {
                if let Label::Action(_) = k {
                    Some(*v)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_arbitraries(&self) -> Vec<ActingProcessId> {
        self.childs
            .iter()
            .filter_map(|(k, v)| {
                if let Label::Arbitrary(_) = k {
                    Some(*v)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_acquires(&self) -> Vec<ActingProcessId> {
        self.childs
            .iter()
            .filter_map(|(k, v)| {
                if let Label::ResourceAcquisition(_) = k {
                    Some(*v)
                } else {
                    None
                }
            })
            .collect()
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
