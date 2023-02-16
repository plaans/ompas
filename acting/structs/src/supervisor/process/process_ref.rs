use crate::supervisor::ActingProcessId;

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum ProcessRef {
    Id(ActingProcessId),
    Relative(ActingProcessId, Vec<Label>),
}

impl ProcessRef {
    pub fn as_id(&self) -> Option<ActingProcessId> {
        if let Self::Id(id) = self {
            Some(*id)
        } else {
            None
        }
    }

    pub fn is_relative(&self) -> bool {
        matches!(self, Self::Relative(..))
    }
}

impl Default for ProcessRef {
    fn default() -> Self {
        Self::Id(0)
    }
}

impl From<ActingProcessId> for ProcessRef {
    fn from(value: ActingProcessId) -> Self {
        Self::Id(value)
    }
}

#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub enum Label {
    Method(usize),
    MethodProcess(MethodLabel),
}

impl Label {
    pub fn as_method_label(&self) -> Option<&MethodLabel> {
        if let Self::MethodProcess(mp) = self {
            Some(mp)
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub enum MethodLabel {
    Subtask(usize),
    Arbitrary(usize),
    Command(usize),
    Acquire(usize),
}

impl From<MethodLabel> for Label {
    fn from(value: MethodLabel) -> Self {
        Label::MethodProcess(value)
    }
}
