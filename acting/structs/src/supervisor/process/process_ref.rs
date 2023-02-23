use crate::supervisor::ActingProcessId;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum ProcessRef {
    Id(ActingProcessId),
    Relative(ActingProcessId, Vec<Label>),
}

impl ProcessRef {
    pub fn push(&mut self, label: Label) {
        match self {
            ProcessRef::Id(id) => *self = ProcessRef::Relative(*id, vec![label]),
            ProcessRef::Relative(_, vec) => vec.push(label),
        }
    }

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

impl Display for ProcessRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcessRef::Id(id) => write!(f, "{id}"),
            ProcessRef::Relative(id, labels) => {
                write!(f, "{id}")?;
                for label in labels {
                    write!(f, "/{label}")?;
                }
                Ok(())
            }
        }
    }
}

impl From<ActingProcessId> for ProcessRef {
    fn from(value: ActingProcessId) -> Self {
        Self::Id(value)
    }
}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub enum Label {
    Method(usize),
    HighLevelTask(usize),
    MethodProcess(MethodLabel),
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Method(m) => {
                write!(f, "m({m})")
            }
            Label::HighLevelTask(id) => {
                write!(f, "s({id})")
            }
            Label::MethodProcess(m) => {
                write!(f, "{m}")
            }
        }
    }
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

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub enum MethodLabel {
    Subtask(usize),
    Arbitrary(usize),
    Command(usize),
    Acquire(usize),
}

impl Display for MethodLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MethodLabel::Subtask(s) => {
                write!(f, "s({s})")
            }
            MethodLabel::Arbitrary(a) => {
                write!(f, "arb({a})")
            }
            MethodLabel::Command(c) => {
                write!(f, "c({c})")
            }
            MethodLabel::Acquire(a) => {
                write!(f, "acq({a})")
            }
        }
    }
}

impl From<MethodLabel> for Label {
    fn from(value: MethodLabel) -> Self {
        Label::MethodProcess(value)
    }
}
