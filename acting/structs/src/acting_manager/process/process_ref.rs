use crate::acting_manager::ActingProcessId;
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

    pub fn pop(&mut self) -> Option<Label> {
        match self {
            ProcessRef::Id(_) => None,
            ProcessRef::Relative(id, vec) => match vec.len() {
                1 => {
                    let r = vec.pop();
                    *self = ProcessRef::Id(*id);
                    r
                }
                _ => vec.pop(),
            },
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
    Refinement(Option<usize>),
    Action(usize),
    Acquire(usize),
    Arbitrary(usize),
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Refinement(m) => {
                write!(
                    f,
                    "refinement{}",
                    match m {
                        None => "".to_string(),
                        Some(m) => format!("({m})"),
                    }
                )
            }
            Label::Action(id) => {
                write!(f, "action({id})")
            }
            Label::Acquire(acq) => {
                write!(f, "acquire({acq})")
            }
            Label::Arbitrary(arb) => {
                write!(f, "arbitrary({arb})")
            }
        }
    }
}
