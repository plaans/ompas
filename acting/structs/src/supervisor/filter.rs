use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::inner::ProcessKind;
use ompas_language::supervisor::*;
use std::fmt::{Display, Formatter};

impl Display for ProcessKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ProcessKind::Task => TASK,
                ProcessKind::Command => COMMAND,
                ProcessKind::Method => METHOD,
                ProcessKind::Acquire => ACQUIRE,
                ProcessKind::Arbitrary => ARBITRARY,
                ProcessKind::RootTask => ROOT_TASK,
            }
        )
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub struct ProcessFilter {
    pub task_type: Option<ProcessKind>,
    pub status: Option<ActionStatus>,
}
