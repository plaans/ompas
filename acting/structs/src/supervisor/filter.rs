use crate::supervisor::action_status::ActionStatus;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub enum ProcessKind {
    Task,
    Command,
}

pub const TASK: &str = "task";
pub const COMMAND: &str = "command";

impl Display for ProcessKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ProcessKind::Task => TASK,
                ProcessKind::Command => COMMAND,
            }
        )
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub struct ProcessFilter {
    pub task_type: Option<ProcessKind>,
    pub status: Option<ActionStatus>,
}
