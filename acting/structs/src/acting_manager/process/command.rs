use crate::acting_manager::process::ActingProcessInner;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct CommandProcess {}

impl CommandProcess {
    pub fn new() -> Self {
        Self {}
    }
}

impl From<CommandProcess> for ActingProcessInner {
    fn from(value: CommandProcess) -> Self {
        Self::Command(value)
    }
}

impl Display for CommandProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
