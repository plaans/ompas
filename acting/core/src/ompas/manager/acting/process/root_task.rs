use crate::ompas::manager::acting::process::ActingProcessInner;
use crate::ompas::manager::acting::ActingProcessId;
use std::fmt::{Display, Formatter};

pub struct RootProcess {
    pub tasks: Vec<ActingProcessId>,
    pub commands: Vec<ActingProcessId>,
}

impl Default for RootProcess {
    fn default() -> Self {
        Self::new()
    }
}

impl RootProcess {
    pub fn new() -> Self {
        Self {
            tasks: vec![],
            commands: vec![],
        }
    }

    pub fn n_task(&self) -> usize {
        self.tasks.len()
    }

    pub fn n_command(&self) -> usize {
        self.commands.len()
    }

    pub fn nth_task(&self, rank: usize) -> Option<ActingProcessId> {
        self.tasks.get(rank).copied()
    }

    pub fn nth_command(&self, rank: usize) -> Option<ActingProcessId> {
        self.commands.get(rank).copied()
    }

    pub fn add_top_level_task(&mut self, id: ActingProcessId) -> usize {
        self.tasks.push(id);
        self.tasks.len() - 1
    }

    pub fn add_top_level_command(&mut self, id: ActingProcessId) -> usize {
        self.commands.push(id);
        self.commands.len() - 1
    }
}

impl From<RootProcess> for ActingProcessInner {
    fn from(value: RootProcess) -> Self {
        Self::RootTask(value)
    }
}

impl Display for RootProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(0) Root")
    }
}
