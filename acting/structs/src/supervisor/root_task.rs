use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;

pub struct RootProcess {
    id: ActingProcessId,
    tasks: Vec<ActingProcessId>,
}

impl RootProcess {
    pub fn new() -> Self {
        Self {
            id: 0,
            tasks: vec![],
        }
    }

    pub fn add_top_level_task(&mut self, id: ActingProcessId) {
        self.tasks.push(id)
    }
}

impl From<RootProcess> for ActingProcessInner {
    fn from(value: RootProcess) -> Self {
        Self::RootTask(value)
    }
}
