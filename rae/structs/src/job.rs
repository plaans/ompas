use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};
use tokio::sync::mpsc;

#[derive(Debug, Clone)]
pub enum JobType {
    Task,
    Event,
}

#[derive(Debug, Clone)]
pub struct Job {
    pub sender: mpsc::Sender<LAsyncHandle>,
    pub core: LValue,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.core)
    }
}

impl Job {
    pub fn new(sender: mpsc::Sender<LAsyncHandle>, value: LValue) -> Self {
        Self {
            sender,
            core: value,
        }
    }
}

pub type JobId = usize;
