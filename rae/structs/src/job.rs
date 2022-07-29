use sompas_structs::lasynchandler::LAsyncHandler;
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
    pub sender: mpsc::Sender<LAsyncHandler>,
    pub _type: JobType,
    pub core: LValue,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} : {}", self._type, self.core)
    }
}

impl Job {
    pub fn new(sender: mpsc::Sender<LAsyncHandler>, value: LValue, _type: JobType) -> Self {
        Self {
            sender,
            _type,
            core: value,
        }
    }
}

impl Display for JobType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            JobType::Task => write!(f, "task"),
            JobType::Event => write!(f, "event"),
        }
    }
}
pub type JobId = usize;