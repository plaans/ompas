use ompas_lisp::structs::{LValueS, LValue};
use std::fmt::{Display, Formatter};
use crate::rae::lisp::JobStream;
use std::alloc::LayoutError;

#[derive(Debug, Clone)]
pub struct Job {
    _type: JobType,
    core: LValueS,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} : {}", self._type, self.core)
    }
}

impl Job {
    pub fn new(value: LValue, _type: JobType)-> Self {
        Self {
            _type,
            core: value.into()
        }
    }
}

#[derive(Debug, Clone)]
pub enum JobType {
    Task,
    Event,
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
