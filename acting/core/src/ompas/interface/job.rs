use crate::ompas::interface::trigger_collection::Response;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};
use tokio::sync::mpsc;

#[derive(Debug, Clone)]
pub enum JobType {
    Task,
    Debug,
}

/*impl Display for JobType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {

                write!(f, "{}", )


    }
}*/

#[derive(Debug, Clone)]
pub struct Job {
    pub sender: mpsc::Sender<Result<Response, LRuntimeError>>,
    pub expr: String,
    pub r#type: JobType,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.expr)
    }
}

impl Job {
    pub fn new_task(sender: mpsc::Sender<Result<Response, LRuntimeError>>, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Task,
        }
    }

    pub fn new_debug(sender: mpsc::Sender<Result<Response, LRuntimeError>>, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Debug,
        }
    }
}

pub type JobId = usize;
