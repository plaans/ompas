use crate::ompas::interface::trigger_collection::Response;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum JobType {
    Task,
    Command,
    Event,
    Debug,
    Init,
}

pub type SenderJob = tokio::sync::mpsc::UnboundedSender<Result<Response, LRuntimeError>>;
pub type ReceiverJobResult = tokio::sync::mpsc::UnboundedReceiver<Result<Response, LRuntimeError>>;

#[derive(Debug, Clone)]
pub struct Job {
    pub sender: SenderJob,
    pub expr: String,
    pub r#type: JobType,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.expr)
    }
}

impl Job {
    pub fn new(sender: SenderJob, r#type: JobType, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type,
        }
    }

    pub fn new_task(sender: SenderJob, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Task,
        }
    }

    pub fn new_debug(sender: SenderJob, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Debug,
        }
    }

    pub fn new_command(sender: SenderJob, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Command,
        }
    }
    pub fn new_event(sender: SenderJob, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Event,
        }
    }

    pub fn new_init(sender: SenderJob, value: LValue) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Init,
        }
    }

    pub fn is_event(&self) -> bool {
        self.r#type == JobType::Event
    }

    pub fn is_task(&self) -> bool {
        self.r#type == JobType::Task
    }
}

pub type JobId = usize;
