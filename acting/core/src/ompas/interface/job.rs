use crate::ompas::interface::trigger_collection::Response;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};
use tokio::sync::mpsc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum JobType {
    Task,
    Command,
    Event,
    Debug,
    Init,
}

#[derive(Debug, Clone)]
pub struct Job {
    pub sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
    pub expr: String,
    pub r#type: JobType,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.expr)
    }
}

impl Job {
    pub fn new(
        sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
        r#type: JobType,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type,
        }
    }

    pub fn new_task(
        sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Task,
        }
    }

    pub fn new_debug(
        sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Debug,
        }
    }

    pub fn new_command(
        sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Command,
        }
    }
    pub fn new_event(
        sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Event,
        }
    }

    pub fn new_init(
        sender: mpsc::UnboundedSender<Result<Response, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            expr: value.to_string(),
            r#type: JobType::Init,
        }
    }
}

pub type JobId = usize;
