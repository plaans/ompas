use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};
use tokio::sync::mpsc;

#[derive(Debug, Clone)]
pub enum JobExpr {
    Task(String),
    Method(String),
    Command(String),
    Debug(String),
}

impl Display for JobExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            JobExpr::Task(t) | JobExpr::Method(t) | JobExpr::Command(t) | JobExpr::Debug(t) => {
                write!(f, "{}", t)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Job {
    pub sender: mpsc::Sender<Result<LAsyncHandle, LRuntimeError>>,
    pub r#type: JobExpr,
}

impl Display for Job {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.r#type)
    }
}

impl Job {
    pub fn new_task(
        sender: mpsc::Sender<Result<LAsyncHandle, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            r#type: JobExpr::Task(value.to_string()),
        }
    }

    pub fn new_method(
        sender: mpsc::Sender<Result<LAsyncHandle, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            r#type: JobExpr::Method(value.to_string()),
        }
    }

    pub fn new_command(
        sender: mpsc::Sender<Result<LAsyncHandle, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            r#type: JobExpr::Command(value.to_string()),
        }
    }

    pub fn new_debug(
        sender: mpsc::Sender<Result<LAsyncHandle, LRuntimeError>>,
        value: LValue,
    ) -> Self {
        Self {
            sender,
            r#type: JobExpr::Debug(value.to_string()),
        }
    }
}

pub type JobId = usize;
