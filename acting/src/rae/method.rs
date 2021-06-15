use crate::rae::context::Status;
use crate::rae::job::{Job, JobId};
use crate::rae::Lisp;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Assignment {
    pub core: Lisp,
}

#[derive(Debug, Clone)]
pub struct Method {
    status: Status,
    core: Vec<String>, //here this is some lisp
}

impl Method {
    pub fn get_step(&self, i: usize) -> Option<String> {
        self.core.get(i).cloned()
    }

    pub fn len(&self) -> usize {
        self.core.len()
    }

    pub fn is_empty(&self) -> bool {
        self.core.is_empty()
    }
}

#[derive(Debug, Clone)]
pub struct RefinementStack {
    pub job: Job,
    pub inner: VecDeque<StackFrame>,
}

#[derive(Default, Debug, Clone)]
pub struct StackFrame {
    pub job_id: JobId,
    pub method: Option<Method>,
    pub step: MethodStepId,
    pub tried: Vec<Method>,
}

impl StackFrame {
    pub fn increment_step(&mut self) {
        self.step += 1;
    }

    pub fn is_last_step(&self) -> bool {
        self.step
            == match &self.method {
                None => 0,
                Some(m) => m.len(),
            }
    }
}

#[derive(Default, Debug, Clone)]
pub struct RefinementMethodInstance {}

pub type MethodStepId = usize;

impl RefinementStack {
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn is_retrial_failure(&self) -> bool {
        false
    }

    pub fn push(&mut self, rt: StackFrame) {
        self.inner.push_front(rt);
    }

    pub fn top(&self) -> Option<&StackFrame> {
        self.inner.front()
    }

    pub fn pop(&mut self) -> Option<StackFrame> {
        self.inner.pop_front()
    }
}
