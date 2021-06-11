use crate::rae::action::Action;
use crate::rae::task::{Task, TaskId};
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub enum MethodStep {
    Action(Action),
    Assignment(Assignment),
    Task(Task)
}

#[derive(Clone, Debug)]
pub enum ActionStatus {
    NotTriggered,
    Running,
    Failure,
    Done
}

#[derive(Default, Debug, Clone)]
pub struct Assignment {

}

#[derive(Debug, Clone)]
pub struct Method {
    status: ActionStatus,
    core: Vec<MethodStep> //here this is some lisp
}

impl Method {
    pub fn get_step(&self, i: usize) -> Option<MethodStep> {
        self.core.get(i).cloned()
    }

    pub fn len(&self) -> usize {
        self.core.len()
    }
}


#[derive(Default, Debug, Clone)]
pub struct RefinementStack {
    inner: VecDeque<RefinementTuple>
}

impl RefinementStack {
    pub fn new(rt: RefinementTuple) -> Self {
        let mut inner = VecDeque::new();
        inner.push_back(rt);
        Self {
            inner,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct RefinementTuple {
    pub task_id : TaskId,
    pub method: Option<Method>,
    pub step : MethodStepId,
    pub tried: Vec<Method>
}

impl RefinementTuple {
    pub fn increment_step(&mut self) {
        self.step+=1;
    }

    pub fn is_last_step(&self) -> bool{
        self.step == match &self.method {
            None => 0,
            Some(m) => m.len()
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct RefinementMethodInstance {

}

pub type MethodStepId = usize;

impl RefinementStack {
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn is_retrial_failure(&self) -> bool  {
        false
    }

    pub fn push(&mut self, rt: RefinementTuple) {
        self.inner.push_front(rt);
    }

    pub fn top(&self) -> Option<&RefinementTuple> {
        self.inner.front()
    }

    pub fn pop(&mut self) -> Option<RefinementTuple> {
        self.inner.pop_front()
    }
}
