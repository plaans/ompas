use crate::rae::structs::{ActionStatus, RAETask, RAEAssignment, RAECommand};
use crate::rae::action::Action;
use crate::rae::task::Task;

#[derive(Default, Debug, Clone)]
pub enum MethodStep {
    Action(Action),
    Assignment(Assignment),
    Task(Task)
}

pub enum ActionStatus {
    Running,
    RetrialFailure,
    Failure,
    Done
}

#[derive(Default, Debug, Clone)]
pub struct Assignment {

}

#[derive(Default, Debug, Clone)]
pub struct Method {
    inner: MethodType,
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
