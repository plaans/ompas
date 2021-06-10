use crate::rae::Lisp;
use crate::rae::method::Method;
use crate::rae::context::RefinementStack;

pub type TaskId = usize;

pub struct Task {
    pub id: TaskId,
    pub methods: Vec<Method>,
    refinement_stack: RefinementStack
}
