use crate::rae::Lisp;
use crate::rae::method::Method;

pub type TaskId = usize;

pub struct Task {
    id: TaskId,
    methods: Vec<Method>,
}
