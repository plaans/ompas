use crate::rae::method::{Method, RefinementStack};

pub type TaskId = usize;

#[derive(Default, Debug, Clone)]
pub struct Task {
    pub id: TaskId,
    pub methods: Vec<Method>,
    pub refinement_stack: RefinementStack
}

impl Task {
    pub fn new() -> Self {
        Self {
            id: 0,
            methods: vec![],
            refinement_stack: Default::default()
        }
    }
}
