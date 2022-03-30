use crate::context::actions_progress::Status;
use im::HashMap;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::SpecialError;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_utils::other::get_and_update_id_counter;
use std::fmt::{Debug, Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct Agenda {
    inner: Arc<RwLock<im::HashMap<usize, TaskRefinement>>>,
    next_id: Arc<AtomicUsize>,
}

impl Agenda {
    pub async fn display(&self, all: bool) -> String {
        let mut string = format!(
            "Agenda:\n\t-number of task: {}\n\t-Actual agenda:\n",
            self.next_id.load(Ordering::Relaxed)
        );
        let mut inner: im::HashMap<usize, TaskRefinement> = self.get_inner().await;
        if !all {
            inner = inner
                .iter()
                .filter_map(|(&id, t)| {
                    if t.status != Status::Done {
                        Some((id, t.clone()))
                    } else {
                        None
                    }
                })
                .collect()
        }
        if inner.is_empty() {
            string.push_str("\t\tIs empty...");
        } else {
            let mut k_v: Vec<(usize, TaskRefinement)> =
                inner.iter().map(|k| (*k.0, k.1.clone())).collect();

            k_v.sort_by(|k1, k2| k1.0.cmp(&k2.0));
            for (id, rs) in k_v {
                string.push_str(format!("\t\t{}: {}\n", id, rs).as_str())
            }
        }
        string
    }
}

impl Agenda {
    pub async fn add_task(&self, task: LValue, parent_task: Option<usize>) -> TaskRefinement {
        let task_id = self.get_next_id();
        let stack = TaskRefinement::new(task, task_id, parent_task);
        self.inner.write().await.insert(task_id, stack.clone());
        stack
    }

    pub async fn remove_task(&self, task_id: &usize) -> Result<(), LError> {
        match self.inner.write().await.remove(task_id) {
            None => Err(SpecialError(
                "agenda.remove_task",
                format!("could not remove task {}", task_id),
            )),
            Some(_) => Ok(()),
        }
    }

    pub async fn get_refinement(&self, task_id: usize) -> Result<TaskRefinement, LError> {
        match self.inner.read().await.get(&task_id) {
            None => Err(SpecialError(
                "agenda.get_stack",
                format!("Task {} has no stack or does not exist.", task_id),
            )),
            Some(rs) => Ok(rs.clone()),
        }
    }

    pub async fn update_refinement(&self, rs: TaskRefinement) -> Result<(), LError> {
        //println!("in update stack\n stack: {}", rs);
        let mut locked = self.inner.write().await;
        match locked.contains_key(&rs.task_id) {
            true => {
                locked.insert(rs.task_id, rs);
                Ok(())
            }
            false => Err(SpecialError(
                "agenda.update_stack",
                format!(
                    "Cannot update stack of task {} as task does not exist",
                    rs.task_id
                ),
            )),
        }
    }

    pub async fn get_next_applicable_method(&self, task_id: &usize) -> LValue {
        let mut locked = self.inner.write().await;
        let mut stack = locked.remove(task_id).unwrap();
        let current_method = stack.current_method;
        stack.tried.push(current_method);
        let next_method = stack.applicable_methods.pop().unwrap_or(LValue::Nil);
        stack.current_method = next_method.clone();
        locked.insert(*task_id, stack);
        next_method
    }

    pub async fn get_inner(&self) -> HashMap<usize, TaskRefinement> {
        self.inner.read().await.clone()
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}

#[derive(Clone, Debug)]
pub struct TaskRefinement {
    task: LValue,
    current_method: LValue,
    applicable_methods: Vec<LValue>,
    tried: Vec<LValue>,
    task_id: usize,
    parent_task: Option<usize>,
    status: Status,
}

impl TaskRefinement {
    pub fn new(task: LValue, task_id: usize, parent_task: Option<usize>) -> Self {
        Self {
            task,
            current_method: LValue::Nil,
            applicable_methods: vec![],
            tried: vec![],
            task_id,
            parent_task,
            status: Status::Pending,
        }
    }
}

/*
GETTERS
 */
impl TaskRefinement {
    pub fn get_current_method(&self) -> &LValue {
        &self.current_method
    }

    pub fn get_tried(&self) -> &Vec<LValue> {
        &self.tried
    }

    pub fn get_task(&self) -> &LValue {
        &self.task
    }

    pub fn get_task_id(&self) -> usize {
        self.task_id
    }

    pub fn get_status(&self) -> Status {
        self.status
    }

    pub fn get_parent_task(&self) -> Option<usize> {
        self.parent_task
    }
}

/*
SETTERS
 */
impl TaskRefinement {
    pub fn set_current_method(&mut self, current_method: LValue) {
        self.current_method = current_method;
    }

    pub fn set_applicable_methods(&mut self, applicable_methods: Vec<LValue>) {
        self.applicable_methods = applicable_methods;
    }

    pub fn add_tried_method(&mut self, tried_method: LValue) {
        self.tried.push(tried_method);
    }

    pub fn set_status(&mut self, status: Status) {
        self.status = status;
    }
}

impl Display for TaskRefinement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        str.push_str(
            format!(
                "task: {}\n\
                parent_task: {}\n\
                Status: {}\n\
                current_method: {}\n\
                tried: {}\n",
                self.task,
                match self.parent_task {
                    Some(p) => p.to_string(),
                    None => "none".to_string(),
                },
                self.status,
                self.current_method,
                LValue::from(self.tried.clone()),
            )
            .as_str(),
        );
        write!(f, "{}", str)
    }
}
