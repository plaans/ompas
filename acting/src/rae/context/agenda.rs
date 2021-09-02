use im::HashMap;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LValue};
use ompas_utils::blocking_async;
use ompas_utils::other::get_and_update_id_counter;
use std::fmt::{Debug, Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct Agenda {
    inner: Arc<RwLock<im::HashMap<usize, RefinementStack>>>,
    next_id: Arc<AtomicUsize>,
}

impl Debug for Agenda {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        str.push_str(
            format!(
                "Agenda:\n\t-number of task: {}\n\t-Actual agenda:\n",
                self.next_id.load(Ordering::Relaxed)
            )
            .as_str(),
        );
        let clone = self.clone();
        let inner = blocking_async!(clone.get_inner().await).unwrap();
        if inner.is_empty() {
            str.push_str("\t\tIs empty...");
        } else {
            for (id, rs) in inner {
                str.push_str(format!("\t\t{}: {:?}", id, rs).as_str())
            }
        }
        write!(f, "{}", str)
    }
}

impl Display for Agenda {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        str.push_str(
            format!(
                "Agenda:\n\t-number of task declared: {}\n\t-Actual agenda:\n",
                self.next_id.load(Ordering::Relaxed)
            )
            .as_str(),
        );
        let clone = self.clone();
        let inner = blocking_async!(clone.get_inner().await).unwrap();
        if inner.is_empty() {
            str.push_str("\t\tIs empty...");
        } else {
            for (id, rs) in inner {
                str.push_str(format!("\t\t{}: {}", id, rs).as_str())
            }
        }
        write!(f, "{}", str)
    }
}

impl Agenda {
    pub fn add_task(&self, task: LValue) -> usize {
        let task_id = self.get_next_id();
        let stack = RefinementStack::new(task, task_id);
        let inner = self.inner.clone();
        blocking_async!({
            inner.write().await.insert(task_id, stack);
        })
        .unwrap();
        task_id
    }

    pub fn remove_task(&self, task_id: &usize) -> Result<(), LError> {
        let inner = self.inner.clone();
        let task_id = *task_id;
        blocking_async!({
            match inner.write().await.remove(&task_id) {
                None => Err(SpecialError(
                    "agenda.remove_task",
                    format!("could not remove task {}", task_id),
                )),
                Some(_) => Ok(()),
            }
        })
        .unwrap()
    }

    pub fn get_stack(&self, task_id: usize) -> Result<RefinementStack, LError> {
        let inner = self.inner.clone();
        match blocking_async!(inner.read().await.get(&task_id).cloned()).unwrap() {
            None => Err(SpecialError(
                "agenda.get_stack",
                format!("Task {} has no stack or does not exist.", task_id),
            )),
            Some(rs) => Ok(rs),
        }
    }

    pub fn update_stack(&self, rs: RefinementStack) -> Result<(), LError> {
        let inner = self.inner.clone();
        blocking_async!({
            let mut locked = inner.write().await;
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
        })
        .unwrap()
    }

    pub fn get_next_applicable_method(&self, task_id: &usize) -> LValue {
        let inner = self.inner.clone();
        let task_id = *task_id;
        blocking_async!({
            let mut locked = inner.write().await;
            let mut stack = locked.remove(&task_id).unwrap();
            let current_method = stack.current_method;
            stack.tried.push(current_method);
            let next_method = stack.applicable_methods.pop().unwrap_or(LValue::Nil);
            stack.current_method = next_method.clone();
            locked.insert(task_id, stack);
            next_method
        })
        .unwrap()
    }

    pub async fn get_inner(&self) -> HashMap<usize, RefinementStack> {
        self.inner.read().await.clone()
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}

#[derive(Clone, Debug)]
pub struct RefinementStack {
    task: LValue,
    current_method: LValue,
    applicable_methods: Vec<LValue>,
    tried: Vec<LValue>,
    task_id: usize,
}

impl RefinementStack {
    pub fn new(task: LValue, task_id: usize) -> Self {
        Self {
            task,
            current_method: LValue::Nil,
            applicable_methods: vec![],
            tried: vec![],
            task_id,
        }
    }

    pub fn set_current_method(&mut self, current_method: LValue) {
        self.current_method = current_method;
    }

    pub fn set_applicable_methods(&mut self, applicable_methods: Vec<LValue>) {
        self.applicable_methods = applicable_methods;
    }

    pub fn add_tried_task(&mut self, tried_task: LValue) {
        self.tried.push(tried_task);
    }
}

impl Display for RefinementStack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        str.push_str(
            format!(
                "task: {}\n\
        current_method: {}\n",
                self.task, self.current_method
            )
            .as_str(),
        );
        write!(f, "{}", str)
    }
}
