use im::HashMap;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LValue};
use ompas_utils::other::get_and_update_id_counter;
use std::cell::Ref;
use std::fmt::{Debug, Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct Agenda {
    inner: Arc<RwLock<im::HashMap<usize, RefinementStack>>>,
    next_id: Arc<AtomicUsize>,
}

impl Agenda {
    pub async fn display(&self) -> String {
        let mut string = format!(
            "Agenda:\n\t-number of task: {}\n\t-Actual agenda:\n",
            self.next_id.load(Ordering::Relaxed)
        );
        let inner: im::HashMap<usize, RefinementStack> = self.get_inner().await;
        if inner.is_empty() {
            string.push_str("\t\tIs empty...");
        } else {
            let mut k_v: Vec<(usize, RefinementStack)> =
                inner.iter().map(|k| (k.0.clone(), k.1.clone())).collect();

            k_v.sort_by(|k1, k2| k1.0.cmp(&k2.0));
            for (id, rs) in k_v {
                string.push_str(format!("\t\t{}: {}\n", id, rs).as_str())
            }
        }
        string
    }
}

impl Agenda {
    pub async fn add_task(&self, task: LValue) -> usize {
        let task_id = self.get_next_id();
        let stack = RefinementStack::new(task, task_id);
        self.inner.write().await.insert(task_id, stack);
        task_id
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

    pub async fn get_stack(&self, task_id: usize) -> Result<RefinementStack, LError> {
        match self.inner.read().await.get(&task_id) {
            None => Err(SpecialError(
                "agenda.get_stack",
                format!("Task {} has no stack or does not exist.", task_id),
            )),
            Some(rs) => Ok(rs.clone()),
        }
    }

    pub async fn update_stack(&self, rs: RefinementStack) -> Result<(), LError> {
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
