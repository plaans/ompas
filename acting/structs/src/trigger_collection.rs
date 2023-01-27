use crate::ActionId;
use ompas_utils::other::get_and_update_id_counter;
use sompas_structs::lasynchandler::LAsyncHandle;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::RwLock;

pub type TriggerId = usize;

#[derive(Default)]
pub struct TriggerCollection {
    inner: Arc<RwLock<HashMap<TriggerId, TaskTrigger>>>,
    next_id: Arc<AtomicUsize>,
}

impl TriggerCollection {
    pub async fn add_task(&self, task_trigger: TaskTrigger) -> usize {
        let id = self.get_next_id();
        self.inner.write().await.insert(id, task_trigger);
        id
    }

    pub async fn get_task(&self, id: ActionId) -> Option<TaskTrigger> {
        self.inner.read().await.get(&id).cloned()
    }

    fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}

#[derive(Debug)]
pub enum Response {
    Trigger(TaskTrigger),
    Handle(LAsyncHandle),
}

#[derive(Clone, Debug)]
pub struct TaskTrigger {
    task_id: Arc<RwLock<Option<ActionId>>>,
    handle: LAsyncHandle,
}

impl TaskTrigger {
    pub fn new(task_id: Arc<RwLock<Option<ActionId>>>, handle: LAsyncHandle) -> Self {
        Self { task_id, handle }
    }

    pub async fn get_task_id(&self) -> Option<ActionId> {
        *self.task_id.read().await
    }

    pub fn get_handle(&self) -> LAsyncHandle {
        self.handle.clone()
    }
}
