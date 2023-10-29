use crate::model::process_ref::ProcessRef;
use crate::ompas::interface::job::JobType;
use ompas_utils::other::get_and_update_id_counter;
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::mem;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

pub type JobId = usize;

pub struct PendingJob {
    pub id: JobId,
    pub r#type: JobType,
    pub lvalue: LValue,
}

#[derive(Default)]
pub struct JobCollection {
    pendings: Arc<RwLock<Vec<PendingJob>>>,
    inner: Arc<RwLock<HashMap<JobId, JobHandle>>>,
    next_id: Arc<AtomicUsize>,
}

impl JobCollection {
    pub async fn clear(&self) {
        self.inner.write().await.clear();
        let n = self.next_id.load(Ordering::Acquire);
        let _ = self
            .next_id
            .compare_exchange(n, 0, Ordering::Acquire, Ordering::Relaxed);
    }

    pub async fn add_pending_job(&self, r#type: JobType, lvalue: LValue) -> usize {
        let id = self.get_next_id();

        let mut pendings = self.pendings.write().await;
        let rank = pendings.len();
        pendings.push(PendingJob { id, r#type, lvalue });
        self.inner
            .write()
            .await
            .insert(id, JobHandle::Pending(rank));
        id
    }

    pub async fn set_task_process(&self, id: &JobId, task_process: TaskProcess) -> JobId {
        *self.inner.write().await.get_mut(id).unwrap() = JobHandle::Process(task_process);
        *id
    }

    pub async fn add_process(&self, task_process: TaskProcess) -> usize {
        let id = self.get_next_id();
        self.inner
            .write()
            .await
            .insert(id, JobHandle::Process(task_process));
        id
    }

    pub async fn get_job(&self, id: JobId) -> Option<JobHandle> {
        self.inner.read().await.get(&id).cloned()
    }

    fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }

    pub async fn move_pendings(&self) -> Vec<PendingJob> {
        mem::take(&mut *self.pendings.write().await)
    }
}

#[derive(Debug)]
pub enum Response {
    Process(TaskProcess),
    Handle(LAsyncHandle),
}

#[derive(Clone)]
pub enum JobHandle {
    Pending(usize),
    Process(TaskProcess),
}

#[derive(Clone, Debug)]
pub struct TaskProcess {
    pr: ProcessRef,
    handle: LAsyncHandle,
}

impl TaskProcess {
    pub fn new(pr: ProcessRef, handle: LAsyncHandle) -> Self {
        Self { pr, handle }
    }

    pub fn get_ref(&self) -> ProcessRef {
        self.pr.clone()
    }

    pub fn get_handle(&self) -> LAsyncHandle {
        self.handle.clone()
    }
}
