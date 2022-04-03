pub mod task_collection;
pub mod task_network;

use crate::context::refinement::task_collection::{
    AbstractTaskMetaData, ActionMetaData, TaskCollection, TaskFilter, TaskMetaData, TaskStatus,
};
use crate::context::refinement::task_network::TaskNetwork;
use im::OrdMap;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::SpecialError;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_utils::other::get_and_update_id_counter;
use std::fmt::{Display, Formatter};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::mpsc;
use tokio::time::Instant;

pub type TaskId = usize;

pub type Timepoint = u128;
pub type Duration = u128;
#[derive(Debug, Default, Clone)]
pub struct Interval {
    start: Timepoint,
    end: Option<Timepoint>,
}

const FACTOR_TO_SEC: f64 = 1_000_000.0;
const FACTOR_TO_MILLIS: f64 = 1_000.0;

impl Display for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("[{:.3},", self.start as f64 / FACTOR_TO_SEC);
        match &self.end {
            Some(end) => str.push_str(format!("{:^3}]", *end as f64 / FACTOR_TO_SEC).as_str()),
            None => str.push_str("...]"),
        }

        write!(f, "{}", str)
    }
}

impl Interval {
    pub fn new(start: Timepoint, end: Option<Timepoint>) -> Self {
        Self { start, end }
    }

    /// Returns end - start if end is defined.
    pub fn duration(&self) -> Duration {
        match &self.end {
            Some(e) => e - self.start,
            None => u128::MAX,
        }
    }
}

#[derive(Clone)]
pub struct Agenda {
    pub trc: TaskCollection,
    tn: TaskNetwork,
    next_id: Arc<AtomicUsize>,
    time_reference: Instant,
}

impl Default for Agenda {
    fn default() -> Self {
        Self {
            trc: Default::default(),
            tn: Default::default(),
            next_id: Arc::new(Default::default()),
            time_reference: Instant::now(),
        }
    }
}

impl Agenda {
    pub fn reset_time_reference(&mut self) {
        self.time_reference = Instant::now();
    }

    pub fn get_instant(&self) -> Timepoint {
        self.time_reference.elapsed().as_micros()
    }
}

impl Agenda {
    pub async fn format_task_collection(&self, filter: TaskFilter) -> String {
        self.trc.format(filter).await
    }

    pub async fn format_task_network(&self) -> String {
        self.tn.format().await
    }
}

const GET_ABSTRACT_TASK: &str = "get_abstract_task";

impl Agenda {
    pub async fn add_abstract_task(
        &self,
        task: LValue,
        parent_task: Option<usize>,
    ) -> AbstractTaskMetaData {
        let task_id = self.get_next_id();
        let start = self.time_reference.elapsed().as_micros();
        let stack = AbstractTaskMetaData::new(task, task_id, parent_task, start);
        self.trc.insert(task_id, stack.clone()).await;
        if let Some(parent_task) = parent_task {
            self.tn.add_task_to_parent(parent_task, task_id).await;
        } else {
            self.tn.add_new_root_task(task_id).await;
        }
        stack
    }

    pub async fn add_action(
        &self,
        action: LValue,
        parent_task: usize,
    ) -> (TaskId, mpsc::Receiver<TaskStatus>) {
        let task_id = self.get_next_id();
        let start = self.time_reference.elapsed().as_micros();
        let (action, rx) = ActionMetaData::new(task_id, parent_task, action, start);
        self.trc.insert(task_id, action).await;
        (task_id, rx)
    }

    pub async fn get_abstract_task(&self, task_id: usize) -> Result<AbstractTaskMetaData, LError> {
        match self.trc.get(task_id).await {
            TaskMetaData::AbstractTask(a) => Ok(a),
            TaskMetaData::Action(_) => Err(SpecialError(
                GET_ABSTRACT_TASK,
                format!("{} does not exist", task_id),
            )),
        }
    }

    pub async fn update_task(&self, id: TaskId, task: impl Into<TaskMetaData>) {
        //println!("in update stack\n stack: {}", rs);
        self.trc.update(id, task).await
    }

    pub async fn update_status(&self, id: TaskId, status: TaskStatus) {
        self.trc.update_status(id, status).await
    }

    pub async fn get_status(&self, id: TaskId) -> TaskStatus {
        self.trc.get_status(id).await
    }

    pub async fn get_task_collection(&self) -> OrdMap<TaskId, TaskMetaData> {
        self.trc.get_inner().await
    }

    pub async fn set_end_time(&self, id: TaskId) {
        let end = self.time_reference.elapsed().as_micros();
        let mut task = self.trc.get(id).await;
        task.set_end_timepoint(end);
        self.trc.update(id, task).await;
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}
