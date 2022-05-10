use crate::interval::{Duration, Timepoint};
use crate::options::SelectMode;
use crate::state::task_network::TaskNetwork;
use crate::state::task_state::{
    AbstractTaskMetaData, ActionMetaData, TaskCollection, TaskFilter, TaskMetaData,
    TaskMetaDataView,
};
use crate::state::task_status::TaskStatus;
use crate::TaskId;
use chrono::{DateTime, Utc};
use core::convert::Into;
use core::default::Default;
use core::option::Option;
use core::option::Option::{None, Some};
use core::result::Result;
use core::result::Result::{Err, Ok};
use core::sync::atomic::AtomicUsize;
use sompas_structs::lerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::{lerror, string};
use sompas_utils::other::get_and_update_id_counter;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;
use std::{env, fs};
use tokio::sync::mpsc;

const TASK_NAME: &str = "name of the task";
const TASK_STATUS: &str = "status";
const TASK_EXECUTION_TIME: &str = "execution time (s)";
const REFINEMENT_METHOD: &str = "method of refinement ";
const REFINEMENT_NUMBER: &str = "number of refinement";
const TOTAL_REFINEMENT_TIME: &str = "total refinement time (s)";
const SUBTASK_NUMBER: &str = "number of subtask";
const ACTION_NUMBER: &str = "number of actions";
const RAE_STATS: &str = "rae_stats";

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
    /*
    GETTERS
     */
    pub async fn get_refinement_method(&self, id: &TaskId) -> SelectMode {
        let task: AbstractTaskMetaData = self.trc.get(id).await.try_into().unwrap();
        let r = task.get_last_refinement();
        r.refinement_type
    }

    pub async fn get_execution_time(&self, id: &TaskId) -> Duration {
        let task: AbstractTaskMetaData = self.trc.get(id).await.try_into().unwrap();
        task.get_duration()
    }

    pub async fn get_number_of_subtasks(&self, id: &TaskId) -> usize {
        self.tn.get_subtasks(id).await.len()
    }

    pub async fn get_number_of_subtasks_recursive(&self, id: &TaskId) -> usize {
        let mut subtasks: Vec<TaskId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            n += 1;
            subtasks.append(&mut self.tn.get_subtasks(&c).await);
        }

        n
    }

    pub async fn get_number_of_abstract_tasks(&self, id: &TaskId) -> usize {
        let mut subtasks: Vec<TaskId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_abstract_task(id).await {
                n += 1;
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
            }
        }
        n
    }

    pub async fn get_number_of_actions(&self, id: &TaskId) -> usize {
        let mut subtasks: Vec<TaskId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_action(&c).await {
                n += 1;
            } else {
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
            }
        }
        n
    }

    pub async fn get_total_number_of_refinement(&self, id: &TaskId) -> usize {
        let task: AbstractTaskMetaData = self.trc.get(id).await.try_into().unwrap();
        let mut n = task.get_number_of_refinement();
        let mut subtasks: Vec<TaskId> = self.tn.get_subtasks(id).await;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_abstract_task(&c).await {
                let task: AbstractTaskMetaData = self.trc.get(&c).await.try_into().unwrap();
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
                n += task.get_number_of_refinement();
            }
        }
        n
    }

    pub async fn get_total_refinement_time(&self, id: &TaskId) -> Duration {
        let mut total_time: Duration = Duration::Finite(0);
        let task: AbstractTaskMetaData = self.trc.get(id).await.try_into().unwrap();
        total_time += task.get_total_refinement_time();
        let mut subtasks: Vec<TaskId> = self.tn.get_subtasks(id).await;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_abstract_task(&c).await {
                let task: AbstractTaskMetaData = self.trc.get(&c).await.try_into().unwrap();
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
                total_time += task.get_total_refinement_time();
            }
        }
        total_time
    }

    pub async fn get_stats(&self) -> LValue {
        let mut map: im::HashMap<LValue, LValue> = Default::default();
        let task_collection: im::HashMap<TaskId, TaskMetaData> =
            self.trc.inner.read().await.clone();
        let parent: Vec<TaskId> = self.tn.get_parents().await;
        for p in &parent {
            let mut task_stats: im::HashMap<LValue, LValue> = Default::default();
            task_stats.insert(
                string!(REFINEMENT_METHOD),
                self.get_refinement_method(p).await.to_string().into(),
            );
            task_stats.insert(
                string!(REFINEMENT_NUMBER),
                self.get_total_number_of_refinement(p).await.into(),
            );
            task_stats.insert(
                string!(SUBTASK_NUMBER),
                self.get_number_of_subtasks_recursive(p).await.into(),
            );
            task_stats.insert(
                string!(TASK_EXECUTION_TIME),
                self.get_execution_time(p).await.to_string().into(),
            );
            task_stats.insert(
                string!(TASK_STATUS),
                self.get_status(p).await.to_string().into(),
            );
            task_stats.insert(
                string!(ACTION_NUMBER),
                self.get_number_of_actions(p).await.into(),
            );
            task_stats.insert(
                string!(TOTAL_REFINEMENT_TIME),
                self.get_total_refinement_time(p).await.to_string().into(),
            );
            map.insert(
                task_collection.get(p).unwrap().get_label(),
                task_stats.into(),
            );
        }

        map.into()
    }

    pub async fn export_to_csv(&self, working_dir: Option<PathBuf>, file: Option<String>) {
        let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
        let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();

        let dir_path: PathBuf = match working_dir {
            Some(wd) => {
                let mut dir_path = wd;
                dir_path.push(RAE_STATS);
                dir_path
            }
            None => format!(
                "{}/ompas/{}",
                match env::var("HOME") {
                    Ok(val) => val,
                    Err(_) => ".".to_string(),
                },
                RAE_STATS
            )
            .into(),
        };

        fs::create_dir_all(&dir_path).expect("could not create stats directory");
        let mut file_path = dir_path.clone();
        file_path.push(match file {
            Some(f) => format!("{}.csv", f),
            None => format!("{}{}.csv", RAE_STATS, string_date),
        });
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&file_path)
            .expect("error creating stat file");
        let header = format!(
            "\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\"\n",
            TASK_NAME,
            TASK_STATUS,
            TASK_EXECUTION_TIME,
            REFINEMENT_METHOD,
            REFINEMENT_NUMBER,
            TOTAL_REFINEMENT_TIME,
            SUBTASK_NUMBER,
            ACTION_NUMBER
        );
        if file.metadata().unwrap().len() == 0 {
            file.write_all(header.as_bytes())
                .expect("could not write to stat file");
        }
        let task_collection: im::HashMap<TaskId, TaskMetaData> =
            self.trc.inner.read().await.clone();
        let parent: Vec<TaskId> = self.tn.get_parents().await;
        for p in &parent {
            file.write_all(
                format!(
                    "\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\"\n",
                    task_collection.get(p).unwrap().get_label(),
                    self.get_status(p).await,
                    {
                        let u: Duration = self.get_execution_time(p).await;
                        if u.is_finite() {
                            u.as_secs().to_string()
                        } else {
                            u.to_string()
                        }
                    },
                    self.get_refinement_method(p).await,
                    self.get_total_number_of_refinement(p).await,
                    self.get_total_refinement_time(p).await.as_secs(),
                    self.get_number_of_subtasks_recursive(p).await,
                    self.get_number_of_actions(p).await,
                )
                .as_bytes(),
            )
            .expect("could not write to stat file")
        }
    }

    pub fn reset_time_reference(&mut self) {
        self.time_reference = Instant::now();
    }

    pub fn get_instant(&self) -> Timepoint {
        self.time_reference.elapsed().as_micros()
    }

    pub async fn format_task_collection(&self, filter: TaskFilter) -> String {
        self.trc.format(filter).await
    }

    pub async fn format_task_network(&self) -> String {
        self.tn.format().await
    }

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
        self.tn.add_task_to_parent(parent_task, task_id).await;
        (task_id, rx)
    }

    #[function_name::named]
    pub async fn get_abstract_task(
        &self,
        task_id: &TaskId,
    ) -> Result<AbstractTaskMetaData, LRuntimeError> {
        match self.trc.get(task_id).await {
            TaskMetaData::AbstractTask(a) => Ok(a),
            TaskMetaData::Action(_) => Err(lerror!(
                function_name!(),
                format!("{} does not exist", task_id)
            )),
        }
    }

    pub async fn update_task(&self, id: &TaskId, task: impl Into<TaskMetaData>) {
        //println!("in update stack\n stack: {}", rs);
        self.trc.update(id, task).await
    }

    pub async fn update_status(&self, id: &TaskId, status: TaskStatus) {
        self.trc.update_status(id, status).await
    }

    pub async fn get_status(&self, id: &TaskId) -> TaskStatus {
        self.trc.get_status(id).await
    }

    pub async fn get_task_collection(&self) -> im::HashMap<TaskId, TaskMetaData> {
        self.trc.get_inner().await
    }

    pub async fn set_end_time(&self, id: &TaskId) {
        let end = self.time_reference.elapsed().as_micros();
        let mut task: TaskMetaData = self.trc.get(id).await;
        task.set_end_timepoint(end);
        self.trc.update(id, task).await;
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}
