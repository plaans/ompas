use crate::interval::{Duration, Interval, Timepoint};
use crate::options::SelectMode;
use crate::plan::Plan;
use crate::state::task_status::TaskStatus;
use crate::TaskId;
use itertools::Itertools;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use tokio::sync::{mpsc, RwLock};

#[derive(Clone, Default)]
pub struct TaskCollection {
    pub inner: Arc<RwLock<im::HashMap<TaskId, TaskMetaData>>>,
}

impl TaskCollection {
    /*
    FORMAT
     */
    pub async fn format(&self, filter: TaskFilter) -> String {
        let inner: im::HashMap<usize, TaskMetaData> = self.get_inner().await;

        let inner: Vec<TaskMetaData> = inner
            .values()
            .filter(|&t| {
                if let Some(task_type) = &filter.task_type {
                    match task_type {
                        TaskType::AbstractTask => t.is_abstract_task(),
                        TaskType::Action => t.is_action(),
                    }
                } else {
                    true
                }
            })
            .filter(|t| {
                if let Some(status) = &filter.status {
                    status == &t.get_status()
                } else {
                    true
                }
            })
            .sorted_by_key(|&t| t.get_id())
            .cloned()
            .collect();

        if inner.is_empty() {
            "Empty Agenda...".to_string()
        } else {
            let mut string = format!(
                "Agenda:\n\t-number of task: {}\n\t-Actual agenda:\n",
                inner.len()
            );
            for task in inner {
                string.push_str(format!("{}\n", task).as_str())
            }
            string
        }
    }

    /*
    BOOLEAN
     */
    pub async fn is_action(&self, id: &TaskId) -> bool {
        self.inner.read().await.get(id).unwrap().is_action()
    }

    pub async fn is_abstract_task(&self, id: &TaskId) -> bool {
        self.inner.read().await.get(id).unwrap().is_abstract_task()
    }

    /*
    GETTERS
     */
    pub async fn get_inner(&self) -> im::HashMap<TaskId, TaskMetaData> {
        self.inner.read().await.clone()
    }

    pub async fn get_status(&self, id: &TaskId) -> TaskStatus {
        self.inner.read().await.get(id).unwrap().get_status()
    }

    pub async fn get(&self, id: &TaskId) -> TaskMetaData {
        self.inner.read().await.get(id).unwrap().clone()
    }

    /*
    SETTERS
     */
    pub async fn insert(&self, id: TaskId, task: impl Into<TaskMetaData>) {
        self.inner.write().await.insert(id, task.into());
    }

    pub async fn update(&self, id: &TaskId, task: impl Into<TaskMetaData>) {
        let mut locked = self.inner.write().await;

        if locked.contains_key(id) {
            locked.insert(*id, task.into());
        }
    }

    pub async fn update_status(&self, id: &TaskId, status: TaskStatus) {
        self.inner
            .write()
            .await
            .get_mut(id)
            .unwrap()
            .update_status(status)
            .await
    }
}

#[derive(Clone, Debug)]
pub enum TaskMetaData {
    AbstractTask(AbstractTaskMetaData),
    Action(ActionMetaData),
}

impl TaskMetaData {
    pub fn is_abstract_task(&self) -> bool {
        match self {
            TaskMetaData::AbstractTask(_) => true,
            TaskMetaData::Action(_) => false,
        }
    }

    pub fn is_action(&self) -> bool {
        match self {
            TaskMetaData::AbstractTask(_) => false,
            TaskMetaData::Action(_) => true,
        }
    }

    pub fn get_status(&self) -> TaskStatus {
        match self {
            TaskMetaData::AbstractTask(a) => a.status,
            TaskMetaData::Action(a) => a.status,
        }
    }

    pub async fn update_status(&mut self, status: TaskStatus) {
        match self {
            TaskMetaData::AbstractTask(a) => a.update_status(status),
            TaskMetaData::Action(a) => a.update_status(status).await,
        }
    }

    pub fn get_id(&self) -> TaskId {
        match self {
            TaskMetaData::AbstractTask(a) => a.id,
            TaskMetaData::Action(a) => a.id,
        }
    }

    pub fn get_label(&self) -> LValue {
        match self {
            TaskMetaData::AbstractTask(a) => a.label.clone(),
            TaskMetaData::Action(a) => a.label.clone(),
        }
    }

    pub fn set_end_timepoint(&mut self, end: Timepoint) {
        match self {
            TaskMetaData::AbstractTask(a) => a.set_end_timepoint(end),
            TaskMetaData::Action(a) => a.set_end_timepoint(end),
        }
    }

    pub async fn set_as_done(&mut self, end: Timepoint) {
        self.update_status(TaskStatus::Done).await;
        self.set_end_timepoint(end);
    }

    pub async fn set_as_failure(&mut self, end: Timepoint) {
        self.update_status(TaskStatus::Failure).await;
        self.set_end_timepoint(end);
    }
}
impl Display for TaskMetaData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TaskMetaData::AbstractTask(a) => a.to_string(),
                TaskMetaData::Action(a) => a.to_string(),
            }
        )
    }
}

impl From<AbstractTaskMetaData> for TaskMetaData {
    fn from(a: AbstractTaskMetaData) -> Self {
        Self::AbstractTask(a)
    }
}

impl From<ActionMetaData> for TaskMetaData {
    fn from(a: ActionMetaData) -> Self {
        Self::Action(a)
    }
}

impl TryFrom<TaskMetaData> for ActionMetaData {
    type Error = LRuntimeError;

    fn try_from(value: TaskMetaData) -> Result<Self, Self::Error> {
        if let TaskMetaData::Action(a) = value {
            Ok(a)
        } else {
            Err(Default::default())
        }
    }
}
impl TryFrom<TaskMetaData> for AbstractTaskMetaData {
    type Error = LRuntimeError;

    fn try_from(value: TaskMetaData) -> Result<Self, Self::Error> {
        if let TaskMetaData::AbstractTask(a) = value {
            Ok(a)
        } else {
            Err(Default::default())
        }
    }
}

pub trait TaskMetaDataView {
    fn get_label(&self) -> &LValue;

    fn get_id(&self) -> &TaskId;

    fn get_status(&self) -> &TaskStatus;

    fn get_parent_task(&self) -> Option<usize>;

    fn set_end_timepoint(&mut self, end: Timepoint);

    fn get_start(&self) -> &Timepoint;

    fn get_end(&self) -> &Option<Timepoint>;

    fn get_duration(&self) -> Duration;
}

#[derive(Clone, Debug)]
pub struct ActionMetaData {
    id: TaskId,
    parent: usize,
    label: LValue,
    status: TaskStatus,
    interval: Interval,
    sender_to_watcher: Option<mpsc::Sender<TaskStatus>>,
}

impl ActionMetaData {
    const STATUS_CHANNEL_SIZE: usize = 10;

    pub fn new(
        id: TaskId,
        parent: usize,
        label: LValue,
        start: Timepoint,
    ) -> (Self, mpsc::Receiver<TaskStatus>) {
        let (tx, rx) = mpsc::channel(Self::STATUS_CHANNEL_SIZE);

        (
            Self {
                id,
                parent,
                label,
                status: TaskStatus::Pending,
                interval: Interval { start, end: None },
                sender_to_watcher: Some(tx),
            },
            rx,
        )
    }

    pub async fn update_status(&mut self, status: TaskStatus) {
        self.status = status;
        if let Some(tx) = &self.sender_to_watcher {
            if tx.try_send(self.status).is_err() {
                self.sender_to_watcher = None;
            }
        }
    }
}

impl TaskMetaDataView for ActionMetaData {
    fn get_label(&self) -> &LValue {
        &self.label
    }

    fn get_id(&self) -> &TaskId {
        &self.id
    }

    fn get_status(&self) -> &TaskStatus {
        &self.status
    }

    fn get_parent_task(&self) -> Option<usize> {
        Some(self.parent)
    }

    fn set_end_timepoint(&mut self, end: Timepoint) {
        self.interval.end = Some(end)
    }

    fn get_start(&self) -> &Timepoint {
        &self.interval.start
    }

    fn get_end(&self) -> &Option<Timepoint> {
        &self.interval.end
    }

    fn get_duration(&self) -> Duration {
        self.interval.duration()
    }
}

impl Display for ActionMetaData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "({:^3}){} {}\
            \nparent: {}",
            self.id, self.interval, self.label, self.parent
        )
    }
}

#[derive(Clone, Debug)]
pub struct AbstractTaskMetaData {
    id: TaskId,
    parent_task: Option<usize>,
    status: TaskStatus,
    label: LValue,
    current_method: LValue,
    refinement: Vec<RefinementMetaData>,
    interval: Interval,
    tried: Vec<LValue>,
}

impl AbstractTaskMetaData {
    pub fn new(label: LValue, id: usize, parent_task: Option<usize>, start: Timepoint) -> Self {
        Self {
            id,
            parent_task,
            status: TaskStatus::Pending,
            label,
            current_method: LValue::Nil,
            refinement: vec![],
            interval: Interval { start, end: None },
            tried: vec![],
        }
    }
}

/*
GETTERS
 */

impl TaskMetaDataView for AbstractTaskMetaData {
    fn get_label(&self) -> &LValue {
        &self.label
    }

    fn get_id(&self) -> &TaskId {
        &self.id
    }

    fn get_status(&self) -> &TaskStatus {
        &self.status
    }

    fn get_parent_task(&self) -> Option<usize> {
        self.parent_task
    }

    fn set_end_timepoint(&mut self, end: Timepoint) {
        self.interval.end = Some(end)
    }

    fn get_start(&self) -> &Timepoint {
        &self.interval.start
    }

    fn get_end(&self) -> &Option<Timepoint> {
        &self.interval.end
    }

    fn get_duration(&self) -> Duration {
        self.interval.duration()
    }
}

impl AbstractTaskMetaData {
    /*
    GETTERS
     */
    pub fn get_number_of_refinement(&self) -> usize {
        self.refinement.len()
    }

    pub fn get_total_refinement_time(&self) -> Duration {
        let mut total_time: Duration = Duration::Finite(0);
        for r in &self.refinement {
            total_time += r.interval.duration()
        }
        total_time
    }

    pub fn get_last_refinement(&self) -> &RefinementMetaData {
        self.refinement.last().unwrap()
    }

    pub fn get_current_method(&self) -> &LValue {
        &self.current_method
    }

    pub fn get_tried(&self) -> &Vec<LValue> {
        &self.tried
    }

    /*
    SETTERS
     */
    pub fn set_current_method(&mut self, current_method: LValue) {
        self.current_method = current_method;
    }

    pub fn add_refinement(&mut self, rmd: RefinementMetaData) {
        self.refinement.push(rmd);
    }

    pub fn add_tried_method(&mut self, tried_method: LValue) {
        self.tried.push(tried_method);
    }

    pub fn update_status(&mut self, status: TaskStatus) {
        self.status = status;
    }
}

impl Display for AbstractTaskMetaData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = String::new();
        str.push_str(
            format!(
                "({:^3}){} {}\n\
                * parent_task: {}\n\
                * Status: {}\n\
                * current_method: {}\n\
                * tried: {}\n",
                self.id,
                self.interval,
                self.label,
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

#[derive(Clone, Debug)]
pub struct RefinementMetaData {
    pub refinement_type: SelectMode,
    pub applicable_methods: Vec<LValue>,
    pub choosed: LValue,
    pub plan: Option<Plan>,
    pub interval: Interval,
}

impl Display for RefinementMetaData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "refinement mode: {}\
            choosed: {}\
        applicable: {}\
        time: {}\n",
            self.refinement_type,
            self.choosed,
            LValue::from(&self.applicable_methods),
            self.interval.duration()
        )
    }
}

#[derive(Debug, Copy, Clone)]
pub enum TaskType {
    AbstractTask,
    Action,
}

pub const ABSTRACT_TASK: &str = "abstract-task";
pub const ACTION: &str = "action";

impl Display for TaskType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TaskType::AbstractTask => ABSTRACT_TASK,
                TaskType::Action => ACTION,
            }
        )
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub struct TaskFilter {
    pub task_type: Option<TaskType>,
    pub status: Option<TaskStatus>,
}
