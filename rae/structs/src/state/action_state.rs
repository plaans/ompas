use crate::interval::{Duration, Interval, Timepoint};
use crate::plan::Plan;
use crate::select_mode::SelectMode;
use crate::state::action_status::ActionStatus;
//use crate::state::task_status::TaskStatus;
use crate::ActionId;
use itertools::Itertools;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use tokio::sync::{watch, RwLock};

#[derive(Clone, Default)]
pub struct ActionCollection {
    pub inner: Arc<RwLock<im::HashMap<ActionId, ActionMetaData>>>,
}

impl ActionCollection {
    /*
    FORMAT
     */
    pub async fn format(&self, filter: TaskFilter) -> String {
        let inner: im::HashMap<usize, ActionMetaData> = self.get_inner().await;

        let inner: Vec<ActionMetaData> = inner
            .values()
            .filter(|&t| {
                if let Some(task_type) = &filter.task_type {
                    match task_type {
                        TaskType::Task => t.is_task(),
                        TaskType::Command => t.is_command(),
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
    pub async fn is_command(&self, id: &ActionId) -> bool {
        self.inner.read().await.get(id).unwrap().is_command()
    }

    pub async fn is_task(&self, id: &ActionId) -> bool {
        self.inner.read().await.get(id).unwrap().is_task()
    }

    /*
    GETTERS
     */
    pub async fn get_inner(&self) -> im::HashMap<ActionId, ActionMetaData> {
        self.inner.read().await.clone()
    }

    pub async fn get_status(&self, id: &ActionId) -> ActionStatus {
        self.inner.read().await.get(id).unwrap().get_status()
    }

    pub async fn get(&self, id: &ActionId) -> ActionMetaData {
        self.inner.read().await.get(id).unwrap().clone()
    }

    /*
    SETTERS
     */
    pub async fn insert(&self, id: ActionId, task: impl Into<ActionMetaData>) {
        self.inner.write().await.insert(id, task.into());
    }

    pub async fn update(&self, id: &ActionId, task: impl Into<ActionMetaData>) {
        let mut locked = self.inner.write().await;

        if locked.contains_key(id) {
            locked.insert(*id, task.into());
        }
    }

    pub async fn update_status(&self, id: &ActionId, status: ActionStatus) {
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
pub enum ActionMetaData {
    Task(TaskMetaData),
    Command(CommandMetaData),
}

impl ActionMetaData {
    pub fn is_task(&self) -> bool {
        match self {
            ActionMetaData::Task(_) => true,
            ActionMetaData::Command(_) => false,
        }
    }

    pub fn is_command(&self) -> bool {
        match self {
            ActionMetaData::Task(_) => false,
            ActionMetaData::Command(_) => true,
        }
    }

    pub fn get_status(&self) -> ActionStatus {
        match self {
            ActionMetaData::Task(a) => a.status,
            ActionMetaData::Command(a) => a.status,
        }
    }

    pub async fn update_status(&mut self, status: ActionStatus) {
        match self {
            ActionMetaData::Task(a) => a.update_status(status),
            ActionMetaData::Command(a) => a.update_status(status).await,
        }
    }

    pub fn get_id(&self) -> ActionId {
        match self {
            ActionMetaData::Task(a) => a.id,
            ActionMetaData::Command(a) => a.id,
        }
    }

    pub fn get_label(&self) -> LValue {
        match self {
            ActionMetaData::Task(a) => a.label.clone(),
            ActionMetaData::Command(a) => a.label.clone(),
        }
    }

    pub fn set_end_timepoint(&mut self, end: Timepoint) {
        match self {
            ActionMetaData::Task(a) => a.set_end_timepoint(end),
            ActionMetaData::Command(a) => a.set_end_timepoint(end),
        }
    }

    pub async fn set_as_done(&mut self, end: Timepoint) {
        self.update_status(ActionStatus::Success).await;
        self.set_end_timepoint(end);
    }

    pub async fn set_as_failure(&mut self, end: Timepoint) {
        self.update_status(ActionStatus::Failure).await;
        self.set_end_timepoint(end);
    }
}
impl Display for ActionMetaData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ActionMetaData::Task(a) => a.to_string(),
                ActionMetaData::Command(a) => a.to_string(),
            }
        )
    }
}

impl From<TaskMetaData> for ActionMetaData {
    fn from(a: TaskMetaData) -> Self {
        Self::Task(a)
    }
}

impl From<CommandMetaData> for ActionMetaData {
    fn from(a: CommandMetaData) -> Self {
        Self::Command(a)
    }
}

impl TryFrom<ActionMetaData> for CommandMetaData {
    type Error = LRuntimeError;

    fn try_from(value: ActionMetaData) -> Result<Self, Self::Error> {
        if let ActionMetaData::Command(a) = value {
            Ok(a)
        } else {
            Err(Default::default())
        }
    }
}
impl TryFrom<ActionMetaData> for TaskMetaData {
    type Error = LRuntimeError;

    fn try_from(value: ActionMetaData) -> Result<Self, Self::Error> {
        if let ActionMetaData::Task(a) = value {
            Ok(a)
        } else {
            Err(Default::default())
        }
    }
}

pub trait ActionMetaDataView {
    fn get_label(&self) -> &LValue;

    fn get_id(&self) -> &ActionId;

    fn get_status(&self) -> &ActionStatus;

    fn get_parent_task(&self) -> Option<usize>;

    fn set_end_timepoint(&mut self, end: Timepoint);

    fn get_start(&self) -> &Timepoint;

    fn get_end(&self) -> &Option<Timepoint>;

    fn get_duration(&self) -> Duration;
}

#[derive(Debug)]
pub struct CommandMetaData {
    id: ActionId,
    parent: usize,
    label: LValue,
    status: ActionStatus,
    interval: Interval,
    sender_to_watcher: Option<watch::Sender<ActionStatus>>,
}

impl Clone for CommandMetaData {
    fn clone(&self) -> Self {
        Self {
            id: self.id,
            parent: self.parent,
            label: self.label.clone(),
            status: self.status,
            interval: self.interval.clone(),
            sender_to_watcher: None,
        }
    }
}

impl CommandMetaData {
    pub fn new(
        id: ActionId,
        parent: usize,
        label: LValue,
        start: Timepoint,
    ) -> (Self, watch::Receiver<ActionStatus>) {
        let (tx, rx) = watch::channel(ActionStatus::Pending);

        (
            Self {
                id,
                parent,
                label,
                status: ActionStatus::Pending,
                interval: Interval { start, end: None },
                sender_to_watcher: Some(tx),
            },
            rx,
        )
    }

    pub async fn update_status(&mut self, status: ActionStatus) {
        self.status = status;
        if let Some(tx) = &self.sender_to_watcher {
            if tx.send(self.status).is_err() {
                self.sender_to_watcher = None;
            }
        }
    }
}

impl ActionMetaDataView for CommandMetaData {
    fn get_label(&self) -> &LValue {
        &self.label
    }

    fn get_id(&self) -> &ActionId {
        &self.id
    }

    fn get_status(&self) -> &ActionStatus {
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

impl Display for CommandMetaData {
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
pub struct TaskMetaData {
    id: ActionId,
    parent_task: Option<usize>,
    status: ActionStatus,
    label: LValue,
    current_method: LValue,
    refinement: Vec<RefinementMetaData>,
    interval: Interval,
    tried: Vec<LValue>,
}

impl TaskMetaData {
    pub fn new(label: LValue, id: usize, parent_task: Option<usize>, start: Timepoint) -> Self {
        Self {
            id,
            parent_task,
            status: ActionStatus::Pending,
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

impl ActionMetaDataView for TaskMetaData {
    fn get_label(&self) -> &LValue {
        &self.label
    }

    fn get_id(&self) -> &ActionId {
        &self.id
    }

    fn get_status(&self) -> &ActionStatus {
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

impl TaskMetaData {
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

    pub fn get_last_refinement(&self) -> Option<&RefinementMetaData> {
        self.refinement.last()
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

    pub fn update_status(&mut self, status: ActionStatus) {
        self.status = status;
    }
}

impl Display for TaskMetaData {
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
    Task,
    Command,
}

pub const TASK: &str = "task";
pub const COMMAND: &str = "command";

impl Display for TaskType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TaskType::Task => TASK,
                TaskType::Command => COMMAND,
            }
        )
    }
}

#[derive(Copy, Clone, Default, Debug)]
pub struct TaskFilter {
    pub task_type: Option<TaskType>,
    pub status: Option<ActionStatus>,
}
