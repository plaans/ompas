use crate::conversion::flow_graph::graph::Dot;
use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::interval::Timepoint;
use crate::supervisor::process::acquire::AcquireProcess;
use crate::supervisor::process::arbitrary::ArbitraryProcess;
use crate::supervisor::process::command::CommandProcess;
use crate::supervisor::process::method::MethodProcess;
use crate::supervisor::process::process_ref::{Label, MethodLabel, ProcessRef};
use crate::supervisor::process::root_task::RootProcess;
use crate::supervisor::process::task::{Refinement, RefinementTrace, TaskProcess};
use crate::supervisor::process::{ActingProcess, ActingProcessInner, ProcessOrigin};
use crate::supervisor::ActingProcessId;
use chrono::{DateTime, Utc};
use sompas_structs::lvalue::LValue;
use std::env::set_current_dir;
use std::fmt::Write;
use std::fs;
use std::fs::File;
use std::io::Write as ioWrite;
use std::path::PathBuf;
use tokio::sync::watch;
use tokio::sync::watch::Receiver;
use tokio::time::Instant;

const COLOR_PLANNING: &str = "red";
const COLOR_EXECUTION: &str = "blue";

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ProcessKind {
    Method,
    Acquire,
    Arbitrary,
    Command,
    Task,
    RootTask,
}

pub struct InnerSupervisor {
    inner: Vec<ActingProcess>,
    time_reference: Instant,
}

impl InnerSupervisor {
    pub fn new(time_reference: Instant) -> Self {
        Self {
            inner: vec![ActingProcess::root()],
            time_reference,
        }
    }

    pub fn get_instant(&self) -> Timepoint {
        self.time_reference.elapsed().as_micros()
    }

    pub fn get_id(&self, pr: impl Into<ProcessRef>) -> Option<ActingProcessId> {
        let pr = pr.into();
        match pr {
            ProcessRef::Id(id) => Some(id),
            ProcessRef::Relative(id, mut labels) => {
                let mut id = id;
                labels.reverse();
                while let Some(label) = labels.pop() {
                    let obj = &self.inner[id];
                    match label {
                        Label::MethodProcess(m) => {
                            if let ActingProcessInner::Method(mp) = &obj.inner {
                                id = if let Some(id) = mp.process_set.get(&m) {
                                    *id
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }

                        Label::Method(m) => {
                            if let ActingProcessInner::Task(t) = &obj.inner {
                                id = if let Some(r) = t.refinements.get(m) {
                                    r.method
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                        Label::HighLevelTask(rank) => {
                            id = self.inner[0]
                                .inner
                                .as_root()
                                .unwrap()
                                .nth_task(rank)
                                .unwrap()
                        }
                    }
                }
                Some(id)
            }
        }
    }

    pub fn get_kind(&self, id: ActingProcessId) -> Option<ProcessKind> {
        self.get(id).map(|ap| ap.inner.kind())
    }

    //New processes
    pub fn new_high_level_task(&mut self, value: LValue) -> ProcessRef {
        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            ProcessOrigin::Execution,
            TaskProcess::new(id, 0, value, Some(self.get_instant())),
        ));

        let root: &mut RootProcess = self.inner[0].inner.as_mut_root().unwrap();
        let rank = root.n_task();
        root.add_top_level_task(id);

        ProcessRef::Relative(0, vec![Label::HighLevelTask(rank)])
    }

    pub fn new_task(
        &mut self,
        label: MethodLabel,
        parent: ActingProcessId,
        value: LValue,
        planned: bool,
    ) -> ActingProcessId {
        let (origin, start) = match planned {
            true => (ProcessOrigin::Planning, None),
            false => (ProcessOrigin::Execution, Some(self.get_instant())),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            TaskProcess::new(id, parent, value, start),
        ));
        self.inner[parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        id
    }

    pub fn new_method(
        &mut self,
        parent: ActingProcessId,
        debug: String,
        value: LValue,
        trace: RefinementTrace,
        planned: bool,
    ) -> ActingProcessId {
        let (origin, start) = match planned {
            true => (ProcessOrigin::Planning, None),
            false => (ProcessOrigin::Execution, Some(self.get_instant())),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            MethodProcess::new(id, parent, debug, value, start),
        ));
        self.inner[parent]
            .inner
            .as_mut_task()
            .unwrap()
            .add_refinement(Refinement { method: id, trace });
        id
    }

    pub fn new_arbitrary(
        &mut self,
        label: MethodLabel,
        parent: ActingProcessId,
        value: LValue,
        planned: bool,
    ) -> ActingProcessId {
        let (origin, timepoint, suggested, chosen) = match planned {
            true => (ProcessOrigin::Planning, None, Some(value), None),
            false => (
                ProcessOrigin::Execution,
                Some(self.get_instant()),
                None,
                Some(value),
            ),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            ArbitraryProcess::new(id, parent, suggested, chosen, timepoint),
        ));
        self.inner[parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        id
    }

    pub fn new_acquire(
        &mut self,
        label: MethodLabel,
        parent: ActingProcessId,
        planned: bool,
    ) -> ActingProcessId {
        let (origin, request_date) = match planned {
            true => (ProcessOrigin::Planning, None),
            false => (ProcessOrigin::Execution, Some(self.get_instant())),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            AcquireProcess::new(id, parent, request_date),
        ));
        self.inner[parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        id
    }

    pub fn new_command(
        &mut self,
        label: MethodLabel,
        parent: ActingProcessId,
        value: LValue,
        planned: bool,
    ) -> (ActingProcessId, Option<Receiver<ActionStatus>>) {
        let (origin, start) = match planned {
            true => (ProcessOrigin::Planning, None),
            false => (ProcessOrigin::Execution, Some(self.get_instant())),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            CommandProcess::new(id, parent, value, start),
        ));

        let watch = if planned {
            None
        } else {
            let (tx, rx) = watch::channel(ActionStatus::Pending);
            self.inner[id].inner.as_mut_command().unwrap().set_watch(tx);
            Some(rx)
        };

        self.inner[parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        (id, watch)
    }

    /*
    Get process inner struct
     */
    pub fn get(&self, process_ref: impl Into<ProcessRef>) -> Option<&ActingProcess> {
        self.get_id(process_ref.into()).map(|id| &self.inner[id])
    }

    pub fn get_mut(&mut self, process_ref: impl Into<ProcessRef>) -> Option<&mut ActingProcess> {
        self.get_id(process_ref.into())
            .map(|id| &mut self.inner[id])
    }

    pub fn get_tried_method(&self, id: ActingProcessId) -> Vec<LValue> {
        let mut methods = self
            .get(id)
            .unwrap()
            .inner
            .as_task()
            .unwrap()
            .get_tried_method();

        methods
            .drain(..)
            .map(|m| {
                self.get(m)
                    .unwrap()
                    .inner
                    .as_method()
                    .unwrap()
                    .value
                    .clone()
            })
            .collect()
    }

    pub fn export_trace_dot_graph(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();
        let mut queue = vec![0];

        while let Some(id) = queue.pop() {
            let ap = &self.inner[id];
            let label = ap.inner.to_string();
            let color = match ap.origin {
                ProcessOrigin::Execution => COLOR_EXECUTION,
                ProcessOrigin::Planning => COLOR_PLANNING,
            };

            writeln!(dot, "P{id} [label = \"{label}\", color = {color}];").unwrap();
            match &ap.inner {
                ActingProcessInner::RootTask(rt) => {
                    for st in &rt.tasks {
                        writeln!(dot, "P{id} -> P{st};").unwrap();
                        queue.push(*st)
                    }
                }
                ActingProcessInner::Command(_) => {}
                ActingProcessInner::Task(t) => {
                    for r in t.refinements.iter().map(|p| p.method) {
                        writeln!(dot, "P{id} -> P{};", r).unwrap();
                        queue.push(r)
                    }
                }
                ActingProcessInner::Method(m) => {
                    for sub in m.process_set.values() {
                        writeln!(dot, "P{id} -> P{sub};").unwrap();
                        queue.push(*sub)
                    }
                }
                ActingProcessInner::Arbitrary(_) => {}
                ActingProcessInner::Acquire(_) => {}
            }
        }

        dot.push('}');
        dot
    }

    pub fn dump_trace(&self, path: Option<PathBuf>) {
        let mut path = match path {
            None => "/tmp".into(),
            Some(p) => p,
        };
        let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
        let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
        path.push(format!("supervisor-trace_{}", string_date));
        fs::create_dir_all(&path).unwrap();

        let mut path_dot = path.clone();
        let dot_file_name = "trace.dot";
        path_dot.push(&dot_file_name);
        let mut file = File::create(&path_dot).unwrap();
        let dot = self.export_trace_dot_graph();
        file.write_all(dot.as_bytes()).unwrap();
        set_current_dir(&path).unwrap();
        let trace = "trace.png";
        std::process::Command::new("dot")
            .args(["-Tpng", &dot_file_name, "-o", &trace])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut md_path = path.clone();
        let md_file_name = "trace.md";
        md_path.push(&md_file_name);
        let mut md_file = File::create(&md_path).unwrap();
        let md: String = format!(
            "
## Trace
\n
![]({})
\n",
            trace
        );

        md_file.write_all(md.as_bytes()).unwrap();

        std::process::Command::new("google-chrome")
            .arg(&md_file_name)
            .spawn()
            .unwrap();
    }
}
/*
impl InnerSupervisor {
    pub async fn clear(&self) {
        *self.trc.inner.write().await = Default::default();
        *self.tn.subtasks.write().await = Default::default();
        loop {
            let id = self.next_id.load(Ordering::Relaxed);
            if self
                .next_id
                .compare_exchange(id, id + 1, Ordering::Acquire, Ordering::Relaxed) //Equivalent to compare_and_swap
                .is_ok()
            {
                break;
            }
        }
    }

    /*
    GETTERS
     */
    pub async fn get_refinement_method(&self, id: &ActionId) -> Option<SelectMode> {
        if let ActionMetaData::Task(task) = self.trc.get(id).await {
            let r = task.get_last_refinement();
            r.map(|ok| ok.refinement_type)
        } else {
            None
        }
    }

    pub async fn get_execution_time(&self, id: &ActionId) -> Duration {
        let task: TaskMetaData = self.trc.get(id).await.try_into().unwrap();
        task.get_duration()
    }

    pub async fn get_number_of_subtasks(&self, id: &ActionId) -> usize {
        self.tn.get_subtasks(id).await.len()
    }

    pub async fn get_number_of_subtasks_recursive(&self, id: &ActionId) -> usize {
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            n += 1;
            subtasks.append(&mut self.tn.get_subtasks(&c).await);
        }

        n
    }

    pub async fn get_number_of_abstract_tasks(&self, id: &ActionId) -> usize {
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_task(id).await {
                n += 1;
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
            }
        }
        n
    }

    pub async fn get_number_of_actions(&self, id: &ActionId) -> usize {
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_command(&c).await {
                n += 1;
            } else {
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
            }
        }
        n
    }

    pub async fn get_total_number_of_refinement(&self, id: &ActionId) -> usize {
        let task: TaskMetaData = self.trc.get(id).await.try_into().unwrap();
        let mut n = task.get_number_of_refinement();
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_task(&c).await {
                let task: TaskMetaData = self.trc.get(&c).await.try_into().unwrap();
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
                n += task.get_number_of_refinement();
            }
        }
        n
    }

    pub async fn get_total_refinement_time(&self, id: &ActionId) -> Duration {
        let mut total_time: Duration = Duration::Finite(0);
        let task: TaskMetaData = self.trc.get(id).await.try_into().unwrap();
        total_time += task.get_total_refinement_time();
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_task(&c).await {
                let task: TaskMetaData = self.trc.get(&c).await.try_into().unwrap();
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
                total_time += task.get_total_refinement_time();
            }
        }
        total_time
    }

    pub async fn get_stats(&self) -> LValue {
        let mut map: im::HashMap<LValue, LValue> = Default::default();
        let task_collection: im::HashMap<ActionId, ActionMetaData> =
            self.trc.inner.read().await.clone();
        let parent: Vec<ActionId> = self.tn.get_parents().await;
        for p in &parent {
            let mut task_stats: im::HashMap<LValue, LValue> = Default::default();
            task_stats.insert(
                string!(REFINEMENT_METHOD),
                match self.get_refinement_method(p).await {
                    Some(s) => s.to_string(),
                    None => "none".to_string(),
                }
                    .into(),
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
        let task_collection: im::HashMap<ActionId, ActionMetaData> =
            self.trc.inner.read().await.clone();
        let parent: Vec<ActionId> = self.tn.get_parents().await;
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
                    match self.get_refinement_method(p).await {
                        Some(s) => s.to_string(),
                        None => "none".to_string(),
                    },
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

    pub async fn add_task(&self, task: LValue, parent_task: Option<usize>) -> TaskMetaData {
        let task_id = self.get_next_id();
        let start = self.time_reference.elapsed().as_micros();
        let stack = TaskMetaData::new(task, task_id, parent_task, start);
        self.trc.insert(task_id, stack.clone()).await;
        if let Some(parent_task) = parent_task {
            self.tn.add_task_to_parent(parent_task, task_id).await;
        } else {
            self.tn.add_new_root_task(task_id).await;
        }
        stack
    }

    pub async fn add_command(
        &self,
        action: LValue,
        parent_task: Option<usize>,
    ) -> (ActionId, watch::Receiver<ActionStatus>) {
        let task_id = self.get_next_id();
        let start = self.time_reference.elapsed().as_micros();
        let mut parent = false;
        let parent_task = match parent_task {
            None => task_id,
            Some(id) => {
                parent = true;
                id
            }
        };

        let (action, rx) = CommandMetaData::new(task_id, parent_task, action, start);
        self.trc.insert(task_id, action).await;
        if parent {
            self.tn.add_task_to_parent(parent_task, task_id).await;
        }
        (task_id, rx)
    }

    #[function_name::named]
    pub async fn get_task(&self, task_id: &ActionId) -> Result<TaskMetaData, LRuntimeError> {
        match self.trc.get(task_id).await {
            ActionMetaData::Task(a) => Ok(a),
            ActionMetaData::Command(_) => Err(lruntimeerror!(
                function_name!(),
                format!("{} does not exist", task_id)
            )),
        }
    }

    pub async fn update_task(&self, id: &ActionId, task: impl Into<ActionMetaData>) {
        //println!("in update stack\n stack: {}", rs);
        self.trc.update(id, task).await
    }

    pub async fn update_status(&self, id: &ActionId, status: ActionStatus) {
        self.trc.update_status(id, status).await
    }

    pub async fn get_status(&self, id: &ActionId) -> ActionStatus {
        self.trc.get_status(id).await
    }

    pub async fn get_action_collection(&self) -> im::HashMap<ActionId, ActionMetaData> {
        self.trc.get_inner().await
    }

    pub async fn set_end_time(&self, id: &ActionId) {
        let end = self.time_reference.elapsed().as_micros();
        let mut task: ActionMetaData = self.trc.get(id).await;
        task.set_end_timepoint(end);
        self.trc.update(id, task).await;
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}
*/
