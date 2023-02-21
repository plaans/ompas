use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::filter::ProcessFilter;
use crate::supervisor::inner::ProcessKind;
use crate::supervisor::interval::Timepoint;
use crate::supervisor::process::arbitrary::ArbitraryChoice::Planning;
use crate::supervisor::process::arbitrary::{ArbitraryChoice, ArbitraryProcess, ArbitraryTrace};
use crate::supervisor::process::command::CommandProcess;
use crate::supervisor::process::process_ref::{MethodLabel, ProcessRef};
use crate::supervisor::process::task::{Refinement, RefinementInner};
use crate::supervisor::process::{ActingProcess, ProcessStatus};
use inner::InnerSupervisor;
use sompas_structs::lvalue::LValue;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::watch::Receiver;
use tokio::sync::{watch, RwLock};
use tokio::time::Instant;

pub mod filter;
pub mod inner;
pub mod interval;
pub mod process;

pub mod action_status;
pub mod task_network;

pub type ActingProcessId = usize;

#[derive(Clone)]
pub struct Supervisor {
    pub inner: Arc<RwLock<InnerSupervisor>>,
    instant: Instant,
}

impl Default for Supervisor {
    fn default() -> Self {
        let instant = Instant::now();
        Self {
            inner: Arc::new(RwLock::new(InnerSupervisor::new(instant.clone()))),
            instant,
        }
    }
}

impl Supervisor {
    pub async fn clear(&self) {
        todo!()
    }

    pub async fn dump_trace(&self, path: Option<PathBuf>) {
        self.inner.read().await.dump_trace(path)
    }

    pub async fn format_task_network(&self) -> String {
        todo!()
    }

    pub async fn print_processes(&self, _pf: ProcessFilter) -> String {
        todo!()
    }

    pub async fn get_stats(&self) -> LValue {
        todo!()
    }

    pub async fn export_to_csv(
        &self,
        _working_dir: Option<PathBuf>,
        _file: Option<String>,
    ) -> LValue {
        todo!()
    }

    pub async fn get_id(&self, pr: impl Into<ProcessRef>) -> Option<ActingProcessId> {
        self.inner.read().await.get_id(pr)
    }

    pub async fn get_status(&self, id: ActingProcessId) -> Option<ProcessStatus> {
        self.inner.read().await.get(id).map(|ap| ap.status)
    }

    pub async fn get_kind(&self, id: ActingProcessId) -> Option<ProcessKind> {
        self.inner.read().await.get_kind(id)
    }

    pub async fn update_task_status(&self, id: impl Into<ProcessRef>, status: ActionStatus) {
        let mut inner = self.inner.write().await;
        inner
            .get_mut(id)
            .unwrap()
            .inner
            .as_mut_task()
            .unwrap()
            .set_status(status);
    }

    pub fn get_timepoint(&self) -> Timepoint {
        self.instant.elapsed().as_millis().into()
    }

    pub async fn get_tried_method(&self, id: ActingProcessId) -> Vec<LValue> {
        self.inner.read().await.get_tried_method(id)
    }

    //Task methods
    pub async fn new_task(
        &self,
        label: MethodLabel,
        parent: ActingProcessId,
        value: LValue,
        planned: bool,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_task(label, parent, value, planned)
    }

    pub async fn get_task_value(&self, id: &ActingProcessId) -> LValue {
        self.inner
            .read()
            .await
            .get(*id)
            .unwrap()
            .inner
            .as_task()
            .unwrap()
            .value
            .clone()
    }

    pub async fn get_task_planned_refinement(
        &self,
        task_id: &ActingProcessId,
    ) -> Option<Refinement> {
        self.inner
            .read()
            .await
            .get(*task_id)
            .unwrap()
            .inner
            .as_task()
            .unwrap()
            .get_planned_refinement()
            .cloned()
    }

    pub async fn update_task_last_refinement(
        &self,
        task_id: &ActingProcessId,
        refinement: Refinement,
    ) {
        self.inner
            .write()
            .await
            .get_mut(*task_id)
            .unwrap()
            .inner
            .as_mut_task()
            .unwrap()
            .update_last_refinement(refinement);
    }

    pub async fn set_task_last_refinement_end(
        &self,
        task_id: &ActingProcessId,
        status: ActionStatus,
    ) {
        let method_id = self
            .inner
            .read()
            .await
            .get(*task_id)
            .unwrap()
            .inner
            .as_task()
            .unwrap()
            .get_id_current_method()
            .unwrap();

        self.set_method_end(&method_id).await;
        self.set_method_status(&method_id, status).await;
    }

    pub async fn new_method(
        &self,
        parent: ActingProcessId,
        debug: String,
        refinement: RefinementInner,
        planned: bool,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_method(parent, debug, refinement, planned)
    }

    pub async fn set_method_end(&self, method_id: &ActingProcessId) {
        self.inner
            .write()
            .await
            .get_mut(*method_id)
            .unwrap()
            .inner
            .as_mut_method()
            .unwrap()
            .set_end(self.get_timepoint())
    }

    pub async fn set_method_status(&self, method_id: &ActingProcessId, status: ActionStatus) {
        self.inner
            .write()
            .await
            .get_mut(*method_id)
            .unwrap()
            .inner
            .as_mut_method()
            .unwrap()
            .set_status(status)
    }

    pub async fn get_number_arbitrary(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .process_set
            .keys()
            .filter(|p| matches!(p, MethodLabel::Arbitrary(_)))
            .count()
    }

    pub async fn get_number_subtask(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .process_set
            .keys()
            .filter(|p| matches!(p, MethodLabel::Subtask(_)))
            .count()
    }

    pub async fn get_number_command(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .process_set
            .keys()
            .filter(|p| matches!(p, MethodLabel::Command(_)))
            .count()
    }

    pub async fn get_number_acquire(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .process_set
            .keys()
            .filter(|p| matches!(p, MethodLabel::Acquire(_)))
            .count()
    }

    //Command methods
    pub async fn new_command(
        &self,
        label: MethodLabel,
        parent: ActingProcessId,
        value: LValue,
        planned: bool,
    ) -> (ActingProcessId, Option<Receiver<ActionStatus>>) {
        self.inner
            .write()
            .await
            .new_command(label, parent, value, planned)
    }

    pub async fn start_planned_command(
        &self,
        id_command: &ActingProcessId,
    ) -> watch::Receiver<ActionStatus> {
        let mut lock = self.inner.write().await;
        let process: &mut ActingProcess = lock.get_mut(*id_command).unwrap();
        process.status = ProcessStatus::Executed;
        let command: &mut CommandProcess = process.inner.as_mut_command().unwrap();
        command.set_start(self.get_timepoint());
        let (tx, rx) = watch::channel(ActionStatus::Pending);
        command.set_watch(tx);
        rx
    }

    pub async fn update_command_status(&self, id: impl Into<ProcessRef>, status: ActionStatus) {
        let mut inner = self.inner.write().await;
        inner
            .get_mut(id)
            .unwrap()
            .inner
            .as_mut_command()
            .unwrap()
            .set_status(status);
    }

    pub async fn set_command_end(&self, id: ActingProcessId) {
        self.inner
            .write()
            .await
            .get_mut(id)
            .unwrap()
            .inner
            .as_mut_command()
            .unwrap()
            .set_end(self.get_timepoint())
    }

    // Arbitrary methods
    pub async fn new_arbitrary(
        &self,
        label: MethodLabel,
        parent: ActingProcessId,
        possibilities: Vec<LValue>,
        choice: ArbitraryChoice,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_arbitrary(label, parent, possibilities, choice)
    }

    pub async fn try_set_planned_arbitrary(
        &self,
        id_arbitrary: &ActingProcessId,
        possibilities: Vec<LValue>,
        other: LValue,
    ) -> LValue {
        let mut lock = self.inner.write().await;
        let process: &mut ActingProcess = lock.get_mut(*id_arbitrary).unwrap();
        process.status = ProcessStatus::Executed;
        let arbitrary: &mut ArbitraryProcess = process.inner.as_mut_arbitrary().unwrap();
        let trace = arbitrary.get_last_trace();

        let default = ArbitraryTrace {
            possibilities: possibilities.clone(),
            choice: ArbitraryChoice::Execution(other),
            instant: self.get_timepoint(),
        };

        let trace = if let Planning(planned) = &trace.choice {
            if possibilities.contains(planned) {
                ArbitraryTrace {
                    possibilities,
                    choice: trace.choice.clone(),
                    instant: self.get_timepoint(),
                }
            } else {
                default
            }
        } else {
            default
        };
        let choosed = trace.choice.inner().clone();
        arbitrary.add_trace(trace);
        choosed
    }

    // Acquire methods
    pub async fn new_acquire(
        &self,
        resource_label: String,
        label: MethodLabel,
        parent: ActingProcessId,
        planned: bool,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_acquire(label, resource_label, parent, planned)
    }

    pub async fn set_acquire_request_timepoint(&self, acquire_id: &ActingProcessId) {
        self.inner
            .write()
            .await
            .get_mut(*acquire_id)
            .unwrap()
            .inner
            .as_mut_acquire()
            .unwrap()
            .set_request(self.get_timepoint());
    }

    pub async fn set_acquire_acquisition_start(&self, acquire_id: &ActingProcessId) {
        self.inner
            .write()
            .await
            .get_mut(*acquire_id)
            .unwrap()
            .inner
            .as_mut_acquire()
            .unwrap()
            .set_acquisition_start(self.get_timepoint());
    }

    pub async fn set_acquire_acquisition_end(&self, acquire_id: &ActingProcessId) {
        self.inner
            .write()
            .await
            .get_mut(*acquire_id)
            .unwrap()
            .inner
            .as_mut_acquire()
            .unwrap()
            .set_acquisition_start(self.get_timepoint());
    }
}
