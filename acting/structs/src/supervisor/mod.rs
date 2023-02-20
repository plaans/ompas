use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::filter::ProcessFilter;
use crate::supervisor::inner::ProcessKind;
use crate::supervisor::interval::Timepoint;
use crate::supervisor::process::process_ref::{MethodLabel, ProcessRef};
use crate::supervisor::process::task::RefinementTrace;
use crate::supervisor::process::ProcessOrigin;
use inner::InnerSupervisor;
use sompas_structs::lvalue::LValue;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::watch::Receiver;
use tokio::sync::RwLock;
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

    pub async fn get_origin(&self, id: ActingProcessId) -> Option<ProcessOrigin> {
        self.inner.read().await.get(id).map(|ap| ap.origin)
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

    pub fn get_instant(&self) -> Timepoint {
        self.instant.elapsed().as_micros()
    }

    pub async fn get_tried_method(&self, id: ActingProcessId) -> Vec<LValue> {
        self.inner.read().await.get_tried_method(id)
    }

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

    pub async fn new_method(
        &self,
        parent: ActingProcessId,
        debug: String,
        value: LValue,
        trace: RefinementTrace,
        planned: bool,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_method(parent, debug, value, trace, planned)
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
            .set_end(self.get_instant())
    }

    // Arbitrary methods
    pub async fn new_arbitrary(
        &self,
        label: MethodLabel,
        parent: ActingProcessId,
        value: LValue,
        planned: bool,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_arbitrary(label, parent, value, planned)
    }

    // Arbitrary methods
    pub async fn new_acquire(
        &self,
        label: MethodLabel,
        parent: ActingProcessId,
        planned: bool,
    ) -> ActingProcessId {
        self.inner.write().await.new_acquire(label, parent, planned)
    }
}
