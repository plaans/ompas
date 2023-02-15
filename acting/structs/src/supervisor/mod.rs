use crate::state::action_status::ActionStatus;
use crate::supervisor::acquire::AcquireProcess;
use crate::supervisor::arbitrary::ArbitraryProcess;
use crate::supervisor::command::CommandProcess;
use crate::supervisor::interval::Timepoint;
use crate::supervisor::method::MethodProcess;
use crate::supervisor::process::{ActingProcess, ProcessOrigin};
use crate::supervisor::process_ref::{Label, MethodLabel, ProcessRef};
use crate::supervisor::task::TaskProcess;
use process::ActingProcessInner;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::Instant;

pub mod acquire;
pub mod agenda;
pub mod arbitrary;
pub mod command;
pub mod interval;
pub mod method;
pub mod process;
pub mod process_ref;
pub mod root_task;
pub mod task;

pub type ActingProcessId = usize;

#[derive(Clone, Default)]
pub struct Supervisor {
    pub inner: Arc<RwLock<InnerSupervisor>>,
}

impl Supervisor {
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

    pub async fn get_instant(&self) -> Timepoint {
        self.inner.read().await.get_instant()
    }

    pub async fn get_tried_method(&self, id: ActingProcessId) -> Vec<LValue> {
        self.inner.read().await.get_tried_method(id)
    }
}

pub struct InnerSupervisor {
    inner: Vec<ActingProcess>,
    time_reference: Instant,
}

impl Default for InnerSupervisor {
    fn default() -> Self {
        Self {
            inner: vec![ActingProcess::root()],
            time_reference: Instant::now(),
        }
    }
}

impl InnerSupervisor {
    pub fn get_instant(&self) -> Timepoint {
        self.time_reference.elapsed().as_micros()
    }

    pub fn get_id(&self, path: ProcessRef) -> Option<ActingProcessId> {
        match path {
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
                                id = if let Some(id) = t.methods.get(m) {
                                    *id
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                    }
                }
                Some(id)
            }
        }
    }

    //New processes
    pub fn new_high_level_task(&mut self, debug: String) -> ActingProcessId {
        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            ProcessOrigin::Execution,
            TaskProcess::new(id, 0, debug, Some(self.get_instant())),
        ));
        self.inner[0]
            .inner
            .as_mut_root()
            .unwrap()
            .add_top_level_task(id);
        id
    }

    pub fn new_task(
        &mut self,
        label: MethodLabel,
        parent: ActingProcessId,
        debug: String,
        planned: bool,
    ) -> ActingProcessId {
        let (origin, start) = match planned {
            true => (ProcessOrigin::Planning, None),
            false => (ProcessOrigin::Execution, Some(self.get_instant())),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            TaskProcess::new(id, parent, debug, start),
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
            MethodProcess::new(id, parent, value, start),
        ));
        self.inner[parent]
            .inner
            .as_mut_task()
            .unwrap()
            .add_method(id);
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
    ) -> ActingProcessId {
        let (origin, start) = match planned {
            true => (ProcessOrigin::Planning, None),
            false => (ProcessOrigin::Execution, Some(self.get_instant())),
        };

        let id = self.inner.len();
        self.inner.push(ActingProcess::new(
            origin,
            CommandProcess::new(id, parent, value, start),
        ));
        self.inner[parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        id
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
            .methods
            .clone();

        methods
            .drain(..)
            .map(|m| {
                self.get(id)
                    .unwrap()
                    .inner
                    .as_method()
                    .unwrap()
                    .value
                    .clone()
            })
            .collect()
    }
}
