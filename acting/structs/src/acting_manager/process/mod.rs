use crate::acting_manager::action_status::ProcessStatus;
use crate::acting_manager::action_status::ProcessStatus::Pending;
use crate::acting_manager::inner::ProcessKind;
use crate::acting_manager::interval::Timepoint;
use crate::acting_manager::process::acquire::AcquireProcess;
use crate::acting_manager::process::action::ActionProcess;
use crate::acting_manager::process::arbitrary::ArbitraryProcess;
use crate::acting_manager::process::method::RefinementProcess;
use crate::acting_manager::process::plan_var::ExecutionVar;
use crate::acting_manager::process::root_task::RootProcess;
use crate::acting_manager::{AMId, ActingProcessId};
use std::fmt::{Display, Formatter};
use tokio::sync::watch;

pub mod acquire;
pub mod action;
pub mod arbitrary;
pub mod method;
pub mod plan_var;
pub mod process_ref;
pub mod root_task;
pub mod task;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ProcessOrigin {
    Planner,
    Execution,
    PlannerDropped,
    ExecPlanInherited,
}

impl ProcessOrigin {
    pub fn is_exec(&self) -> bool {
        matches!(self, Self::ExecPlanInherited | Self::Execution)
    }
}

pub struct ActingProcess {
    id: ActingProcessId,
    _parent: ActingProcessId,
    am_id: AMId,
    debug: Option<String>,
    pub origin: ProcessOrigin,
    pub status: ProcessStatus,
    pub start: ExecutionVar<Timepoint>,
    pub end: ExecutionVar<Timepoint>,
    pub inner: ActingProcessInner,
    pub status_update: Option<watch::Sender<ProcessStatus>>,
}

impl Display for ActingProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}, {})[{},{}]",
            self.id, self.status, self.start, self.end
        )?;
        let debug = if let Some(debug) = &self.debug {
            debug.to_string()
        } else {
            "".to_string()
        };
        match &self.inner {
            ActingProcessInner::RootTask(_) => {
                write!(f, "root")
            }
            ActingProcessInner::Action(_) => {
                write!(f, "{}", debug)
            }
            ActingProcessInner::Method(_) => {
                write!(f, "{}", debug)
            }
            ActingProcessInner::Arbitrary(arb) => {
                write!(f, "arb({})", arb.var)
            }
            ActingProcessInner::Acquire(acq) => {
                write!(f, "{}: acq({},{})", acq.s_acq, acq.resource, acq.quantity)
            }
        }
    }
}

impl ActingProcess {
    pub fn new(
        id: ActingProcessId,
        _parent: ActingProcessId,
        origin: ProcessOrigin,
        om_id: AMId,
        debug: Option<String>,
        start: ExecutionVar<Timepoint>,
        end: ExecutionVar<Timepoint>,
        inner: impl Into<ActingProcessInner>,
    ) -> Self {
        Self {
            id,
            _parent,
            am_id: om_id,
            debug,
            origin,
            status: Pending,
            start,
            end,
            inner: inner.into(),
            status_update: None,
        }
    }

    pub fn id(&self) -> ActingProcessId {
        self.id
    }

    pub fn parent(&self) -> ActingProcessId {
        self._parent
    }

    pub fn om_id(&self) -> AMId {
        self.am_id
    }

    pub fn debug(&self) -> &Option<String> {
        &self.debug
    }

    pub fn set_status(&mut self, status: ProcessStatus) {
        self.status = status;
        if let Some(sender) = &mut self.status_update {
            sender.send(status).unwrap_or_else(|e| panic!("{}", e));
        }
    }

    pub fn dropped(&mut self) {
        self.origin = ProcessOrigin::PlannerDropped;
    }

    pub fn executed(&mut self) {
        self.origin = ProcessOrigin::ExecPlanInherited;
    }

    pub fn get_om_id(&self) -> AMId {
        self.am_id
    }

    pub fn get_inner(&self) -> &ActingProcessInner {
        &self.inner
    }

    pub fn get_mut_inner(&mut self) -> &mut ActingProcessInner {
        &mut self.inner
    }
}

pub enum ActingProcessInner {
    RootTask(RootProcess),
    Action(ActionProcess),
    Method(RefinementProcess),
    Arbitrary(ArbitraryProcess),
    Acquire(AcquireProcess),
}

impl Display for ActingProcessInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActingProcessInner::RootTask(r) => write!(f, "{r}"),
            ActingProcessInner::Action(r) => write!(f, "{r}"),
            ActingProcessInner::Method(r) => write!(f, "{r}"),
            ActingProcessInner::Arbitrary(r) => write!(f, "{r}"),
            ActingProcessInner::Acquire(r) => write!(f, "{r}"),
        }
    }
}

impl ActingProcessInner {
    pub fn kind(&self) -> ProcessKind {
        match self {
            ActingProcessInner::RootTask(_) => ProcessKind::RootTask,
            ActingProcessInner::Action(_) => ProcessKind::Action,
            ActingProcessInner::Method(_) => ProcessKind::Method,
            ActingProcessInner::Arbitrary(_) => ProcessKind::Arbitrary,
            ActingProcessInner::Acquire(_) => ProcessKind::Acquire,
        }
    }

    pub fn as_root(&self) -> Option<&RootProcess> {
        if let Self::RootTask(root) = self {
            Some(root)
        } else {
            None
        }
    }

    pub fn as_mut_root(&mut self) -> Option<&mut RootProcess> {
        if let Self::RootTask(root) = self {
            Some(root)
        } else {
            None
        }
    }

    pub fn as_method(&self) -> Option<&RefinementProcess> {
        if let Self::Method(method) = self {
            Some(method)
        } else {
            None
        }
    }

    pub fn as_mut_method(&mut self) -> Option<&mut RefinementProcess> {
        if let Self::Method(method) = self {
            Some(method)
        } else {
            None
        }
    }

    pub fn as_arbitrary(&self) -> Option<&ArbitraryProcess> {
        if let Self::Arbitrary(arbitrary) = self {
            Some(arbitrary)
        } else {
            None
        }
    }

    pub fn as_mut_arbitrary(&mut self) -> Option<&mut ArbitraryProcess> {
        if let Self::Arbitrary(arbitrary) = self {
            Some(arbitrary)
        } else {
            None
        }
    }

    pub fn as_acquire(&self) -> Option<&AcquireProcess> {
        if let Self::Acquire(acquire) = self {
            Some(acquire)
        } else {
            None
        }
    }

    pub fn as_mut_acquire(&mut self) -> Option<&mut AcquireProcess> {
        if let Self::Acquire(acquire) = self {
            Some(acquire)
        } else {
            None
        }
    }

    pub fn as_action(&self) -> Option<&ActionProcess> {
        if let Self::Action(action) = self {
            Some(action)
        } else {
            None
        }
    }

    pub fn as_mut_action(&mut self) -> Option<&mut ActionProcess> {
        if let Self::Action(action) = self {
            Some(action)
        } else {
            None
        }
    }
}