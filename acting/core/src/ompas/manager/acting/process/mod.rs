use crate::model::process_ref::Label;
use crate::model::sym_domain::cst;
use crate::ompas::manager::acting::acting_var::{ActingVarCollection, ActingVarRef};
use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::process::acquire::AcquireProcess;
use crate::ompas::manager::acting::process::arbitrary::ArbitraryProcess;
use crate::ompas::manager::acting::process::command::CommandProcess;
use crate::ompas::manager::acting::process::refinement::RefinementProcess;
use crate::ompas::manager::acting::process::root_task::RootProcess;
use crate::ompas::manager::acting::process::task::TaskProcess;
use crate::ompas::manager::acting::{AMId, ActingProcessId};
use crate::ompas::manager::state::action_status::ProcessStatus;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use tokio::sync::watch;

pub mod acquire;

pub mod arbitrary;
pub mod command;
pub mod process_stat;
pub mod refinement;
pub mod root_task;
pub mod task;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ProcessOrigin {
    Planner,
    Execution,
}

impl ProcessOrigin {
    pub fn is_exec(&self) -> bool {
        matches!(self, Self::Execution)
    }
}

pub struct ActingProcess {
    id: ActingProcessId,
    parent: ActingProcessId,
    label: Option<Label>,
    am_id: AMId,
    debug: Option<String>,
    pub origin: ProcessOrigin,
    pub status: ProcessStatus,
    pub start: ActingVarRef<Timepoint>,
    pub end: ActingVarRef<Timepoint>,
    pub inner: ActingProcessInner,
    pub status_update: Option<watch::Sender<ProcessStatus>>,
}

impl ActingProcess {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        label: Option<Label>,
        origin: ProcessOrigin,
        am_id: AMId,
        debug: Option<String>,
        start: ActingVarRef<Timepoint>,
        end: ActingVarRef<Timepoint>,
        inner: impl Into<ActingProcessInner>,
    ) -> Self {
        Self {
            id,
            parent,
            label,
            am_id,
            debug,
            origin,
            status: if origin == ProcessOrigin::Planner {
                ProcessStatus::Planned
            } else {
                ProcessStatus::Pending
            },
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
        self.parent
    }

    pub fn am_id(&self) -> AMId {
        self.am_id
    }

    pub fn label(&self) -> Option<Label> {
        self.label
    }

    pub fn debug(&self) -> &Option<String> {
        &self.debug
    }

    pub fn set_debug(&mut self, debug: String) {
        self.debug = Some(debug);
    }

    pub fn set_status(&mut self, status: ProcessStatus) {
        self.status = status;
        if let Some(sender) = &mut self.status_update {
            if sender.send(status).is_err() {
                self.status_update = None;
            }
        }
    }

    pub fn get_am_id(&self) -> AMId {
        self.am_id
    }

    pub fn get_inner(&self) -> &ActingProcessInner {
        &self.inner
    }

    pub fn get_mut_inner(&mut self) -> &mut ActingProcessInner {
        &mut self.inner
    }

    pub fn format(&self, acting_vars: &ActingVarCollection) -> String {
        let mut f = String::new();
        write!(
            f,
            "({}, {})[{},{}]",
            self.id,
            self.status,
            acting_vars.format_acting_var(&self.start),
            acting_vars.format_acting_var(&self.end)
        )
        .unwrap();
        let debug = if let Some(debug) = &self.debug {
            debug.to_string()
        } else {
            "".to_string()
        };
        match &self.inner {
            ActingProcessInner::RootTask(_) => {
                write!(f, "root").unwrap();
            }
            ActingProcessInner::Method(_)
            | ActingProcessInner::AbstractModel(_)
            | ActingProcessInner::Task(_)
            | ActingProcessInner::Command(_) => {
                write!(f, "{}", debug).unwrap();
            }
            ActingProcessInner::Arbitrary(arb) => {
                write!(f, "arb({})", acting_vars.format_acting_var(&arb.var)).unwrap();
            }
            ActingProcessInner::Acquire(acq) => {
                write!(
                    f,
                    "{}: acq({},{})",
                    acting_vars.format_acting_var(&acq.s_acq),
                    acting_vars.format_acting_var(&acq.resource),
                    acting_vars.format_acting_var(&acq.quantity)
                )
                .unwrap();
            }
        }
        f
    }
}

pub enum ActingProcessInner {
    RootTask(RootProcess),
    Task(TaskProcess),
    Command(CommandProcess),
    AbstractModel(RefinementProcess),
    Method(RefinementProcess),
    Arbitrary(ArbitraryProcess),
    Acquire(AcquireProcess),
}

impl Display for ActingProcessInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActingProcessInner::RootTask(r) => write!(f, "{r}"),
            ActingProcessInner::Task(r) => write!(f, "{r}"),
            ActingProcessInner::Method(r) => write!(f, "{r}"),
            ActingProcessInner::Arbitrary(r) => write!(f, "{r}"),
            ActingProcessInner::Acquire(r) => write!(f, "{r}"),
            ActingProcessInner::AbstractModel(r) => {
                write!(f, "{r}")
            }
            ActingProcessInner::Command(c) => {
                write!(f, "{c}")
            }
        }
    }
}

impl ActingProcessInner {
    pub fn kind(&self) -> ActingProcessKind {
        match self {
            ActingProcessInner::RootTask(_) => ActingProcessKind::Root,
            ActingProcessInner::Task(_) => ActingProcessKind::Task,
            ActingProcessInner::Method(_) => ActingProcessKind::Method,
            ActingProcessInner::Arbitrary(_) => ActingProcessKind::Arbitrary,
            ActingProcessInner::Acquire(_) => ActingProcessKind::Acquire,
            ActingProcessInner::AbstractModel(_) => ActingProcessKind::AbstractModel,
            ActingProcessInner::Command(_) => ActingProcessKind::Command,
        }
    }

    pub fn get_abstract_model(&self) -> Option<ActingProcessId> {
        match self {
            ActingProcessInner::Task(t) => t.abstract_model,
            ActingProcessInner::Command(c) => c.abstract_model,
            _ => None,
        }
    }

    pub fn get_args(&self) -> Option<&Vec<ActingVarRef<cst::Cst>>> {
        match self {
            ActingProcessInner::Task(t) => Some(&t.args),
            ActingProcessInner::Command(c) => Some(&c.args),
            ActingProcessInner::Method(m) => Some(&m.args),
            _ => None,
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
    //
    // pub fn as_abstract_model(&self) -> Option<&RefinementProcess> {
    //     if let Self::AbstractModel(method) = self {
    //         Some(method)
    //     } else {
    //         None
    //     }
    // }
    //
    // pub fn as_mut_abstract_model(&mut self) -> Option<&mut RefinementProcess> {
    //     if let Self::AbstractModel(method) = self {
    //         Some(method)
    //     } else {
    //         None
    //     }
    // }

    pub fn as_refinement(&self) -> Option<&RefinementProcess> {
        match self {
            Self::AbstractModel(m) | Self::Method(m) => Some(m),
            _ => None,
        }
    }

    pub fn as_mut_refinement(&mut self) -> Option<&mut RefinementProcess> {
        match self {
            Self::AbstractModel(m) | Self::Method(m) => Some(m),
            _ => None,
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

    pub fn as_task(&self) -> Option<&TaskProcess> {
        if let Self::Task(action) = self {
            Some(action)
        } else {
            None
        }
    }

    pub fn as_mut_task(&mut self) -> Option<&mut TaskProcess> {
        if let Self::Task(action) = self {
            Some(action)
        } else {
            None
        }
    }

    pub fn as_command(&self) -> Option<&CommandProcess> {
        if let Self::Command(action) = self {
            Some(action)
        } else {
            None
        }
    }

    pub fn as_mut_command(&mut self) -> Option<&mut CommandProcess> {
        if let Self::Command(action) = self {
            Some(action)
        } else {
            None
        }
    }
}
