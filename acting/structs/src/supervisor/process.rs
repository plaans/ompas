use crate::supervisor::inner::ProcessKind;
use crate::supervisor::process::acquire::AcquireProcess;
use crate::supervisor::process::arbitrary::ArbitraryProcess;
use crate::supervisor::process::command::CommandProcess;
use crate::supervisor::process::method::MethodProcess;
use crate::supervisor::process::root_task::RootProcess;
use crate::supervisor::process::task::TaskProcess;
use std::fmt::{Display, Formatter};

pub mod acquire;
pub mod arbitrary;
pub mod command;
pub mod method;
pub mod process_ref;
pub mod root_task;
pub mod task;

#[derive(Copy, Clone)]
pub enum ProcessOrigin {
    Execution,
    Planning,
}

pub struct ActingProcess {
    pub origin: ProcessOrigin,
    pub inner: ActingProcessInner,
}

impl ActingProcess {
    pub fn root() -> Self {
        Self {
            origin: ProcessOrigin::Execution,
            inner: ActingProcessInner::RootTask(RootProcess::new()),
        }
    }

    pub fn new(origin: ProcessOrigin, kind: impl Into<ActingProcessInner>) -> Self {
        Self {
            origin,
            inner: kind.into(),
        }
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
    Command(CommandProcess),
    Task(TaskProcess),
    Method(MethodProcess),
    Arbitrary(ArbitraryProcess),
    Acquire(AcquireProcess),
}

impl Display for ActingProcessInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActingProcessInner::RootTask(r) => write!(f, "{r}"),
            ActingProcessInner::Command(r) => write!(f, "{r}"),
            ActingProcessInner::Task(r) => write!(f, "{r}"),
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
            ActingProcessInner::Command(_) => ProcessKind::Command,
            ActingProcessInner::Task(_) => ProcessKind::Task,
            ActingProcessInner::Method(_) => ProcessKind::Method,
            ActingProcessInner::Arbitrary(_) => ProcessKind::Arbitrary,
            ActingProcessInner::Acquire(_) => ProcessKind::Acquire,
        }
    }

    pub fn as_mut_command(&mut self) -> Option<&mut CommandProcess> {
        if let Self::Command(command) = self {
            Some(command)
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

    pub fn as_root(&self) -> Option<&RootProcess> {
        if let Self::RootTask(root) = self {
            Some(root)
        } else {
            None
        }
    }

    pub fn as_method(&self) -> Option<&MethodProcess> {
        if let Self::Method(method) = self {
            Some(method)
        } else {
            None
        }
    }

    pub fn as_mut_method(&mut self) -> Option<&mut MethodProcess> {
        if let Self::Method(method) = self {
            Some(method)
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

    pub fn as_mut_resource(&mut self) -> Option<&AcquireProcess> {
        if let Self::Acquire(acquire) = self {
            Some(acquire)
        } else {
            None
        }
    }

    pub fn as_task(&self) -> Option<&TaskProcess> {
        if let Self::Task(task) = self {
            Some(task)
        } else {
            None
        }
    }

    pub fn as_mut_task(&mut self) -> Option<&mut TaskProcess> {
        if let Self::Task(task) = self {
            Some(task)
        } else {
            None
        }
    }
}
