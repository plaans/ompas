use crate::supervisor::acquire::AcquireProcess;
use crate::supervisor::arbitrary::ArbitraryProcess;
use crate::supervisor::command::CommandProcess;
use crate::supervisor::method::MethodProcess;
use crate::supervisor::root_task::RootProcess;
use crate::supervisor::task::TaskProcess;

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

impl ActingProcessInner {
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
