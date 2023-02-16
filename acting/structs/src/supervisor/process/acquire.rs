use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;

#[derive()]
pub struct AcquireProcess {
    id: ActingProcessId,
    parent: ActingProcessId,
    request_date: Option<Timepoint>,
    acquisition: Option<Interval>,
}

impl AcquireProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        request_date: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            request_date,
            acquisition: None,
        }
    }

    pub fn set_request_date(&mut self, request_date: Timepoint) {
        self.request_date = Some(request_date)
    }

    pub fn set_start_acquisition(&mut self, start: Timepoint) {
        self.acquisition = Some(Interval::new(start, None))
    }

    pub fn set_end_acquisition(&mut self, end: Timepoint) {
        if let Some(interval) = &mut self.acquisition {
            interval.end = Some(end)
        }
    }
}

impl From<AcquireProcess> for ActingProcessInner {
    fn from(value: AcquireProcess) -> Self {
        Self::Acquire(value)
    }
}
