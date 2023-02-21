use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use std::fmt::{Display, Formatter};

#[derive()]
pub struct AcquireProcess {
    id: ActingProcessId,
    _parent: ActingProcessId,
    request_date: Option<Timepoint>,
    acquisition: Option<Interval>,
}

impl AcquireProcess {
    pub fn new(
        id: ActingProcessId,
        _parent: ActingProcessId,
        request_date: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            _parent,
            request_date,
            acquisition: None,
        }
    }

    pub fn set_request(&mut self, request_date: Timepoint) {
        self.request_date = Some(request_date)
    }

    pub fn set_acquisition_start(&mut self, start: Timepoint) {
        self.acquisition = Some(Interval::new(start, None::<Timepoint>))
    }

    pub fn set_acquisition_end(&mut self, end: Timepoint) {
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

impl Display for AcquireProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let request = match &self.request_date {
            None => "..".to_string(),
            Some(timepoint) => timepoint.as_secs().to_string(),
        };

        let acquisition = match &self.acquisition {
            None => "[..,..]".to_string(),
            Some(interval) => interval.to_string(),
        };
        write!(f, "({}){} => {}", self.id, request, acquisition)
    }
}
