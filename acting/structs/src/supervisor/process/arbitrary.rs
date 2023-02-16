use crate::supervisor::interval::{Interval, Timepoint};
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub struct ArbitraryProcess {
    id: ActingProcessId,
    _parent: ActingProcessId,
    suggested: Option<LValue>,
    chosen: Option<LValue>,
    interval: Option<Interval>,
}

impl Display for ArbitraryProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let timepoint = match &self.interval {
            None => "".to_string(),
            Some(timepoint) => timepoint.to_string(),
        };

        let chosen = match &self.chosen {
            None => "".to_string(),
            Some(lv) => lv.to_string(),
        };

        let suggested = match &self.suggested {
            None => "".to_string(),
            Some(lv) => format!("({lv})"),
        };

        write!(f, "({}){}{}{}", self.id, timepoint, chosen, suggested)
    }
}

impl ArbitraryProcess {
    pub fn new(
        id: ActingProcessId,
        _parent: ActingProcessId,
        suggested: Option<LValue>,
        chosen: Option<LValue>,
        timepoint: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            _parent,
            suggested,
            chosen,
            interval: timepoint.map(|t| Interval::new_instant(t)),
        }
    }
}

impl From<ArbitraryProcess> for ActingProcessInner {
    fn from(value: ArbitraryProcess) -> Self {
        Self::Arbitrary(value)
    }
}
