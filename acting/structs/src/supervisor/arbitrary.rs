use crate::supervisor::interval::Timepoint;
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;

pub struct ArbitraryProcess {
    id: ActingProcessId,
    parent: ActingProcessId,
    suggested: Option<LValue>,
    chosen: Option<LValue>,
    timepoint: Option<Timepoint>,
}

impl ArbitraryProcess {
    pub fn new(
        id: ActingProcessId,
        parent: ActingProcessId,
        suggested: Option<LValue>,
        chosen: Option<LValue>,
        timepoint: Option<Timepoint>,
    ) -> Self {
        Self {
            id,
            parent,
            suggested,
            chosen,
            timepoint,
        }
    }
}

impl From<ArbitraryProcess> for ActingProcessInner {
    fn from(value: ArbitraryProcess) -> Self {
        Self::Arbitrary(value)
    }
}
