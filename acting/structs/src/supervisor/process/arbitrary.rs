use crate::supervisor::interval::Timepoint;
use crate::supervisor::process::ActingProcessInner;
use crate::supervisor::ActingProcessId;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub struct ArbitraryProcess {
    pub id: ActingProcessId,
    _parent: ActingProcessId,
    traces: Vec<ArbitraryTrace>,
}

#[derive(Clone)]
pub struct ArbitraryTrace {
    pub possibilities: Vec<LValue>,
    pub choice: ArbitraryChoice,
    pub instant: Timepoint,
}

#[derive(Clone)]
pub enum ArbitraryChoice {
    Planning(LValue),
    Execution(LValue),
}

impl ArbitraryChoice {
    pub fn inner(&self) -> &LValue {
        match self {
            Self::Planning(lv) | Self::Execution(lv) => lv,
        }
    }

    pub fn is_planned(&self) -> bool {
        matches!(self, Self::Planning(..))
    }
}

impl Display for ArbitraryChoice {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ArbitraryChoice::Planning(lv) => write!(f, "planning({lv})"),
            ArbitraryChoice::Execution(lv) => write!(f, "execution({lv})"),
        }
    }
}

impl Display for ArbitraryTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\narb{}\n => {}",
            self.instant,
            LValue::from(&self.possibilities),
            self.choice
        )
    }
}

impl ArbitraryProcess {
    pub fn new(id: ActingProcessId, _parent: ActingProcessId, trace: ArbitraryTrace) -> Self {
        Self {
            id,
            _parent,
            traces: vec![trace],
        }
    }

    pub fn add_trace(&mut self, trace: ArbitraryTrace) {
        self.traces.push(trace)
    }

    pub fn get_last_trace(&self) -> &ArbitraryTrace {
        self.traces.last().unwrap()
    }
}

impl From<ArbitraryProcess> for ActingProcessInner {
    fn from(value: ArbitraryProcess) -> Self {
        Self::Arbitrary(value)
    }
}

impl Display for ArbitraryProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}){}", self.id, self.traces.last().unwrap())
    }
}
