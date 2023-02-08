use crate::conversion::chronicle::interval::Interval;
use crate::sym_table::lit::Lit;
use crate::sym_table::VarId;

pub type FlowId = usize;

#[derive(Clone)]
pub struct Flow {
    pub valid: bool,
    pub interval: Interval,
    pub result: VarId,
    pub parent: Option<FlowId>,
    pub kind: FlowKind,
}

#[derive(Clone)]
pub enum FlowKind {
    Lit(Lit),
    Seq(Vec<FlowId>),
    Branching(BranchingFlow),
    FlowHandle(FlowId),
    FlowResourceHandle(FlowId),
    FlowPause(FlowPause),
}

impl From<Lit> for FlowKind {
    fn from(value: Lit) -> Self {
        Self::Lit(value)
    }
}

impl From<BranchingFlow> for FlowKind {
    fn from(value: BranchingFlow) -> Self {
        Self::Branching(value)
    }
}

#[derive(Clone)]
pub struct BranchingFlow {
    pub cond_flow: FlowId,
    pub true_flow: FlowId,
    pub false_flow: FlowId,
}

#[derive(Clone)]
pub struct FlowPause {
    pub duration: Option<VarId>,
}
