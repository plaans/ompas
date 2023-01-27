use crate::conversion::chronicle::interval::Interval;
use crate::conversion::flow_graph::assignment::Assignment;
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
    Assignment(Assignment),
    Seq(Vec<FlowId>),
    Branching(BranchingFlow),
    FlowHandle(FlowId),
    FlowResourceHandle(FlowId),
    FlowPause(FlowPause),
}

impl From<Assignment> for FlowKind {
    fn from(value: Assignment) -> Self {
        Self::Assignment(value)
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
