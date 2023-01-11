use crate::structs::chronicle::interval::Interval;
use crate::structs::flow_graph::assignment::Assignment;
use crate::structs::sym_table::VarId;

pub type FlowId = usize;

#[derive(Clone)]
pub enum FlowKind {
    Assignment(Assignment),
    Seq(Vec<FlowId>, FlowId),
    Branching(BranchingFlow),
    FlowResult(FlowResult),
    FlowAsync(FlowAsync),
    FlowWait(FlowWait),
}

impl From<FlowResult> for FlowKind {
    fn from(value: FlowResult) -> Self {
        Self::FlowResult(value)
    }
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

impl From<FlowAsync> for FlowKind {
    fn from(value: FlowAsync) -> Self {
        Self::FlowAsync(value)
    }
}

#[derive(Clone)]
pub struct Flow {
    pub valid: bool,
    pub parent: Option<FlowId>,
    pub kind: FlowKind,
}

impl<T> From<T> for Flow
where
    T: Into<FlowKind>,
{
    fn from(value: T) -> Self {
        Self {
            valid: true,
            parent: None,
            kind: value.into(),
        }
    }
}

#[derive(Clone)]
pub struct BranchingFlow {
    pub cond: FlowId,
    pub true_flow: FlowId,
    pub false_flow: FlowId,
    pub result: FlowId,
}

#[derive(Clone)]
pub struct FlowResult {
    pub result: VarId,
    pub timepoint: VarId,
}

#[derive(Clone)]
pub struct FlowAsync {
    pub result: VarId,
    pub timepoint: VarId,
    pub flow: FlowId,
}

#[derive(Clone)]
pub struct FlowWait {
    pub interval: Interval,
    pub duration: Option<VarId>,
}
