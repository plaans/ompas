use crate::structs::chronicle::interval::Interval;
use crate::structs::flow_graph::graph::VerticeId;
use crate::structs::sym_table::AtomId;

pub type FlowId = usize;

#[derive(Clone)]
pub enum FlowKind {
    Vertice(VerticeId),
    Seq(Vec<FlowId>),
    //Result(Interval, AtomId),
    Branching(BranchingFlow),
}

impl From<VerticeId> for FlowKind {
    fn from(value: VerticeId) -> Self {
        Self::Vertice(value)
    }
}
impl From<Vec<FlowId>> for FlowKind {
    fn from(value: Vec<FlowId>) -> Self {
        Self::Seq(value)
    }
}

impl From<BranchingFlow> for FlowKind {
    fn from(value: BranchingFlow) -> Self {
        Self::Branching(value)
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
    pub cond_flow: FlowId,
    pub true_flow: FlowId,
    pub false_flow: FlowId,
    pub result: FlowId,
}
