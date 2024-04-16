use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::lit::Lit;
use crate::model::process_ref::Label;
use crate::model::sym_table::VarId;
use new_type::newtype;
use std::fmt::{Display, Formatter};

newtype!(FlowId: usize);

impl Display for FlowId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
//pub type FlowId = usize;

#[derive(Clone)]
pub struct Flow {
    pub valid: bool,
    pub interval: Interval,
    pub result: VarId,
    pub parent: Option<FlowId>,
    pub label: Option<Label>,
    pub kind: FlowKind,
}

#[derive(Clone)]
pub enum FlowKind {
    Lit(Lit),
    Seq(Vec<FlowId>),
    Branching(BranchingFlow),
    FlowHandle(FlowId),
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
