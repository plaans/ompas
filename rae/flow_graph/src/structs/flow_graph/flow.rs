use crate::structs::flow_graph::graph::VerticeId;

pub type FlowId = usize;

#[derive(Clone)]
pub enum Flow {
    Vertice(VerticeId),
    Seq(Vec<FlowId>),
    Async(FlowId),
    If(IfFlow),
}

impl From<VerticeId> for Flow {
    fn from(value: VerticeId) -> Self {
        Self::Vertice(value)
    }
}

#[derive(Clone)]
pub struct IfFlow {
    cond_flow: FlowId,
    flow_true: FlowId,
    flow_false: FlowId,
}
