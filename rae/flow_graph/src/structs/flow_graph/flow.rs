use crate::structs::flow_graph::graph::VerticeId;

pub type FlowId = usize;

pub enum Flow {
    Vertice(VerticeId),
    Seq(Vec<FlowId>),
    Async(FlowId),
    If(IfFlow),
}

pub struct IfFlow {
    cond_flow: FlowId,
    flow_true: FlowId,
    flow_false: FlowId,
}
