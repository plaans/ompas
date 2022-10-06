type NodeId = usize;

pub struct FlowGraph {}

pub struct Node {
    id: NodeId,
    parents: Vec<NodeId>,
    childs: Vec<NodeId>,
    computation: Computation,
}

pub enum Computation {
    Apply,
    Read,
    Cst,
    Write,
    If,
}
