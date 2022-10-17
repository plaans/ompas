/*use crate::structs::flow_graph::graph::VerticeId;
use core::default::Default;

#[derive(Copy, Clone)]
pub struct Edge {
    from: VerticeId,
    to: VerticeId,
    kind: EdgeKind,
}

impl Edge {
    pub fn new(from: VerticeId, to: VerticeId, kind: EdgeKind) -> Self {
        Self { from, to, kind }
    }

    pub fn from(&self) -> &VerticeId {
        &self.from
    }

    pub fn to(&self) -> &VerticeId {
        &self.to
    }
}

#[derive(Debug, Copy, Clone)]
pub enum EdgeKind {
    Seq,
    Branching(bool),
}

impl Default for EdgeKind {
    fn default() -> Self {
        Self::Seq
    }
}*/
