use crate::planning::conversion::flow_graph::graph::VerticeId;
use core::convert::From;

#[derive(Copy, Clone, Debug, Default)]
pub struct Scope {
    pub start: VerticeId,
    pub end: VerticeId,
}

impl Scope {
    pub fn start(&self) -> &VerticeId {
        &self.start
    }

    pub fn get_end(&self) -> &VerticeId {
        &self.end
    }

    pub fn set_start(&mut self, start: VerticeId) {
        self.start = start;
    }

    pub fn set_end(&mut self, end: VerticeId) {
        self.end = end;
    }

    pub fn singleton(id: VerticeId) -> Self {
        Self { start: id, end: id }
    }

    pub fn expression(start: VerticeId, end: VerticeId) -> Self {
        Self { start, end }
    }
}

impl From<VerticeId> for Scope {
    fn from(id: VerticeId) -> Self {
        Self::singleton(id)
    }
}
