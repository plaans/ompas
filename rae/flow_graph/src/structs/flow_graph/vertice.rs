use crate::structs::chronicle::interval::Interval;
use crate::structs::flow_graph::graph::VerticeId;
use crate::structs::sym_table::AtomId;
use crate::Expression;
#[derive(Clone, Debug)]
pub struct Vertice {
    pub id: VerticeId,
    pub interval: Interval,
    pub result: AtomId,
    pub computation: Expression,
}

impl Vertice {
    pub fn get_computation(&self) -> &Expression {
        &self.computation
    }

    pub fn get_vertice_id(&self) -> VerticeId {
        self.id
    }

    pub fn get_result(&self) -> AtomId {
        self.result
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_start(&self) -> AtomId {
        *self.interval.get_start()
    }

    pub fn get_end(&self) -> AtomId {
        *self.interval.get_end()
    }
}
