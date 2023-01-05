use crate::structs::chronicle::interval::Interval;
use crate::structs::flow_graph::graph::VerticeId;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::AtomId;

#[derive(Clone, Debug)]
pub struct Vertice {
    pub id: VerticeId,
    pub interval: Interval,
    pub result: AtomId,
    pub computation: Lit,
}

impl Vertice {
    pub fn get_computation(&self) -> &Lit {
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
