use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::AtomId;
use crate::structs::flow_graph::graph::VerticeId;
use crate::Expression;
use core::option::Option;
use core::option::Option::Some;

#[derive(Clone, Debug)]
pub struct Vertice {
    pub id: VerticeId,
    pub interval: Interval,
    pub result: AtomId,
    pub parent: Option<VerticeId>,
    pub child: Option<VerticeId>,
    pub computation: Expression,
}

impl Vertice {
    /*
    SETTERS
     */
    pub fn set_parent(&mut self, parent: &VerticeId) {
        self.parent = Some(*parent)
    }

    pub fn set_child(&mut self, child: &VerticeId) {
        self.child = Some(*child)
    }

    /*GETTERS*/
    pub fn get_parent(&self) -> &Option<VerticeId> {
        &self.parent
    }

    pub fn get_child(&self) -> &Option<VerticeId> {
        &self.child
    }

    pub fn get_computation(&self) -> &Expression {
        &self.computation
    }

    pub fn get_id(&self) -> &VerticeId {
        &self.id
    }

    pub fn get_result(&self) -> &AtomId {
        &self.result
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_start(&self) -> &AtomId {
        self.interval.get_start()
    }

    pub fn get_end(&self) -> &AtomId {
        self.interval.get_end()
    }
}
