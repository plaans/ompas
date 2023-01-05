use crate::structs::flow_graph::flow::FlowId;
use crate::structs::sym_table::AtomId;
use im::HashMap;

#[derive(Default, Clone)]
pub struct HandleTable {
    //handle_bindings: HashMap<AtomId, Vec<AtomId>>,
    inner: HashMap<AtomId, Handle>,
}

impl HandleTable {
    pub fn insert(&mut self, key: &AtomId, handle: Handle) {
        self.inner.insert(*key, handle);
    }

    pub fn inner(&self) -> &HashMap<AtomId, Handle> {
        &self.inner
    }

    pub fn get(&self, id: &AtomId) -> Option<&Handle> {
        self.inner.get(id)
    }

    pub fn get_mut(&mut self, id: &AtomId) -> Option<&mut Handle> {
        self.inner.get_mut(id)
    }
}

#[derive(Clone)]
pub struct Handle {
    pub(crate) result: AtomId,
    pub(crate) flow: FlowId,
    pub(crate) ends: Vec<AtomId>,
}

impl Handle {
    pub fn add_end(&mut self, end: &AtomId) {
        self.ends.push(*end);
    }
}
