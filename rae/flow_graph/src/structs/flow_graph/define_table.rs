use crate::NodeId;

#[derive(Default, Clone)]
pub struct DefineTable {
    inner: im::HashMap<String, NodeId>,
}

impl DefineTable {
    pub fn insert(&mut self, var: String, node_id: NodeId) {
        self.inner.insert(var, node_id);
    }

    pub fn get(&self, var: &str) -> Option<&NodeId> {
        self.inner.get(var)
    }
}
