use crate::structs::chronicle::AtomId;

#[derive(Default, Clone)]
pub struct DefineTable {
    inner: im::HashMap<String, AtomId>,
}

impl DefineTable {
    pub fn insert(&mut self, var: String, atom_id: AtomId) {
        self.inner.insert(var, atom_id);
    }

    pub fn get(&self, var: &str) -> Option<&AtomId> {
        self.inner.get(var)
    }
}
