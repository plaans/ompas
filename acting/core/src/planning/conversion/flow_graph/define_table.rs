use crate::model::sym_table::VarId;

#[derive(Default, Clone)]
pub struct DefineTable {
    inner: im::HashMap<String, VarId>,
}

impl DefineTable {
    pub fn insert(&mut self, var: String, atom_id: VarId) {
        self.inner.insert(var, atom_id);
    }

    pub fn get(&self, var: &str) -> Option<&VarId> {
        self.inner.get(var)
    }

    pub fn inner(&self) -> &im::HashMap<String, VarId> {
        &self.inner
    }
}
