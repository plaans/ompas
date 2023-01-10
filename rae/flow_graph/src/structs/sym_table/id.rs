use crate::structs::sym_table::VarId;
use std::collections::HashMap;

pub type Version = usize;

#[derive(Clone, Default)]
pub struct SymbolTableId {
    inner: HashMap<String, Id>,
}

impl SymbolTableId {
    pub fn insert(&mut self, symbol: &str, id: &VarId) -> Version {
        match self.inner.get_mut(symbol) {
            None => {
                self.inner.insert(symbol.to_string(), Id::unique(id));
                0
            }
            Some(s) => match s {
                Id::Unique(o_id) => {
                    *s = Id::Several(vec![*o_id, *id]);
                    1
                }
                Id::Several(ids) => {
                    ids.push(*id);
                    ids.len() - 1
                }
            },
        }
    }

    pub fn version(&mut self, symbol: &str) -> Version {
        match self.inner.get(symbol) {
            None => 0,
            Some(id) => id.n_version(),
        }
    }

    pub fn get_id(&self, symbol: &str) -> Option<VarId> {
        self.inner.get(symbol).map(|id| *id.get_id())
    }

    pub fn contains(&self, symbol: &str) -> bool {
        self.inner.contains_key(symbol)
    }
}

#[derive(Clone)]
pub enum Id {
    Unique(VarId),
    Several(Vec<VarId>),
}

impl Id {
    pub fn unique(id: &VarId) -> Self {
        Self::Unique(*id)
    }

    pub fn several(id: &VarId) -> Self {
        Self::Several(vec![*id])
    }

    pub fn get_id(&self) -> &VarId {
        match self {
            Id::Unique(id) => id,
            Id::Several(vec) => vec.last().unwrap(),
        }
    }

    pub fn n_version(&self) -> Version {
        match self {
            Id::Unique(_) => 1,
            Id::Several(vec) => vec.len(),
        }
    }
}
