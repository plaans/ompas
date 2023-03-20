use crate::ompas::manager::state::world_state::StateType;
use im::HashMap;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::borrow::Borrow;

#[derive(Clone, Default, Debug)]
pub struct PartialState {
    pub inner: HashMap<LValueS, LValueS>,
    pub _type: Option<StateType>,
}

impl From<&HashMap<LValueS, LValueS>> for PartialState {
    fn from(m: &HashMap<LValueS, LValueS>) -> Self {
        Self {
            inner: m.clone(),
            _type: None,
        }
    }
}

impl From<HashMap<LValueS, LValueS>> for PartialState {
    fn from(m: HashMap<LValueS, LValueS>) -> Self {
        m.borrow().into()
    }
}

impl From<&PartialState> for HashMap<LValueS, LValueS> {
    fn from(ls: &PartialState) -> Self {
        ls.inner.clone()
    }
}
impl From<PartialState> for HashMap<LValueS, LValueS> {
    fn from(ls: PartialState) -> Self {
        ls.borrow().into()
    }
}

impl PartialState {
    pub fn get(&self, key: &LValueS) -> Option<&LValueS> {
        self.inner.get(key)
    }

    pub fn get_mut(&mut self, key: &LValueS) -> Option<&mut LValueS> {
        self.inner.get_mut(key)
    }

    pub fn set_type(&mut self, _type: StateType) {
        self._type = Some(_type)
    }

    pub fn get_type(&self) -> &Option<StateType> {
        &self._type
    }

    pub fn insert(&mut self, key: LValueS, value: LValueS) {
        self.inner.insert(key, value);
    }

    pub fn remove(&mut self, key: &LValueS) -> Option<LValueS> {
        self.inner.remove(key)
    }

    pub fn union(&self, other: &Self) -> Self {
        PartialState {
            inner: self.inner.clone().union(other.inner.clone()),
            _type: None,
        }
    }

    pub fn append(&mut self, other: &PartialState) {
        let _ = self.inner.clone().union(other.inner.clone());
    }

    pub fn into_map(self) -> LValue {
        let mut map: HashMap<LValue, LValue> = Default::default();
        for e in &self.inner {
            map.insert(e.0.into(), e.1.into());
        }
        map.into()
    }
}
