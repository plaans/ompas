use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::state::StateType;
use im::HashMap;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::borrow::Borrow;

#[derive(Clone, Debug)]
pub struct Fact {
    pub value: LValueS,
    pub date: Option<Timepoint>,
}

impl Fact {
    pub fn new(value: LValueS, date: Option<Timepoint>) -> Self {
        Self { value, date }
    }
}

impl From<&Fact> for LValueS {
    fn from(value: &Fact) -> Self {
        value.value.clone()
    }
}

impl From<&LValueS> for Fact {
    fn from(value: &LValueS) -> Self {
        Self {
            value: value.clone(),
            date: None,
        }
    }
}

impl From<LValueS> for Fact {
    fn from(value: LValueS) -> Self {
        Self { value, date: None }
    }
}

#[derive(Clone, Default, Debug)]
pub struct PartialState {
    pub inner: HashMap<LValueS, Fact>,
    pub _type: Option<StateType>,
}

impl From<&HashMap<LValueS, LValueS>> for PartialState {
    fn from(m: &HashMap<LValueS, LValueS>) -> Self {
        Self {
            inner: m.iter().map(|(k, v)| (k.clone(), v.into())).collect(),
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
        ls.inner
            .iter()
            .map(|(k, v)| (k.clone(), v.into()))
            .collect()
    }
}
impl From<PartialState> for HashMap<LValueS, LValueS> {
    fn from(ls: PartialState) -> Self {
        ls.borrow().into()
    }
}

impl PartialState {
    pub fn get(&self, key: &LValueS) -> Option<&Fact> {
        self.inner.get(key)
    }

    pub fn get_mut(&mut self, key: &LValueS) -> Option<&mut Fact> {
        self.inner.get_mut(key)
    }

    pub fn set_type(&mut self, _type: StateType) {
        self._type = Some(_type)
    }

    pub fn get_type(&self) -> &Option<StateType> {
        &self._type
    }

    pub fn insert(&mut self, key: LValueS, value: Fact) {
        self.inner.insert(key, value);
    }

    pub fn remove(&mut self, key: &LValueS) -> Option<Fact> {
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
        for (k, f) in &self.inner {
            map.insert(k.into(), f.value.clone().into());
        }
        map.into()
    }
}
