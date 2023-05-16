use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::state::partial_state::PartialState;
use crate::ompas::manager::state::world_state::StateType;
use ompas_language::exec::state::INSTANCE;
use ompas_language::sym_table::TYPE_OBJECT;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};

#[derive(Default, Clone, Debug)]
pub struct InstanceCollectionSnapshot {
    pub inner: HashMap<String, HashSet<String>>,
}

impl From<InstanceCollectionSnapshot> for PartialState {
    fn from(i: InstanceCollectionSnapshot) -> Self {
        let mut p = PartialState {
            inner: Default::default(),
            _type: Some(StateType::Instance),
        };
        for (t, set) in i.inner {
            let v: LValueS = set.iter().cloned().collect::<Vec<_>>().into();
            p.insert(list![INSTANCE.into(), t.into()].try_into().unwrap(), v);
        }
        p
    }
}

impl InstanceCollectionSnapshot {
    pub fn is_of_type(&self, i: &str, t: &str) -> bool {
        match self.inner.get(t) {
            Some(set) => set.contains(i),
            None => false,
        }
    }

    pub fn get_instances(&self, t: &str) -> Vec<String> {
        match self.inner.get(t) {
            Some(set) => set.iter().cloned().collect(),
            None => vec![],
        }
    }
}

#[derive(Clone, Default)]
pub struct InstanceCollection {
    pub st: RefSymTable,
    pub inner: HashMap<String, HashSet<String>>,
}

impl From<InstanceCollectionSnapshot> for InstanceCollection {
    fn from(value: InstanceCollectionSnapshot) -> Self {
        Self {
            st: RefSymTable::default(),
            inner: value.inner,
        }
    }
}

impl InstanceCollection {
    pub fn new(st: RefSymTable) -> Self {
        Self {
            st,
            inner: Default::default(),
        }
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }

    pub fn get_snapshot(&self) -> InstanceCollectionSnapshot {
        InstanceCollectionSnapshot {
            inner: self.inner.clone(),
        }
    }
}

impl From<InstanceCollection> for PartialState {
    fn from(i: InstanceCollection) -> Self {
        let mut p = PartialState {
            inner: Default::default(),
            _type: Some(StateType::Instance),
        };
        for (t, set) in i.inner {
            let v: LValueS = set.iter().cloned().collect::<Vec<_>>().into();
            p.insert(list![INSTANCE.into(), t.into()].try_into().unwrap(), v);
        }
        p
    }
}

impl InstanceCollection {
    pub fn add_type(&mut self, t: &str, p: Option<&str>) {
        let parents = if let Some(parent) = p {
            let parent_id = match self.st.get_type_id(parent) {
                Some(id) => id,
                None => self
                    .st
                    .add_type(parent, vec![self.st.get_type_id(TYPE_OBJECT).unwrap()]),
            };
            vec![parent_id]
        } else {
            vec![self.st.get_type_id(TYPE_OBJECT).unwrap()]
        };
        self.st.add_type(t.to_string(), parents);
        self.inner.insert(t.to_string(), Default::default());
    }

    pub fn add_instance(&mut self, i: &str, t: &str) {
        match self.inner.get_mut(t) {
            None => {
                let mut set = HashSet::new();
                set.insert(i.to_string());
                self.inner.insert(t.to_string(), set);
                self.st.add_type(
                    t.to_string(),
                    vec![self.st.get_type_id(TYPE_OBJECT).unwrap()],
                );
            }
            Some(set) => {
                set.insert(i.to_string());
            }
        }
        let domain = self.st.get_type_as_domain(t).unwrap();
        self.st.new_constant_symbol(i, domain);
    }

    pub fn is_of_type(&self, i: &str, t: &str) -> bool {
        match self.inner.get(t) {
            Some(set) => set.contains(i),
            None => false,
        }
    }

    pub fn get_instances(&self, t: &str) -> Vec<String> {
        match self.inner.get(t) {
            Some(set) => set.iter().cloned().collect(),
            None => vec![],
        }
    }
}
