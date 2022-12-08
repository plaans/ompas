use crate::domain::type_hierarchy::TypeHierarchy;
use crate::state::partial_state::PartialState;
use crate::state::world_state::StateType;
use ompas_rae_language::exec::state::INSTANCE;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default, Debug)]
pub struct InstanceCollection {
    pub type_hierarchy: TypeHierarchy,
    pub inner: HashMap<String, HashSet<String>>,
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
        self.type_hierarchy
            .add_type(t.to_string(), p.map(|s| s.to_string()));
        self.inner.insert(t.to_string(), Default::default());
    }

    pub fn add_instance(&mut self, i: &str, t: &str) {
        match self.inner.get_mut(t) {
            None => {
                let mut set = HashSet::new();
                set.insert(i.to_string());
                self.inner.insert(t.to_string(), set);
                self.type_hierarchy.add_type(t.to_string(), None);
            }
            Some(set) => {
                set.insert(i.to_string());
            }
        }
    }

    pub async fn is_of_type(&self, i: &str, t: &str) -> bool {
        match self.inner.get(t) {
            Some(set) => set.contains(i),
            None => false,
        }
    }

    pub async fn get_instances(&self, t: &str) -> Vec<String> {
        match self.inner.get(t) {
            Some(set) => set.iter().cloned().collect(),
            None => vec![],
        }
    }
}
