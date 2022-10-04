use crate::domain::type_hierarchy::TypeHierarchy;
use crate::state::partial_state::PartialState;
use crate::state::world_state::StateType;
use ompas_rae_language::RAE_INSTANCE;
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
            p.insert(list![RAE_INSTANCE.into(), t.into()].try_into().unwrap(), v);
        }
        p
    }
}

impl InstanceCollection {
    pub fn add_type(&mut self, t: String, p: Option<String>) {
        self.type_hierarchy.add_type(t.clone(), p);
        self.inner.insert(t, Default::default());
    }

    pub fn add_instance(&mut self, i: String, t: String) {
        match self.inner.get_mut(&t) {
            None => {
                let mut set = HashSet::new();
                set.insert(i);
                self.inner.insert(t.clone(), set);
                self.type_hierarchy.add_type(t, None);
            }
            Some(set) => {
                set.insert(i);
            }
        }
    }

    pub async fn is_of_type(&self, i: String, t: String) -> bool {
        match self.inner.get(&t) {
            Some(set) => set.contains(&i),
            None => false,
        }
    }

    pub async fn get_instances(&self, t: String) -> Vec<String> {
        match self.inner.get(&t) {
            Some(set) => set.iter().cloned().collect(),
            None => vec![],
        }
    }
}
