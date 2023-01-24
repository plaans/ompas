use crate::state::partial_state::PartialState;
use crate::state::world_state::StateType;
use crate::sym_table::domain::ref_type_lattice::RefTypeLattice;
use crate::sym_table::domain::type_lattice::TypeLattice;
use crate::sym_table::TYPE_OBJECT;
use ompas_rae_language::exec::state::INSTANCE;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct InstanceCollection {
    pub lattice: RefTypeLattice,
    pub inner: HashMap<String, HashSet<String>>,
}

impl Default for InstanceCollection {
    fn default() -> Self {
        Self {
            lattice: RefTypeLattice::new(),
            inner: Default::default(),
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
    pub async fn add_type(&mut self, t: &str, p: Option<&str>) {
        let parents = if let Some(t) = p {
            vec![self.lattice.get_type_id(t).await.unwrap()]
        } else {
            vec![self.lattice.get_type_id(TYPE_OBJECT).await.unwrap()]
        };
        self.lattice.add_type(t.to_string(), parents).await;
        self.inner.insert(t.to_string(), Default::default());
    }

    pub async fn add_instance(&mut self, i: &str, t: &str) {
        match self.inner.get_mut(t) {
            None => {
                let mut set = HashSet::new();
                set.insert(i.to_string());
                self.inner.insert(t.to_string(), set);
                self.lattice
                    .add_type(
                        t.to_string(),
                        vec![self.lattice.get_type_id(TYPE_OBJECT).await.unwrap()],
                    )
                    .await;
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

    pub async fn get_lattice(&self) -> TypeLattice {
        self.lattice.get_lattice().await
    }

    pub fn get_ref_lattice(&self) -> RefTypeLattice {
        self.lattice.clone()
    }
}
