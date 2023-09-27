use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::state::partial_state::{Fact, PartialState};
use crate::ompas::manager::state::StateType;
use ompas_language::exec::state::{INSTANCE, INSTANCES};
use ompas_language::sym_table::TYPE_OBJECT;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default)]
pub struct InstanceCollection {
    pub st: RefSymTable,
    pub inner: HashMap<String, HashSet<String>>,
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

    pub fn get_snapshot(&self) -> Self {
        self.clone()
        /*InstanceCollectionSnapshot {
            st: self.st.clone(),
            inner: self.inner.clone(),
        }*/
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
            p.insert(
                LValueS::List(vec![INSTANCES.into(), t.to_string().into()]),
                (&v).into(),
            );

            for o in set {
                p.insert(
                    LValueS::List(vec![INSTANCE.into(), o.into()]),
                    Fact::new(t.to_string().into(), None),
                );
            }
        }
        p
    }
}

impl InstanceCollection {
    pub fn add_type(&mut self, t: &str, p: Option<&str>) {
        let parents = if let Some(parent) = p {
            let parent_id = match self.st.get_type_id(parent) {
                Some(id) => id,
                None => {
                    self.add_type(parent, None);
                    self.st.get_type_id(parent).unwrap()
                }
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
        let lattice = self.st.get_lattice();
        let type_id = if let Some(id) = lattice.get_type_id(t) {
            id
        } else {
            return false;
        };

        let types = lattice.get_all_childs(type_id);
        for t in types {
            let name = lattice.format_type(&t);
            //println!("{name}?");
            if let Some(set) = self.inner.get(&name) {
                if set.contains(i) {
                    //println!("yup!");
                    return true;
                }
            }
        }
        false
    }

    pub fn get_instances(&self, t: &str) -> Vec<String> {
        let lattice = self.st.get_lattice();
        let type_id = if let Some(id) = lattice.get_type_id(t) {
            id
        } else {
            return vec![];
        };
        let types = lattice.get_all_childs(type_id);
        let mut instances: HashSet<String> = Default::default();

        for id in types {
            let name = lattice.format_type(&id);
            if let Some(set) = self.inner.get(&name) {
                for s in set {
                    instances.insert(s.clone());
                }
            }
        }
        instances.iter().cloned().collect()
    }
}
