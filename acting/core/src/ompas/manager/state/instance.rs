use crate::model::sym_domain::type_lattice::TypeLattice;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::state::partial_state::{Fact, PartialState};
use crate::ompas::manager::state::StateType;
use aries::collections::seq::Seq;
use ompas_language::exec::state::{INSTANCE, INSTANCES};
use ompas_language::sym_table::TYPE_OBJECT;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default)]
pub struct InstanceCollection {
    pub st: RefSymTable,
    pub inner: HashMap<String, InstanceSet>,
    lattice: Option<TypeLattice>,
}

#[derive(Clone)]
pub struct InstanceSet {
    unk_element: String,
    pub elements: HashSet<String>,
}

impl InstanceSet {
    pub fn new(unk_element: String) -> Self {
        Self {
            unk_element,
            elements: Default::default(),
        }
    }

    pub fn add_element(&mut self, e: impl ToString) {
        self.elements.insert(e.to_string());
    }

    pub fn remove_element(&mut self, e: &str) {
        self.elements.remove(e);
    }

    pub fn get_instance(&self) -> Vec<String> {
        self.elements.clone().to_vec()
    }

    pub fn get_all_elements(&self) -> Vec<String> {
        let mut vec = self.elements.clone().to_vec();
        vec.push(self.unk_element.to_string());
        vec
    }

    pub fn contains(&self, e: &str) -> bool {
        self.elements.contains(e)
    }
}

impl InstanceCollection {
    pub fn new(st: RefSymTable) -> Self {
        let mut inner: HashMap<_, _> = Default::default();
        inner.insert(
            TYPE_OBJECT.to_string(),
            InstanceSet::new(Self::format_unk_type(TYPE_OBJECT)),
        );

        Self {
            st,
            inner,
            lattice: None,
        }
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }

    pub fn get_snapshot(&self) -> Self {
        self.clone()
    }
}

impl From<InstanceCollection> for PartialState {
    fn from(i: InstanceCollection) -> Self {
        let mut p = PartialState {
            inner: Default::default(),
            _type: Some(StateType::Instance),
        };
        for (t, set) in i.inner {
            let v: LValueS = set.elements.iter().cloned().collect::<Vec<_>>().into();
            p.insert(
                LValueS::List(vec![INSTANCES.into(), t.to_string().into()]),
                (&v).into(),
            );

            for o in set.elements {
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
    fn format_unk_type(t: &str) -> String {
        format!("unk_{}", t)
    }
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
        let set = InstanceSet::new(Self::format_unk_type(t));
        self.inner.insert(t.to_string(), set);
    }

    pub fn get_unk_of_type(&self, t: &str) -> String {
        match self.inner.get(t) {
            None => "unk".to_string(),
            Some(t) => t.unk_element.to_string(),
        }
    }

    pub fn add_instance(&mut self, i: &str, t: &str) {
        match self.inner.get_mut(t) {
            None => {
                self.add_type(t, None);
                self.inner.get_mut(t).unwrap().add_element(i);
            }
            Some(set) => {
                set.add_element(i.to_string());
            }
        }
        let domain = self.st.get_type_as_domain(t).unwrap();
        self.st.new_constant_symbol(i, domain);
    }

    pub fn remove_instance(&mut self, i: &str) {
        for (_, set) in &mut self.inner {
            set.remove_element(i)
        }
    }

    pub fn is_of_type(&mut self, i: &str, t: &str) -> bool {
        //let lattice = self.st.0.read().unwrap().get_lattice();
        let lattice = if let Some(lattice) = &self.lattice {
            lattice
        } else {
            self.lattice = Some(self.st.get_lattice());
            self.lattice.as_ref().unwrap()
        };
        //println!("t(tl.clone) = {} µs", time.elapsed().unwrap().as_micros());
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
                    //println!("t(instance) = {} µs", time.elapsed().unwrap().as_micros());
                    return true;
                }
            }
        }

        false
    }

    pub fn get_instances(&mut self, t: &str) -> Vec<String> {
        let lattice = if let Some(lattice) = &self.lattice {
            lattice
        } else {
            self.lattice = Some(self.st.get_lattice());
            self.lattice.as_ref().unwrap()
        };
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
                for s in &set.elements {
                    instances.insert(s.clone());
                }
            }
        }
        instances.iter().cloned().collect()
    }
}
