use im::HashMap;
use std::fmt::Display;

pub type TypeId = usize;

#[derive(Default, Debug, Clone)]
pub struct TypeHierarchy {
    pub inner: HashMap<TypeId, String>,
    pub reverse: HashMap<String, TypeId>,
    pub parent: HashMap<TypeId, Option<TypeId>>,
    pub child: HashMap<TypeId, Vec<TypeId>>,
}

impl TypeHierarchy {
    pub fn get_types(&self) -> Vec<String> {
        self.inner.values().cloned().collect()
    }

    pub fn get_id(&self, t: impl Display) -> Option<&TypeId> {
        self.reverse.get(&t.to_string())
    }

    pub fn get_symbol(&self, id: &TypeId) -> Option<&String> {
        self.inner.get(id)
    }

    pub fn add_type(&mut self, t: String, parent: Option<String>) {
        if let Some(p) = &parent {
            let parent_id = match self.get_id(p) {
                None => {
                    let id = self.inner.len();
                    self.inner.insert(id, p.to_string());
                    self.reverse.insert(p.to_string(), id);
                    self.parent.insert(id, None);
                    self.child.insert(id, vec![]);
                    id
                }
                Some(id) => *id,
            };
            let child_id = self.inner.len();
            self.child.get_mut(&parent_id).unwrap().push(child_id);
            self.inner.insert(child_id, t.to_string());
            self.reverse.insert(t, child_id);
            self.parent.insert(child_id, Some(parent_id));
            self.child.insert(child_id, vec![]);
        } else {
            let id = self.inner.len();
            self.inner.insert(id, t.to_string());
            self.reverse.insert(t, id);
            self.child.insert(id, vec![]);
            self.parent.insert(id, None);
        }
    }

    pub fn get_direct_parent(&self, t: &str) -> Option<&String> {
        if let Some(id) = self.get_id(t) {
            match self.parent.get(id).unwrap() {
                None => None,
                Some(p) => self.get_symbol(p),
            }
        } else {
            None
        }
    }

    pub fn get_parents(&self, t: &str) -> Vec<String> {
        let mut parents = vec![];
        let mut parent = self.get_direct_parent(t);
        while let Some(p) = parent {
            parents.push(p.clone());
            parent = self.get_direct_parent(p)
        }
        parents
    }

    pub fn get_direct_childs(&self, t: &str) -> Option<Vec<String>> {
        if let Some(id) = self.get_id(t) {
            self.child.get(id).map(|p| {
                p.iter()
                    .map(|id| self.get_symbol(id).unwrap().clone())
                    .collect()
            })
        } else {
            None
        }
    }

    pub fn get_childs(&self, t: &str) -> Vec<String> {
        let mut all_childs = vec![];

        let childs = self.get_direct_childs(t);
        if let Some(mut childs) = childs {
            while let Some(child) = childs.pop() {
                all_childs.push(child.clone());
                let child_childs = self.get_direct_childs(&child);
                if let Some(mut new_childs) = child_childs {
                    childs.append(&mut new_childs)
                }
            }
        }
        all_childs
    }

    fn format_childs(&self, id: &TypeId, level: usize) -> String {
        let mut str = format!("{}- {}\n", "\t".repeat(level), self.get_symbol(id).unwrap());
        for c in self.child.get(id).unwrap() {
            str.push_str(self.format_childs(c, level + 1).as_str());
        }
        str
    }

    pub fn format_hierarchy(&self) -> String {
        let root_types: Vec<TypeId> = self
            .parent
            .iter()
            .filter(|(_, &v)| v.is_none())
            .map(|(k, _)| *k)
            .collect();
        let mut str = "Type Hierarchy\n".to_string();
        for r in &root_types {
            str.push_str(self.format_childs(r, 0).as_str());
            str.push('\n');
        }
        str
    }

    pub fn get_tuple_type_parent(&self) -> Vec<(&String, Option<&String>)> {
        let mut vec = vec![];
        for (t, p) in &self.parent {
            vec.push((
                self.get_symbol(t).unwrap(),
                p.map(|id| self.inner.get(&id).unwrap()),
            ));
        }
        vec
    }
}
