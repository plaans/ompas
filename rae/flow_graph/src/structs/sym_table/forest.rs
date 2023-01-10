use std::clone::Clone;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Index, IndexMut};

pub type NodeId = usize;

pub type Rank = usize;

#[derive(Clone, Default, Debug)]
pub struct Node<T: Display + Clone + Default> {
    value: T,
    parent: NodeId,
    rank: Rank,
}

impl<T: Display + Clone + Default> Display for Node<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<T: Display + Clone + Default> Node<T> {
    pub fn get_value(&self) -> &T {
        &self.value
    }
    pub fn set_value(&mut self, value: T) {
        self.value = value;
    }

    pub fn get_parent(&self) -> &NodeId {
        &self.parent
    }
    pub fn set_parent(&mut self, parent: &NodeId) {
        self.parent = *parent;
    }
    pub fn get_rank(&self) -> Rank {
        self.rank
    }
    pub fn set_rank(&mut self, rank: Rank) {
        self.rank = rank;
    }
}

impl<T: Display + Clone + Default> Node<T> {
    pub fn new(value: T, parent: NodeId) -> Self {
        Self {
            value,
            parent,
            rank: Default::default(),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Forest<T: Display + Default + Clone> {
    inner: Vec<Node<T>>,
}

impl<T> Index<NodeId> for Forest<T>
where
    T: Display + Default + Clone + Default,
{
    type Output = T;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.inner[index].value
    }
}

impl<T> IndexMut<NodeId> for Forest<T>
where
    T: Display + Default + Clone + Default,
{
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.inner[index].value
    }
}

impl<T: Display + Default + Clone> Forest<T> {
    pub fn flat_bindings(&mut self) {
        for i in 0..self.inner.len() {
            self.find(&i);
        }
    }

    pub fn get_node(&self, id: &NodeId) -> Option<&Node<T>> {
        self.inner.get(*id)
    }

    pub fn get_value(&self, id: &NodeId) -> Option<&T> {
        match self.inner.get(*id) {
            Some(node) => Some(&node.value),
            None => None,
        }
    }

    pub fn set_value(&mut self, id: &NodeId, value: T) {
        if let Some(node) = self.inner.get_mut(*id) {
            node.value = value;
        }
    }

    pub fn new_node(&mut self, value: T) -> NodeId {
        let id = self.inner.len();
        let node = Node::new(value, id);
        self.inner.push(node);
        id
    }

    pub fn get_parent(&self, id: &NodeId) -> &NodeId {
        self.inner.get(*id).unwrap().get_parent()
    }

    pub fn set_parent(&mut self, id: &NodeId, parent: &NodeId) {
        self.inner.get_mut(*id).expect("").set_parent(parent);
    }

    pub fn get_rank(&self, id: &NodeId) -> Rank {
        self.inner.get(*id).expect("strong error").get_rank()
    }

    pub fn set_rank(&mut self, id: &NodeId, rank: Rank) {
        self.inner.get_mut(*id).expect("").set_rank(rank);
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl<T: Debug + Display + Default + Clone> Display for Forest<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();

        let mut groups: HashMap<NodeId, VecDeque<(NodeId, Node<T>)>> = Default::default();

        for (node_id, node) in self.inner.iter().enumerate() {
            let parent_id = node.get_parent();
            if *parent_id != node_id {
                if let Some(vec) = groups.get_mut(parent_id) {
                    vec.push_back((node_id, node.clone()));
                } else {
                    groups.insert(*parent_id, {
                        let mut vec = VecDeque::new();
                        vec.push_front((node_id, node.clone()));
                        vec
                    });
                }
            } else {
                groups.insert(*parent_id, VecDeque::new());
            }
        }

        //println!("[debug] groups: {:?}\n", groups);

        for (root_id, tree) in groups {
            let mut format = "{".to_string();
            for (i, (id, element)) in tree.iter().enumerate() {
                if i != 0 {
                    format.push(',');
                }
                format.push_str(format!("{}({})", element, id).as_str())
            }

            format.push('}');

            string.push_str(format!("{}({}): {}\n", self.inner[root_id], root_id, format).as_str());
        }
        write!(f, "{}", string)
    }
}

impl<T: Display + Default + Clone> Forest<T> {
    pub fn union(&mut self, x: &NodeId, y: &NodeId) {
        let x_root = *self.find(x);
        let y_root = *self.find(y);
        if x_root != y_root {
            if self.get_rank(&x_root) < self.get_rank(&y_root) {
                self.set_parent(&x_root, &y_root);
            } else {
                self.set_parent(&y_root, &x_root);
                if self.get_rank(x) == self.get_rank(y) {
                    self.set_rank(&x_root, self.get_rank(&x_root) + 1)
                }
            }
        }
        self.flat_bindings()
    }

    /// x becomes the parent of y
    pub fn union_ordered(&mut self, x: &NodeId, y: &NodeId) {
        let x_root = *self.find(x);
        let y_root = *self.find(y);
        if x_root != y_root {
            self.set_parent(&y_root, &x_root);
            self.set_rank(&x_root, self.get_rank(&x_root) + 1)
        }
        self.flat_bindings()
    }

    pub fn find(&mut self, x: &NodeId) -> &NodeId {
        if x != self.get_parent(x) {
            let parent = *self.get_parent(x);
            let parent = *self.find(&parent);
            self.set_parent(x, &parent);
            self.set_rank(&parent, self.get_rank(&parent) + 1);
        }
        self.get_parent(x)
    }
}
