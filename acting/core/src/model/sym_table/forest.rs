use new_type::newtype;
use std::clone::Clone;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

newtype!(NodeId: usize);
impl From<NodeId> for usize {
    fn from(value: NodeId) -> Self {
        value.0
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

newtype!(Rank: usize);

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
    pub fn set_parent(&mut self, parent: NodeId) {
        self.parent = parent;
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
pub struct Forest<T: Display + Default + Clone, I: Into<NodeId> + From<NodeId>> {
    inner: Vec<Node<T>>,
    _index: PhantomData<I>,
}

impl<T, I> Index<I> for Forest<T, I>
where
    T: Display + Default + Clone + Default,
    I: Into<NodeId> + From<NodeId>,
{
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.inner[index.into().0].value
    }
}

impl<T, I> IndexMut<I> for Forest<T, I>
where
    T: Display + Default + Clone + Default,
    I: Into<NodeId> + From<NodeId>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.inner[index.into()].value
    }
}

impl<T> Index<NodeId> for Vec<Node<T>>
where
    T: Display + Default + Clone + Default,
{
    type Output = Node<T>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self[index.0]
    }
}

impl<T> IndexMut<NodeId> for Vec<Node<T>>
where
    T: Display + Default + Clone + Default,
{
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self[index.0]
    }
}

impl<T: Display + Default + Clone, I: Into<NodeId> + From<NodeId>> Forest<T, I> {
    pub fn flat_bindings(&mut self) {
        for i in 0..self.inner.len() {
            self._find(NodeId(i));
        }
    }

    pub fn get_node(&self, id: I) -> Option<&Node<T>> {
        self._get_node(id.into())
    }

    fn _get_node(&self, id: NodeId) -> Option<&Node<T>> {
        self.inner.get(id.0)
    }

    pub fn get_value(&self, id: I) -> Option<&T> {
        self._get_value(id.into())
    }

    fn _get_value(&self, id: NodeId) -> Option<&T> {
        self.inner.get(id.0).map(|n| &n.value)
    }

    pub fn set_value(&mut self, id: I, value: T) {
        self._set_value(id.into(), value)
    }

    fn _set_value(&mut self, id: NodeId, value: T) {
        if let Some(node) = self.inner.get_mut(id.0) {
            node.value = value
        }
    }

    pub fn new_node(&mut self, value: T) -> I {
        let id = self.inner.len();
        let node = Node::new(value, id.into());
        self.inner.push(node);
        NodeId(id).into()
    }

    pub fn get_parent(&self, id: I) -> I {
        self._get_parent(id.into()).into()
    }

    fn _get_parent(&self, id: NodeId) -> NodeId {
        self.inner[id].parent
    }

    pub fn set_parent(&mut self, id: I, parent: I) {
        self._set_parent(id.into(), parent.into())
    }

    pub fn _set_parent(&mut self, id: NodeId, parent: NodeId) {
        self.inner[id].parent = parent
    }

    pub fn get_rank(&self, id: I) -> Rank {
        self._get_rank(id.into())
    }

    fn _get_rank(&self, id: NodeId) -> Rank {
        self.inner[id].rank
    }

    fn _set_rank(&mut self, id: NodeId, rank: Rank) {
        self.inner[id.0].rank = rank
    }

    pub fn set_rank(&mut self, id: I, rank: Rank) {
        self._set_rank(id.into(), rank)
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

impl<T: Debug + Display + Default + Clone, I: Into<NodeId> + From<NodeId>> Display
    for Forest<T, I>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut string = String::new();

        let mut groups: HashMap<NodeId, VecDeque<(NodeId, Node<T>)>> = Default::default();

        for (node_id, node) in self.inner.iter().enumerate() {
            let parent_id = node.get_parent();
            if parent_id.0 != node_id {
                if let Some(vec) = groups.get_mut(parent_id) {
                    vec.push_back((NodeId(node_id), node.clone()));
                } else {
                    groups.insert(*parent_id, {
                        let mut vec = VecDeque::new();
                        vec.push_front((NodeId(node_id), node.clone()));
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

impl<T: Display + Default + Clone, I: Into<NodeId> + From<NodeId>> Forest<T, I> {
    pub fn union(&mut self, x: I, y: I) {
        self._union(x.into(), y.into())
    }

    fn _union(&mut self, x: NodeId, y: NodeId) {
        let x_root = self._find(x);
        let y_root = self._find(y);
        if x_root != y_root {
            if self._get_rank(x_root) < self._get_rank(y_root) {
                self._set_parent(x_root, y_root);
            } else {
                self._set_parent(y_root, x_root);
                if self._get_rank(x) == self._get_rank(y) {
                    self._set_rank(x_root, self._get_rank(x_root) + Rank(1))
                }
            }
        }
        //self.flat_bindings()
    }

    pub fn union_ordered(&mut self, x: I, y: I) {
        self._union_ordered(x.into(), y.into());
    }

    /// x becomes the parent of y
    fn _union_ordered(&mut self, x: NodeId, y: NodeId) {
        let x_root = self._find(x);
        let y_root = self._find(y);
        if x_root != y_root {
            self._set_parent(y_root, x_root);
            self._set_rank(x_root, self._get_rank(x_root) + Rank(1));
        }
    }

    pub fn find(&mut self, x: I) -> I {
        self._find(x.into()).into()
    }

    fn _find(&mut self, x: NodeId) -> NodeId {
        let x = x.into();
        if x != self._get_parent(x) {
            let parent = self._get_parent(x);
            let parent = self._find(parent);
            self._set_parent(x, parent);
            self._set_rank(parent, self._get_rank(parent) + Rank(1));
        }
        self.inner[x].parent
    }
}
