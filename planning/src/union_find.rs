use std::clone::Clone;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

#[derive(Default, Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeId {
    inner: usize,
}

impl From<&usize> for NodeId {
    fn from(u: &usize) -> Self {
        Self { inner: u.clone() }
    }
}

impl From<usize> for NodeId {
    fn from(u: usize) -> Self {
        (&u).into()
    }
}

impl NodeId {
    pub fn index(&self) -> &usize {
        &self.inner
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

pub type Rank = usize;

#[derive(Clone, Default)]
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

    pub fn get_parent(&self) -> NodeId {
        self.parent
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

#[derive(Default)]
pub struct Forest<T: Display + Default + Clone> {
    inner: Vec<Node<T>>,
}

impl<T: Display + Default + Clone> Forest<T> {
    pub fn new_node(&mut self, value: T) -> NodeId {
        let id = self.inner.len().into();
        let node = Node::new(value, id);
        self.inner.push(node);
        id
    }

    pub fn get_parent(&self, id: &NodeId) -> NodeId {
        self.inner
            .get(*id.index())
            .expect("strong error")
            .get_parent()
    }

    pub fn set_parent(&mut self, id: &NodeId, parent: NodeId) {
        self.inner
            .get_mut(*id.index())
            .expect("")
            .set_parent(parent);
    }

    pub fn get_rank(&self, id: &NodeId) -> Rank {
        self.inner
            .get(*id.index())
            .expect("strong error")
            .get_rank()
    }

    pub fn set_rank(&mut self, id: &NodeId, rank: Rank) {
        self.inner.get_mut(*id.index()).expect("").set_rank(rank);
    }
}

impl<T: Display + Default + Clone> Forest<T> {
    pub fn format(&self) -> String {
        let mut string = String::new();

        let mut groups: HashMap<NodeId, VecDeque<Node<T>>> = Default::default();

        for (node_id, node) in self.inner.iter().enumerate() {
            let node_id = &node_id.into();
            if let Some(vec) = groups.get_mut(node_id) {
                if self.get_parent(node_id) == *node_id {
                    vec.push_front(node.clone());
                } else {
                    vec.push_back(node.clone());
                }
            } else {
                groups.insert(*node_id, {
                    let mut vec = VecDeque::new();
                    vec.push_front(node.clone());
                    vec
                });
            }
        }

        for (_, tree) in groups {
            let mut format = "{".to_string();
            for (i, element) in tree.iter().enumerate() {
                if i != 0 {
                    if i != 1 {
                        format.push(',');
                    }

                    format.push_str(format!("{}", element).as_str())
                }
            }

            format.push_str("}");

            string
                .push_str(format!("{}: {}\n", tree.front().unwrap().get_value(), format).as_str());
        }
        string
    }
}

pub fn union<T: Display + Default + Clone>(forest: &mut Forest<T>, x: &NodeId, y: &mut NodeId) {
    let x_root = find(forest, x);
    let y_root = find(forest, y);
    if x_root != y_root {
        if forest.get_rank(&x_root) < forest.get_rank(&y_root) {
            forest.set_parent(&x_root, y_root);
        } else {
            forest.set_parent(&y_root, x_root);
            if forest.get_rank(x) == forest.get_rank(y) {
                forest.set_rank(&x_root, forest.get_rank(&x_root) + 1)
            }
        }
    }
}
pub fn find<T: Display + Default + Clone>(forest: &mut Forest<T>, x: &NodeId) -> NodeId {
    if *x != forest.get_parent(x) {
        let parent = forest.get_parent(x);
        let parent = find(forest, &parent);
        forest.set_parent(x, parent);
    }
    forest.get_parent(x).clone()
}

/*
 fonction MakeSet(x)
     x.parent := x
     x.rang   := 0

 fonction Union(x, y)
     xRacine := Find(x)
     yRacine := Find(y)
     si xRacine


    {\displaystyle \neq }

\neq  yRacine
           si xRacine.rang < yRacine.rang
                 xRacine.parent := yRacine
           sinon
                 yRacine.parent := xRacine
                 si xRacine.rang == yRacine.rang
                         xRacine.rang := xRacine.rang + 1

                          fonction Find(x)
     si x.parent


    {\displaystyle \neq }

\neq  x
        x.parent := Find(x.parent)
     retourner x.parent
 */
