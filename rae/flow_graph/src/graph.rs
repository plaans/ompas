use log::Level::Debug;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub type NodeId = usize;
pub type Dot = String;

#[derive(Default, Debug, Clone)]
pub struct FlowGraph {
    inner: Vec<Node>,
}

pub const NODE_PREFIX: char = 'N';
pub const RESULT_PREFIX: char = 'r';
pub const TIMEPOINT_PREFIX: char = 't';
pub const BRANCHING_ARROW: &str = "->";
pub const PAR_ARROW: &str = "->";

#[derive(Debug, Copy, Clone)]
enum LinkKind {
    Par,
    Branching,
}

impl Default for LinkKind {
    fn default() -> Self {
        Self::Branching
    }
}

impl FlowGraph {
    pub fn new_node(&mut self, value: impl Into<Computation>, parent: Option<NodeId>) -> NodeId {
        let id = self.inner.len();

        let node = Node {
            id,
            parents: Default::default(),
            childs: Default::default(),
            computation: value.into(),
        };

        self.inner.push(node);
        match parent {
            None => {}
            Some(p) => self.add_parent(&id, &p),
        }
        id
    }

    pub fn add_child(&mut self, node_id: &NodeId, child_id: &NodeId) {
        self.inner.get_mut(*node_id).unwrap().add_child(child_id);
        self.inner.get_mut(*child_id).unwrap().add_parent(node_id);
    }

    pub fn add_parent(&mut self, node_id: &NodeId, parent_id: &NodeId) {
        self.inner.get_mut(*node_id).unwrap().add_parent(parent_id);
        self.inner.get_mut(*parent_id).unwrap().add_child(node_id);
    }

    /*
    Dot export
     */

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        for node in &self.inner {
            let node_name = format!("{}{}", NODE_PREFIX, node.id);
            let timepoint_name = format!("{}{}", TIMEPOINT_PREFIX, node.id);
            let result_name = format!("{}{}", RESULT_PREFIX, node.id);
            dot.push_str(
                format!(
                    "{} [label= \"{}: {} <- {}\"]\n",
                    node_name,
                    timepoint_name,
                    result_name,
                    node.computation.to_string(),
                )
                .as_str(),
            );
            for child in &node.childs.inner {
                let arrow = match node.childs.link_kind {
                    LinkKind::Par => PAR_ARROW,
                    LinkKind::Branching => BRANCHING_ARROW,
                };
                let child_name = format!("{}{}", NODE_PREFIX, child);
                dot.push_str(format!("{} {} {}\n", node_name, arrow, child_name).as_str());
            }
        }

        dot.push('}');
        dot
    }
}

#[derive(Clone, Debug, Default)]
pub struct Parents {
    inner: Vec<NodeId>,
    link_kind: LinkKind,
}

#[derive(Clone, Debug, Default)]
pub struct Childs {
    inner: Vec<NodeId>,
    link_kind: LinkKind,
}

#[derive(Clone, Debug)]
pub struct Node {
    id: NodeId,
    parents: Parents,
    childs: Childs,
    computation: Computation,
}

impl Node {
    /*
    ADDERS
     */
    pub fn add_parent(&mut self, parent: &NodeId) {
        self.parents.inner.push(*parent)
    }

    pub fn add_child(&mut self, child: &NodeId) {
        self.childs.inner.push(*child)
    }

    /*GETTERS*/
    pub fn get_parents(&mut self) -> &Vec<NodeId> {
        &self.parents.inner
    }

    pub fn get_childs(&mut self) -> &Vec<NodeId> {
        &self.childs.inner
    }

    pub fn get_computation(&mut self) -> &Computation {
        &self.computation
    }

    pub fn get_id(&mut self) -> &NodeId {
        &self.id
    }
}

impl From<CstValue> for Computation {
    fn from(cst: CstValue) -> Self {
        Self::Cst(cst)
    }
}

#[derive(Debug, Clone)]
pub enum Computation {
    Apply(Vec<NodeId>),
    Write(Vec<NodeId>),
    Read(Vec<NodeId>),
    Cst(CstValue),
    If,
}

impl Computation {
    pub fn apply(vec: Vec<NodeId>) -> Computation {
        Computation::Apply(vec)
    }

    pub fn write(vec: Vec<NodeId>) -> Computation {
        Computation::Write(vec)
    }

    pub fn read(vec: Vec<NodeId>) -> Computation {
        Computation::Read(vec)
    }

    pub fn cst(cst: impl Into<CstValue>) -> Computation {
        Computation::Cst(cst.into())
    }
}

impl Display for Computation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Computation::Apply(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for node in vec {
                    if first {
                        first = false;
                        args.push_str(format!("{}{}", RESULT_PREFIX, node).as_str())
                    } else {
                        args.push_str(format!(",{}{}", RESULT_PREFIX, node).as_str())
                    }
                }
                write!(f, "apply({})", args)
            }
            Computation::Write(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for node in vec {
                    if first {
                        first = false;
                        args.push_str(format!("{}{}", RESULT_PREFIX, node).as_str())
                    } else {
                        args.push_str(format!(",{}{}", RESULT_PREFIX, node).as_str())
                    }
                }
                write!(f, "apply({})", args)
            }
            Computation::Read(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for node in vec {
                    if first {
                        first = false;
                        args.push_str(format!("{}{}", RESULT_PREFIX, node).as_str())
                    } else {
                        args.push_str(format!(",{}{}", RESULT_PREFIX, node).as_str())
                    }
                }
                write!(f, "read({})", args)
            }
            Computation::Cst(cst) => {
                let mut args = "".to_string();
                write!(f, "cst({})", cst.to_string())
            }
            Computation::If => {
                write!(f, "if")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum CstValue {
    Result(NodeId),
    Number(LNumber),
    Bool(bool),
    Symbol(String),
    String(String),
    Expression(LValue),
}

impl Display for CstValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CstValue::Number(n) => write!(f, "{}", n),
            CstValue::Bool(b) => write!(f, "{}", b),
            CstValue::Symbol(s) => write!(f, "{}", s),
            CstValue::String(s) => write!(f, "{}", s),
            CstValue::Expression(e) => write!(f, "{}", e),
            CstValue::Result(r) => write!(f, "{}{}", RESULT_PREFIX, r),
        }
    }
}

impl CstValue {
    pub fn number(n: LNumber) -> Self {
        Self::Number(n)
    }
    pub fn bool(b: bool) -> Self {
        Self::Bool(b)
    }
    pub fn symbol(s: String) -> Self {
        Self::Symbol(s)
    }
    pub fn string(s: String) -> Self {
        Self::String(s)
    }
    pub fn expression(e: LValue) -> Self {
        Self::Expression(e)
    }
    pub fn result(r: NodeId) -> Self {
        Self::Result(r)
    }
}
