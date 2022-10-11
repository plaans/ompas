use crate::structs::chronicle::sym_table::SymTable;
use crate::structs::chronicle::{AtomId, FormatWithSymTable};
use std::fmt::Write;

pub type Dot = String;

pub type VerticeId = usize;

#[derive(Clone)]
pub struct FlowGraph {
    pub sym_table: SymTable,
    vertices: Vec<Vertice>,
    edges: Vec<Edge>,
}

impl FlowGraph {
    pub(crate) fn inner(&self) -> &Vec<Vertice> {
        &self.vertices
    }

    pub fn get_result(&self, id: &VerticeId) -> &AtomId {
        self.vertices.get(*id).unwrap().get_result()
    }

    pub fn get_timepoint(&self, id: &VerticeId) -> &AtomId {
        self.vertices.get(*id).unwrap().get_timepoint()
    }

    pub(crate) fn push(&mut self, mut vertice: Vertice) -> VerticeId {
        let id = self.vertices.len();
        vertice.id = id;
        self.vertices.push(vertice);
        id
    }

    pub fn new_vertice(
        &mut self,
        value: impl Into<Expression>,
        parent: Option<VerticeId>,
    ) -> VerticeId {
        let id = self.vertices.len();

        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        let vertice = Vertice {
            id,
            timepoint: t,
            result: r,
            computation: value.into(),
        };

        self.vertices.push(vertice);

        match parent {
            None => {}
            Some(p_id) => self.edges.push(Edge::new(p_id, id, EdgeKind::Seq)),
        }
        id
    }

    pub fn get(&self, id: &VerticeId) -> Option<&Vertice> {
        self.vertices.get(*id)
    }

    /*pub fn backtrack_result(&self, mut id: &VerticeId) -> VerticeId {
        let mut id = *id;
        loop {
            let node: &Vertice = self.inner.get(id.absolute).unwrap();
            if let Expression::Cst(CstValue::Result(r)) = &node.computation {
                id = *r;
            } else {
                return id;
            }
        }
    }*/

    /*pub fn duplicate_result_node(
        &mut self,
        result_id: usize,
        value: impl Into<Expression>,
        parent: Option<VerticeId>,
    ) -> VerticeId {
        let id =
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
    }*/

    pub fn add_child(&mut self, node_id: &VerticeId, child_id: &VerticeId) {
        self.edges
            .push(Edge::new(*node_id, *child_id, EdgeKind::Seq))
    }

    pub fn add_parent(&mut self, node_id: &VerticeId, parent_id: &VerticeId) {
        self.edges
            .push(Edge::new(*parent_id, *node_id, EdgeKind::Seq))
    }

    /*pub fn set_child_link_lind(&mut self, node_id: &VerticeId, link_kind: EdgeKind) {
        let mut node: &mut Vertice = self.inner.get_mut(node_id.absolute).unwrap();
        node.childs.link_kind = link_kind;
        let childs = node.childs.inner.clone();
        for child in childs {
            self.inner
                .get_mut(child.absolute)
                .unwrap()
                .parents
                .link_kind = link_kind;
        }
    }

    pub fn set_parent_link_lind(&mut self, node_id: &VerticeId, link_kind: EdgeKind) {
        let mut node: &mut Vertice = self.inner.get_mut(node_id.absolute).unwrap();
        node.parents.link_kind = link_kind;
        let parents = node.parents.inner.clone();
        for parent in parents {
            self.inner
                .get_mut(parent.absolute)
                .unwrap()
                .parents
                .link_kind = link_kind;
        }
    }*/

    /*
    Dot export
     */

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        /*if !self.inner.is_empty() {
            dot.push_str("S\n");
            dot.push_str(format!("S -> {}0\n", NODE_PREFIX).as_str());
        }*/
        for vertice in &self.vertices {
            let node_name = format!("{}{}", VERTICE_PREFIX, vertice.id);
            let timepoint = self.sym_table.get_atom(&vertice.timepoint, true).unwrap();
            let result = self.sym_table.get_atom(&vertice.result, true).unwrap();
            match vertice.computation {
                Expression::Start | Expression::End => {
                    dot.push_str(
                        format!(
                            "{} [label= \"{}\"]\n",
                            node_name,
                            vertice.computation.format(&self.sym_table, true),
                        )
                        .as_str(),
                    );
                }
                _ => {
                    dot.push_str(
                        format!(
                            "{} [label= \"{}: {} <- {}\"]\n",
                            node_name,
                            timepoint,
                            result,
                            vertice.computation.format(&self.sym_table, true),
                        )
                        .as_str(),
                    );
                }
            }
        }

        for edge in &self.edges {
            let from = format!("{}{}", VERTICE_PREFIX, edge.from);
            let to = format!("{}{}", VERTICE_PREFIX, edge.to);

            match edge.kind {
                EdgeKind::Seq => {
                    dot.push_str(format!("{} -> {}\n", from, to).as_str());
                }
                EdgeKind::Branching => {
                    dot.push_str(format!("{} -> {} [style = dashed]\n", from, to,).as_str());
                    /*dot.push_str(
                        format!(
                            "{} -> {} [style = dashed label = \"!{}\"]\n",
                            node_name, false_branch, result_name
                        )
                        .as_str(),
                    );*/
                }
            }
        }

        dot.push('}');
        dot
    }
}

impl Default for FlowGraph {
    fn default() -> Self {
        let sym_table = SymTable::default();
        Self {
            sym_table,
            vertices: vec![],
            edges: vec![],
        }
    }
}

pub const VERTICE_PREFIX: char = 'V';
pub const RESULT_PREFIX: char = 'r';
pub const TIMEPOINT_PREFIX: char = 't';
pub const BRANCHING_ARROW: &str = "->";
pub const DASHED_ATTRIBUTE: &str = "[style = dashed]";
pub const PAR_ARROW: &str = "->";

#[derive(Copy, Clone)]
pub struct Edge {
    from: VerticeId,
    to: VerticeId,
    kind: EdgeKind,
}

impl Edge {
    pub fn new(from: VerticeId, to: VerticeId, kind: EdgeKind) -> Self {
        Self { from, to, kind }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum EdgeKind {
    Seq,
    Branching,
}

impl Default for EdgeKind {
    fn default() -> Self {
        Self::Seq
    }
}
/*
#[derive(Clone, Debug, Default)]
pub struct Parents {
    inner: Vec<VerticeId>,
    link_kind: LinkKind,
}

#[derive(Clone, Debug, Default)]
pub struct Childs {
    inner: Vec<VerticeId>,
    link_kind: LinkKind,
}*/

#[derive(Clone, Debug)]
pub struct Vertice {
    pub(crate) id: VerticeId,
    pub(crate) timepoint: AtomId,
    pub(crate) result: AtomId,
    //pub parents: Parents,
    //pub childs: Childs,
    pub(crate) computation: Expression,
}

impl Vertice {
    /*
    ADDERS
     */
    /*pub fn add_parent(&mut self, parent: &VerticeId) {
        self.parents.inner.push(*parent)
    }

    pub fn add_child(&mut self, child: &VerticeId) {
        self.childs.inner.push(*child)
    }

    /*GETTERS*/
    pub fn get_parents(&self) -> &Vec<VerticeId> {
        &self.parents.inner
    }

    pub fn get_childs(&self) -> &Vec<VerticeId> {
        &self.childs.inner
    }*/

    pub fn get_computation(&self) -> &Expression {
        &self.computation
    }

    pub fn get_id(&self) -> &VerticeId {
        &self.id
    }

    pub fn get_result(&self) -> &AtomId {
        &self.result
    }

    pub fn get_timepoint(&self) -> &AtomId {
        &self.timepoint
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Apply(Vec<AtomId>),
    Write(Vec<AtomId>),
    Read(Vec<AtomId>),
    Cst(AtomId),
    Handle(AtomId),
    Start,
    End,
}

impl Expression {
    pub fn apply(vec: Vec<AtomId>) -> Self {
        Self::Apply(vec)
    }

    pub fn write(vec: Vec<AtomId>) -> Self {
        Self::Write(vec)
    }

    pub fn read(vec: Vec<AtomId>) -> Self {
        Self::Read(vec)
    }

    pub fn cst(cst: AtomId) -> Self {
        Self::Cst(cst)
    }

    pub fn handle(h: AtomId) -> Self {
        Self::Handle(h)
    }
}

impl FormatWithSymTable for Expression {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            Expression::Apply(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "apply({})", args)
            }
            Expression::Write(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "apply({})", args)
            }
            Expression::Read(vec) => {
                let mut args = "".to_string();
                let mut first = true;

                for atom in vec {
                    if first {
                        first = false;
                        args.push_str(atom.format(st, sym_version).as_str())
                    } else {
                        args.push_str(format!(",{}", atom.format(st, sym_version)).as_str())
                    }
                }
                write!(str, "read({})", args)
            }
            Expression::Cst(cst) => {
                let mut args = "".to_string();
                write!(str, "cst({})", cst.format(st, sym_version))
            }
            Expression::Handle(vertice) => {
                write!(str, "handle({})", vertice.format(st, sym_version))
            }
            Expression::Start => write!(str, "start"),
            Expression::End => write!(str, "end"),
        };
        str
    }
}

/*
#[derive(Debug, Clone)]
pub enum CstValue {
    Result(usize),
    Timepoint(usize),
    Number(LNumber),
    Bool(bool),
    Symbol(String),
    String(String),
    Expression(LValue),
}

impl Display for CstValue {
    fn fmt(&self, str: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CstValue::Number(n) => write!(str, "{}", n),
            CstValue::Bool(b) => write!(str, "{}", b),
            CstValue::Symbol(s) => write!(str, "{}", s),
            CstValue::String(s) => write!(str, "{}", s),
            CstValue::Expression(e) => write!(str, "{}", e),
            CstValue::Result(r) => write!(str, "{}{}", RESULT_PREFIX, r.relative),
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
}*/
