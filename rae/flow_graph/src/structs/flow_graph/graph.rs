use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::{AtomId, FormatWithSymTable};
use std::fmt::Write;

pub type Dot = String;

pub type VerticeId = usize;
pub type BlockId = usize;
pub type EdgeId = usize;

#[derive(Clone)]
pub struct FlowGraph {
    pub sym_table: RefSymTable,
    vertices: Vec<Vertice>,
    //edges: Vec<Edge>,
    pub(crate) scope: Scope,
}

impl FlowGraph {
    pub fn new(sym_table: RefSymTable) -> Self {
        Self {
            sym_table,
            vertices: vec![],
            scope: Default::default(),
        }
    }

    pub(crate) fn vertices(&self) -> &Vec<Vertice> {
        &self.vertices
    }

    pub fn get_result(&self, id: &VerticeId) -> &AtomId {
        self.vertices.get(*id).unwrap().get_result()
    }

    pub fn get_interval(&self, id: &VerticeId) -> &Interval {
        self.vertices.get(*id).unwrap().get_interval()
    }

    pub(crate) fn push(&mut self, mut vertice: Vertice) -> VerticeId {
        let id = self.vertices.len();
        vertice.id = id;
        self.vertices.push(vertice);
        id
    }

    pub fn new_vertice(&mut self, value: impl Into<Expression>) -> VerticeId {
        let id = self.vertices.len();

        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        let vertice = Vertice {
            id,
            interval: Interval::new_instantaneous(&t),
            result: r,
            parent: None,
            child: None,
            computation: value.into(),
        };

        self.vertices.push(vertice);
        id
    }

    pub fn get(&self, id: &VerticeId) -> Option<&Vertice> {
        self.vertices.get(*id)
    }

    pub fn set_parent(&mut self, vertice_id: &VerticeId, parent_id: &VerticeId) {
        self.vertices
            .get_mut(*vertice_id)
            .unwrap()
            .set_parent(parent_id);
        self.vertices
            .get_mut(*parent_id)
            .unwrap()
            .set_child(vertice_id);
    }

    pub fn set_child(&mut self, vertice_id: &VerticeId, child_id: &VerticeId) {
        self.vertices
            .get_mut(*vertice_id)
            .unwrap()
            .set_child(child_id);
        self.vertices
            .get_mut(*child_id)
            .unwrap()
            .set_parent(vertice_id);
    }

    /*
    Dot export
     */

    pub fn export_vertice(&self, id: &VerticeId) -> Dot {
        let mut next = Some(*id);

        let sym_table = &self.sym_table;

        let mut dot = "".to_string();
        while let Some(vertice_id) = next {
            let vertice = self.vertices.get(vertice_id).unwrap();
            let vertice_name = format!("{}{}", VERTICE_PREFIX, vertice.id);
            //let  = self.sym_table.get_atom(&vertice.timepoint, true).unwrap();
            let result = sym_table.get_atom(&vertice.result, false).unwrap();
            match &vertice.computation {
                Expression::Block(block) => match block {
                    Block::If(if_block) => {
                        dot.push_str(
                            format!(
                                "{} [label= \"{}: {} <- {}\"]\n",
                                vertice_name,
                                vertice.interval.format(&self.sym_table, false),
                                result,
                                vertice.computation.format(&self.sym_table, false),
                            )
                            .as_str(),
                        );
                        dot.push_str(self.export_vertice(&if_block.true_branch.start).as_str());
                        dot.push_str(self.export_vertice(&if_block.false_branch.start).as_str());
                    }
                },
                _ => {
                    dot.push_str(
                        format!(
                            "{} [label= \"{}: {} <- {}\"]\n",
                            vertice_name,
                            vertice.interval.format(&self.sym_table, false),
                            result,
                            vertice.computation.format(&self.sym_table, false),
                        )
                        .as_str(),
                    );
                }
            }

            if let Some(parent) = vertice.parent {
                let parent = format!("{}{}", VERTICE_PREFIX, parent);
                dot.push_str(format!("{} -> {}\n", parent, vertice_name).as_str());
            }

            next = vertice.child;
        }
        dot
    }

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        dot.push_str(self.export_vertice(&self.scope.start).as_str());

        dot.push('}');
        dot
    }
}

impl Default for FlowGraph {
    fn default() -> Self {
        Self {
            sym_table: Default::default(),
            vertices: vec![],
            scope: Default::default(),
        }
    }
}

#[derive(Copy, Clone, Debug, Default)]
pub struct Scope {
    pub(crate) start: VerticeId,
    pub(crate) end: VerticeId,
}

impl Scope {
    pub fn start(&self) -> &VerticeId {
        &self.start
    }

    pub fn end(&self) -> &VerticeId {
        &self.end
    }

    pub fn set_start(&mut self, start: VerticeId) {
        self.start = start;
    }

    pub fn set_end(&mut self, end: VerticeId) {
        self.end = end;
    }

    pub fn singleton(id: VerticeId) -> Self {
        Self { start: id, end: id }
    }

    pub fn expression(start: VerticeId, end: VerticeId) -> Self {
        Self { start, end }
    }
}

impl From<VerticeId> for Scope {
    fn from(id: VerticeId) -> Self {
        Self::singleton(id)
    }
}

pub const VERTICE_PREFIX: char = 'V';
pub const RESULT_PREFIX: char = 'r';
pub const IF_PREFIX: &str = "if";
pub const TIMEPOINT_PREFIX: char = 't';
pub const BRANCHING_ARROW: &str = "->";
pub const DASHED_ATTRIBUTE: &str = "[style = dashed]";
pub const PAR_ARROW: &str = "->";

#[derive(Debug, Clone)]
pub enum Block {
    If(IfBlock),
}

#[derive(Debug, Clone)]
pub struct IfBlock {
    pub(crate) cond: AtomId,
    pub(crate) true_result: AtomId,
    pub(crate) false_result: AtomId,
    pub(crate) true_branch: Scope,
    pub(crate) false_branch: Scope,
}

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

    pub fn from(&self) -> &VerticeId {
        &self.from
    }

    pub fn to(&self) -> &VerticeId {
        &self.to
    }
}

#[derive(Debug, Copy, Clone)]
pub enum EdgeKind {
    Seq,
    Branching(bool),
}

impl Default for EdgeKind {
    fn default() -> Self {
        Self::Seq
    }
}

#[derive(Clone, Debug)]
pub struct Vertice {
    pub(crate) id: VerticeId,
    pub(crate) interval: Interval,
    pub(crate) result: AtomId,
    pub(crate) parent: Option<VerticeId>,
    pub(crate) child: Option<VerticeId>,
    pub(crate) computation: Expression,
}

impl Vertice {
    /*
    SETTERS
     */
    pub fn set_parent(&mut self, parent: &VerticeId) {
        self.parent = Some(*parent)
    }

    pub fn set_child(&mut self, child: &VerticeId) {
        self.child = Some(*child)
    }

    /*GETTERS*/
    pub fn get_parent(&self) -> &Option<VerticeId> {
        &self.parent
    }

    pub fn get_child(&self) -> &Option<VerticeId> {
        &self.child
    }

    pub fn get_computation(&self) -> &Expression {
        &self.computation
    }

    pub fn get_id(&self) -> &VerticeId {
        &self.id
    }

    pub fn get_result(&self) -> &AtomId {
        &self.result
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_start(&self) -> &AtomId {
        &self.interval.start()
    }

    pub fn get_end(&self) -> &AtomId {
        &self.interval.end()
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Block(Block),
    Err(Lit),
    Exec(Vec<AtomId>),
    Apply(Vec<AtomId>),
    Write(Vec<AtomId>),
    Read(Vec<AtomId>),
    Cst(Lit),
    Handle(AtomId),
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
    pub fn exec(vec: Vec<AtomId>) -> Self {
        Self::Exec(vec)
    }

    pub fn cst(cst: Lit) -> Self {
        Self::Cst(cst)
    }

    pub fn handle(h: AtomId) -> Self {
        Self::Handle(h)
    }

    pub fn err(err: Lit) -> Self {
        Self::Err(err)
    }
}

impl FormatWithSymTable for Expression {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
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
                write!(str, "write({})", args)
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
                write!(str, "cst({})", cst.format(st, sym_version))
            }
            Expression::Handle(vertice) => {
                write!(str, "handle({})", vertice.format(st, sym_version))
            }
            Expression::Exec(vec) => {
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
                write!(str, "exec({})", args)
            }
            Expression::Err(err) => {
                write!(str, "err({})", err.format(st, sym_version))
            }
            Expression::Block(block) => {
                match block {
                    Block::If(i) => {
                        write!(
                            str,
                            "if({},{},{})",
                            i.cond.format(st, sym_version),
                            i.true_result.format(st, sym_version),
                            i.false_result.format(st, sym_version),
                            //i.true_branch.format(st, sym_version),
                            //i.false_branch.format(st, sym_version)
                        )
                    }
                }
            }
        }
        .unwrap();
        str
    }
}
