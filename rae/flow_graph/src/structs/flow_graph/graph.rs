use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::{AtomId, FlatBindings, FormatWithSymTable};
use crate::structs::flow_graph::expression::{Block, Expression};
use crate::structs::flow_graph::handle_table::HandleTable;
use crate::structs::flow_graph::scope::Scope;
use crate::structs::flow_graph::vertice::Vertice;
use std::fmt::Write;

pub type Dot = String;

pub type VerticeId = usize;
pub type HandleId = usize;
pub type BlockId = usize;
pub type EdgeId = usize;

#[derive(Clone, Default)]
pub struct FlowGraph {
    pub sym_table: RefSymTable,
    pub(crate) vertices: Vec<Vertice>,
    pub(crate) handles: HandleTable,
    //edges: Vec<Edge>,
    pub(crate) scope: Scope,
}

impl FlowGraph {
    pub fn new(sym_table: RefSymTable) -> Self {
        Self {
            sym_table,
            vertices: vec![],
            handles: Default::default(),
            scope: Default::default(),
        }
    }

    /*pub(crate) fn vertices(&self) -> &Vec<Vertice> {
        &self.vertices
    }*/

    pub fn get_result(&self, id: &VerticeId) -> &AtomId {
        self.vertices.get(*id).unwrap().get_result()
    }

    pub fn get_scope_result(&self, scope: &Scope) -> &AtomId {
        self.get_result(scope.get_end())
    }

    pub fn get_scope_interval(&self, scope: &Scope) -> Interval {
        let start = self.get_interval(scope.start()).get_start();
        let end = self.get_interval(scope.get_end()).get_end();
        Interval::new(start, end)
    }

    pub fn get_interval(&self, id: &VerticeId) -> &Interval {
        self.vertices.get(*id).unwrap().get_interval()
    }

    pub fn remove(&mut self, id: &VerticeId, scope: &mut Scope) {
        let child = *self.get(id).unwrap().get_child();
        let parent = *self.get(id).unwrap().get_parent();
        if scope.start == *id {
            match child {
                None => {}
                Some(child) => {
                    scope.start = child;
                    self.get_mut(&child).unwrap().parent = None;
                }
            }
        } else if scope.end == *id {
            match parent {
                None => {}
                Some(parent) => {
                    scope.end = parent;
                    self.get_mut(&parent).unwrap().child = None;
                }
            }
        } else {
            self.set_parent(&child.unwrap(), &parent.unwrap())
        }
        let vertice = self.get_mut(id).unwrap();
        vertice.child = None;
        vertice.parent = None;
    }

    /*pub(crate) fn push(&mut self, mut vertice: Vertice) -> VerticeId {
        let id = self.vertices.len();
        vertice.id = id;
        self.vertices.push(vertice);
        id
    }*/

    pub fn new_instantaneous_vertice(&mut self, value: impl Into<Expression>) -> VerticeId {
        let id = self.vertices.len();

        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &t);

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

    pub fn new_vertice(&mut self, value: impl Into<Expression>) -> VerticeId {
        let id = self.vertices.len();

        let start = self.sym_table.new_timepoint();
        let end = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &end);

        let vertice = Vertice {
            id,
            interval: Interval::new(&start, &end),
            result: r,
            parent: None,
            child: None,
            computation: value.into(),
        };

        self.vertices.push(vertice);
        id
    }

    /*pub fn new_handle(&mut self, handle: Handle) -> VerticeId {
        let id = self.sym_table.new_handle();
        self.handles.insert(id, handle);
        self.new_vertice(Expression::Handle(id))
    }*/

    pub fn get(&self, id: &VerticeId) -> Option<&Vertice> {
        self.vertices.get(*id)
    }

    pub fn get_mut(&mut self, id: &VerticeId) -> Option<&mut Vertice> {
        self.vertices.get_mut(*id)
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
                    } /*Block::Handle(async_block) => {
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
                          dot.push_str(
                              self.export_vertice(async_block.scope_expression.start())
                                  .as_str(),
                          );
                      }*/
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

        write!(dot, "{}", self.export_vertice(&self.scope.start)).unwrap();
        for (_, handle) in self.handles.inner() {
            write!(dot, "{}", self.export_vertice(&handle.scope.start)).unwrap();
        }

        dot.push('}');
        dot
    }
}

impl FlatBindings for FlowGraph {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        for v in &mut self.vertices {
            v.result.flat_bindings(st);
            v.computation.flat_bindings(st);
            v.interval.flat_bindings(st);
        }
    }
}

pub const VERTICE_PREFIX: char = 'V';
pub const RESULT_PREFIX: char = 'r';
pub const HANDLE_PREFIX: char = 'h';
pub const IF_PREFIX: &str = "if";
pub const TIMEPOINT_PREFIX: char = 't';
pub const BRANCHING_ARROW: &str = "->";
pub const DASHED_ATTRIBUTE: &str = "[style = dashed]";
pub const PAR_ARROW: &str = "->";
