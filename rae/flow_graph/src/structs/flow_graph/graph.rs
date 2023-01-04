use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable};
use crate::structs::flow_graph::expression::Expression;
use crate::structs::flow_graph::flow::{Flow, FlowId};
use crate::structs::flow_graph::handle_table::HandleTable;
use crate::structs::flow_graph::scope::Scope;
use crate::structs::flow_graph::vertice::Vertice;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
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
    pub(crate) flows: Vec<Flow>,
    pub(crate) handles: HandleTable,
    pub(crate) flow: FlowId,
}

impl FlowGraph {
    pub fn new(sym_table: RefSymTable) -> Self {
        Self {
            sym_table,
            vertices: vec![],
            flows: vec![],
            handles: Default::default(),
            flow: 0,
        }
    }

    /*pub(crate) fn vertices(&self) -> &Vec<Vertice> {
        &self.vertices
    }*/

    pub fn get_vertice_result(&self, id: &VerticeId) -> AtomId {
        self.vertices.get(*id).unwrap().get_result()
    }

    pub fn get_flow_result(&self, flow: &FlowId) -> AtomId {
        let flow = &self.flows[*flow];
        match flow {
            Flow::Vertice(v) => self.get_vertice_result(v),
            Flow::Seq(s) => self.get_flow_result(&s.last().unwrap()),
            Flow::Async(_) => todo!(),
            Flow::If(r#if) => todo!(),
        }
    }

    pub fn get_flow_interval(&self, flow: &FlowId) -> Interval {
        let flow = &self.flows[*flow];
        match flow {
            Flow::Vertice(v) => *self.get_vertice_interval(v),
            Flow::Seq(seq) => {
                let start = *self.get_flow_interval(&seq.first().unwrap()).get_start();
                let end = *self.get_flow_interval(&seq.last().unwrap()).get_end();
                Interval::new(&start, &end)
            }
            Flow::Async(_) => todo!(),
            Flow::If(_) => todo!(),
        }
    }

    pub fn get_vertice_interval(&self, id: &VerticeId) -> &Interval {
        self.vertices.get(*id).unwrap().get_interval()
    }

    pub fn new_instantaneous_vertice(&mut self, value: impl Into<Expression>) -> VerticeId {
        let id = self.vertices.len();

        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &t);

        let vertice = Vertice {
            id,
            interval: Interval::new_instantaneous(&t),
            result: r,
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
            computation: value.into(),
        };

        self.vertices.push(vertice);
        id
    }

    pub fn new_flow(&mut self, flow: impl Into<Flow>) -> FlowId {
        let id = self.flows.len();
        self.flows.push(flow.into());
        id
    }

    pub fn new_vertice_flow(&mut self, vertice_id: VerticeId) -> FlowId {
        self.new_flow(Flow::Vertice(vertice_id))
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

    /*
    Dot export
     */

    pub fn export_flow(&self, id: &FlowId) -> (Dot, (VerticeId, VerticeId)) {
        let sym_table = &self.sym_table;
        let mut dot = "".to_string();
        let mut start = None;
        let mut end = None;

        match &self.flows[*id] {
            Flow::Vertice(v) => {
                let vertice: &Vertice = &self.vertices[*v];
                dot.push_str(
                    format!(
                        "V{v} [label= \"{}: {} <- {}\"]\n",
                        vertice.interval.format(sym_table, false),
                        vertice.result.format(sym_table, false),
                        vertice.computation.format(sym_table, false),
                    )
                    .as_str(),
                );
                start = Some(*v);
                end = Some(*v);
            }
            Flow::If(r#if) => {
                todo!()
            }
            Flow::Seq(seq) => {
                let mut previous_end = None;
                for f in seq {
                    let (f_dot, (f_start, f_end)) = self.export_flow(f);
                    write!(dot, "{}", f_dot).unwrap();
                    if let Some(end) = previous_end {
                        write!(dot, "V{end} -> V{f_start}\n").unwrap();
                    }
                    if start == None {
                        start = Some(f_start)
                    }
                    previous_end = Some(f_end);
                }
                end = previous_end;
            }
            _ => {
                todo!()
            }
        }

        /*while let Some(vertice_id) = next {
            let vertice = self.vertices.get(vertice_id).unwrap();
            let vertice_name = format!("{}{}", VERTICE_PREFIX, vertice.id);
            //let  = self.sym_table.get_atom(&vertice.timepoint, true).unwrap();
            let result = sym_table.get_domain(&vertice.result, false).unwrap();
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
        }*/
        (dot, (start.unwrap(), end.unwrap()))
    }

    /*pub fn export_vertice(&self, id: &VerticeId) -> Dot {
        let mut next = Some(*id);

        let sym_table = &self.sym_table;

        let mut dot = "".to_string();
        while let Some(vertice_id) = next {
            let vertice = self.vertices.get(vertice_id).unwrap();
            let vertice_name = format!("{}{}", VERTICE_PREFIX, vertice.id);
            //let  = self.sym_table.get_atom(&vertice.timepoint, true).unwrap();
            let result = sym_table.get_domain(&vertice.result, false).unwrap();
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
    }*/

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        write!(dot, "{}", self.export_flow(&self.flow).0).unwrap();
        /*for (_, handle) in self.handles.inner() {
            write!(dot, "{}", self.export_vertice(&handle.scope.start)).unwrap();
        }*/

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
