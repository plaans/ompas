use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable, GetVariables};
use crate::structs::flow_graph::flow::{BranchingFlow, Flow, FlowId, FlowKind};
use crate::structs::flow_graph::handle_table::HandleTable;
use crate::structs::flow_graph::vertice::Vertice;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
use im::{hashset, HashSet};
use std::collections::HashMap;
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
    pub(crate) map_atom_id_flow_id: HashMap<AtomId, HashSet<VerticeId>>,
    pub(crate) map_vertice_id_flow_id: HashMap<VerticeId, FlowId>,
    pub(crate) handles: HandleTable,
    pub(crate) flow: FlowId,
}

impl FlowGraph {
    pub fn new(sym_table: RefSymTable) -> Self {
        Self {
            sym_table,
            vertices: vec![],
            flows: vec![],
            map_atom_id_flow_id: Default::default(),
            map_vertice_id_flow_id: Default::default(),
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
        match &flow.kind {
            FlowKind::Vertice(v) => self.get_vertice_result(v),
            FlowKind::Seq(s) => self.get_flow_result(&s.last().unwrap()),
            FlowKind::Branching(branching) => self.get_flow_result(&branching.result),
        }
    }

    pub fn get_flow_interval(&self, flow: &FlowId) -> Interval {
        let flow = &self.flows[*flow];
        match &flow.kind {
            FlowKind::Vertice(v) => *self.get_vertice_interval(v),
            FlowKind::Seq(seq) => {
                let start = *self.get_flow_interval(&seq.first().unwrap()).get_start();
                let end = *self.get_flow_interval(&seq.last().unwrap()).get_end();
                Interval::new(&start, &end)
            }
            FlowKind::Branching(branching) => {
                let start = *self.get_flow_interval(&branching.cond_flow).get_start();
                let end = *self.get_flow_interval(&branching.result).get_end();
                Interval::new(&start, &end)
            }
        }
    }

    pub fn get_vertice_interval(&self, id: &VerticeId) -> &Interval {
        self.vertices.get(*id).unwrap().get_interval()
    }

    pub fn new_instantaneous_vertice(&mut self, value: impl Into<Lit>) -> VerticeId {
        let id = self.vertices.len();

        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &t);

        let vertice = Vertice {
            id,
            interval: Interval::new_instantaneous(&t),
            result: r,
            lit: value.into(),
        };

        self.vertices.push(vertice);
        id
    }

    pub fn new_vertice(&mut self, value: impl Into<Lit>) -> VerticeId {
        let id = self.vertices.len();

        let start = self.sym_table.new_timepoint();
        let end = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &end);

        let vertice = Vertice {
            id,
            interval: Interval::new(&start, &end),
            result: r,
            lit: value.into(),
        };

        let mut atoms = hashset![start, end, r];
        atoms = atoms.union(vertice.lit.get_variables());
        for atom in atoms {
            match self.map_atom_id_flow_id.get_mut(&atom) {
                None => {
                    self.map_atom_id_flow_id.insert(atom, hashset![id]);
                }
                Some(set) => {
                    set.insert(id);
                }
            }
        }

        self.vertices.push(vertice);
        id
    }

    pub fn get_atom_of_flow(&self, flow: &FlowId) -> im::HashSet<AtomId> {
        let flow = &self.flows[*flow];

        match &flow.kind {
            FlowKind::Vertice(id) => self.get_atom_of_vertice(id),
            FlowKind::Seq(seq) => {
                let mut set = im::HashSet::default();
                for f in seq {
                    set = set.union(self.get_atom_of_flow(f));
                }
                set
            }
            FlowKind::Branching(branching) => {
                let mut set = self.get_atom_of_flow(&branching.cond_flow);
                set = set.union(self.get_atom_of_flow(&branching.result));
                set
            }
        }
    }

    pub fn get_atom_of_vertice(&self, vertice: &VerticeId) -> im::HashSet<AtomId> {
        let vertice = &self.vertices[*vertice];
        let mut set = vertice.interval.get_variables();
        set.insert(vertice.result);
        let set = set.union(vertice.lit.get_variables());
        set
    }

    pub fn new_flow(&mut self, flow: impl Into<Flow>) -> FlowId {
        let id = self.flows.len();
        let flow = flow.into();
        match &flow.kind {
            FlowKind::Vertice(v) => {
                self.map_vertice_id_flow_id.insert(*v, id);
            }
            FlowKind::Seq(seq) => {
                for f in seq {
                    self.flows[*f].parent = Some(id);
                }
            }
            FlowKind::Branching(branching) => {
                self.flows[branching.cond_flow].parent = Some(id);
                self.flows[branching.false_flow].parent = Some(id);
                self.flows[branching.true_flow].parent = Some(id);
                self.flows[branching.result].parent = Some(id);
            }
        }
        self.flows.push(flow.into());
        id
    }

    pub fn new_vertice_flow(&mut self, vertice_id: VerticeId) -> FlowId {
        self.new_flow(FlowKind::Vertice(vertice_id))
    }

    pub fn new_seq_flow(&mut self, seq: Vec<FlowId>) -> FlowId {
        let id = self.flows.len();
        for f in &seq {
            self.flows[*f].parent = Some(id)
        }
        self.flows.push(FlowKind::Seq(seq).into());
        id
    }

    pub fn merge_flow(&mut self, f1_id: &FlowId, f2_id: &FlowId) -> FlowId {
        let f1 = &self.flows[*f1_id];
        let f2 = &self.flows[*f2_id];

        let flow: FlowKind = match (&f1.kind, &f2.kind) {
            (FlowKind::Seq(s1), FlowKind::Seq(s2)) => {
                let mut seq = s1.clone();
                seq.append(&mut s2.clone());
                FlowKind::Seq(seq)
            }
            (FlowKind::Seq(s1), _) => {
                let mut seq = s1.clone();
                seq.push(*f2_id);
                FlowKind::Seq(seq)
            }
            (_, FlowKind::Seq(s2)) => {
                let mut seq = vec![*f1_id];
                seq.append(&mut s2.clone());
                FlowKind::Seq(seq)
            }
            (_, _) => FlowKind::Seq(vec![*f1_id, *f2_id]),
        };

        self.new_flow(flow)
    }

    pub fn remove_flow(&mut self, flow: &FlowId) {
        if let Some(parent_id) = self.flows[*flow].parent {
            let parent = &mut self.flows[parent_id];
            match &mut parent.kind {
                FlowKind::Seq(seq) => {
                    seq.retain(|f| f != flow);
                    if seq.is_empty() {
                        self.remove_flow(&parent_id)
                    }
                }
                _ => unreachable!(),
            }
        }
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

        match &self.flows[*id].kind {
            FlowKind::Vertice(v) => {
                let vertice: &Vertice = &self.vertices[*v];
                dot.push_str(
                    format!(
                        "V{v} [label= \"{}: {} <- {}\"]\n",
                        vertice.interval.format(sym_table, false),
                        vertice.result.format(sym_table, false),
                        vertice.lit.format(sym_table, false),
                    )
                    .as_str(),
                );
                start = Some(*v);
                end = Some(*v);
            }
            FlowKind::Branching(branching) => {
                let (cond_dot, (cond_start, cond_end)) = self.export_flow(&branching.cond_flow);
                let cond = self
                    .sym_table
                    .get_debug(&self.get_vertice_result(&cond_end));
                start = Some(cond_start);

                let (true_dot, (true_start, true_end)) = self.export_flow(&branching.true_flow);
                let (false_dot, (false_start, false_end)) = self.export_flow(&branching.false_flow);
                let (result_dot, (result_start, result_end)) = self.export_flow(&branching.result);
                write!(dot, "{cond_dot}{true_dot}{false_dot}{result_dot}").unwrap();
                write!(dot, "V{cond_end} -> V{true_start} [label = \"{cond}\"]\n",).unwrap();
                write!(dot, "V{cond_end} -> V{false_start} [label = \"!{cond}\"]\n",).unwrap();
                write!(dot, "V{true_end} -> V{result_start}\n").unwrap();
                write!(dot, "V{false_end}-> V{result_start}\n").unwrap();
                end = Some(result_end);
            }
            FlowKind::Seq(seq) => {
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

        (dot, (start.unwrap(), end.unwrap()))
    }

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        write!(dot, "{}", self.export_flow(&self.flow).0).unwrap();
        for (id, handle) in self.handles.inner() {
            let (f_dot, (start, end)) = self.export_flow(&handle.flow);
            write!(dot, "{} -> V{start}\n", self.sym_table.get_debug(id)).unwrap();
            write!(dot, "{f_dot}").unwrap();
        }

        dot.push('}');
        dot
    }
}

impl FlatBindings for FlowGraph {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        for v in &mut self.vertices {
            v.result.flat_bindings(st);
            v.lit.flat_bindings(st);
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
