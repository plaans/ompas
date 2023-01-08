use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable, GetVariables};
use crate::structs::flow_graph::assignment::Assignment;
use crate::structs::flow_graph::flow::{BranchingFlow, Flow, FlowId, FlowKind};
use crate::structs::flow_graph::handle_table::HandleTable;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::AtomId;
use im::hashset;
use std::collections::HashMap;
use std::fmt::Write;

pub type Dot = String;

pub type VerticeId = usize;
pub type HandleId = usize;

#[derive(Clone, Default)]
pub struct FlowGraph {
    pub sym_table: RefSymTable,
    pub(crate) flows: Vec<Flow>,
    pub(crate) map_atom_id_flow_id: HashMap<VerticeId, Vec<FlowId>>,
    pub(crate) handles: HandleTable,
    pub(crate) flow: FlowId,
}

impl FlowGraph {
    pub fn new(sym_table: RefSymTable) -> Self {
        Self {
            sym_table,
            flows: vec![],
            map_atom_id_flow_id: Default::default(),
            handles: Default::default(),
            flow: 0,
        }
    }

    pub fn is_valid(&self, flow: &FlowId) -> bool {
        self.flows[*flow].valid
    }

    pub fn invalidate(&mut self, flow: &FlowId) {
        self.flows[*flow].valid = false
    }

    pub fn get_kind(&self, flow: &FlowId) -> &FlowKind {
        &self.flows[*flow].kind
    }

    pub fn get_mut_kind(&mut self, flow: &FlowId) -> &mut FlowKind {
        &mut self.flows[*flow].kind
    }

    pub fn set_branch(&mut self, flow: &FlowId, branch: bool) {
        if let FlowKind::Branching(br) = &mut self.flows[*flow].kind {
            br.branch = Some(branch)
        }
    }

    pub fn set_kind(&mut self, flow: &FlowId, kind: FlowKind) {
        self.flows[*flow].kind = kind
    }

    pub fn get_parent(&self, flow: &FlowId) -> &Option<FlowId> {
        &self.flows[*flow].parent
    }

    pub fn set_parent(&mut self, flow: &FlowId, parent: &FlowId) {
        self.flows[*flow].parent = Some(*parent)
    }

    pub fn get_flow_result(&self, flow: &FlowId) -> AtomId {
        let flow = &self.flows[*flow];
        match &flow.kind {
            FlowKind::Assignment(v) => v.result,
            FlowKind::Seq(_, r) => self.get_flow_result(r),
            FlowKind::Branching(branching) => self.get_flow_result(&branching.result),
            FlowKind::FlowResult(fr) => *fr,
        }
    }

    pub fn get_flow_interval(&self, flow: &FlowId) -> Interval {
        let flow = &self.flows[*flow];
        match &flow.kind {
            FlowKind::Assignment(v) => v.interval,
            FlowKind::Seq(seq, _) => {
                let start = *self.get_flow_interval(&seq.first().unwrap()).get_start();
                let end = *self.get_flow_interval(&seq.last().unwrap()).get_end();
                Interval::new(&start, &end)
            }
            FlowKind::Branching(branching) => {
                let start = *self.get_flow_interval(&branching.cond_flow).get_start();
                let end = *self.get_flow_interval(&branching.result).get_end();
                Interval::new(&start, &end)
            }
            FlowKind::FlowResult(_) => unreachable!(),
        }
    }

    pub fn new_instantaneous_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &t);

        let vertice = Assignment {
            interval: Interval::new_instantaneous(&t),
            result: r,
            lit: value.into(),
        };
        let id = self.new_flow(vertice);
        self.new_seq(vec![id])
    }

    pub fn new_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let start = self.sym_table.new_timepoint();
        let end = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.new_scope(&r, &end);

        let vertice = Assignment {
            interval: Interval::new(&start, &end),
            result: r,
            lit: value.into(),
        };
        let id = self.new_flow(vertice);
        self.new_seq(vec![id])
    }

    pub fn get_atom_of_flow(&self, flow: &FlowId) -> im::HashSet<AtomId> {
        let flow = &self.flows[*flow];

        match &flow.kind {
            FlowKind::Assignment(a) => a.get_variables(),
            FlowKind::Seq(seq, _) => {
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
            FlowKind::FlowResult(fr) => hashset![*fr],
        }
    }

    pub fn new_seq(&mut self, seq: Vec<FlowId>) -> FlowId {
        assert!(!seq.is_empty());
        let flow = self.merge_flows(seq);
        self.new_flow(flow)
    }

    pub fn new_branching(&mut self, branching: BranchingFlow) -> FlowId {
        let id = self.new_flow(FlowKind::Branching(branching));
        self.new_seq(vec![id])
    }

    pub fn new_result(&mut self, result: AtomId) -> FlowId {
        self.new_flow(FlowKind::FlowResult(result))
    }

    fn new_flow(&mut self, flow: impl Into<Flow>) -> FlowId {
        let id = self.flows.len();
        let flow = flow.into();
        match &flow.kind {
            FlowKind::Assignment(v) => {
                for v in v.get_variables() {
                    match self.map_atom_id_flow_id.get_mut(&v) {
                        None => {
                            self.map_atom_id_flow_id.insert(v, vec![id]);
                        }
                        Some(set) => {
                            set.push(id);
                        }
                    };
                }
            }
            FlowKind::Seq(seq, result) => {
                for f in seq {
                    self.flows[*f].parent = Some(id);
                }
                self.flows[*result].parent = Some(id);
            }
            FlowKind::Branching(branching) => {
                self.flows[branching.cond_flow].parent = Some(id);
                self.flows[branching.false_flow].parent = Some(id);
                self.flows[branching.true_flow].parent = Some(id);
                self.flows[branching.result].parent = Some(id);
            } //FlowKind::Result(_, _) => unreachable!(),
            FlowKind::FlowResult(fr) => {
                match self.map_atom_id_flow_id.get_mut(fr) {
                    None => {
                        self.map_atom_id_flow_id.insert(*fr, vec![id]);
                    }
                    Some(set) => {
                        set.push(id);
                    }
                };
            }
        }
        self.flows.push(flow.into());
        id
    }

    pub fn update_flow(&mut self, id: &FlowId) {
        let kind = if let FlowKind::Seq(seq, _) = self.flows[*id].kind.clone() {
            self.merge_flows(seq)
        } else {
            self.flows[*id].kind.clone()
        };

        self.flows[*id].kind = kind
    }

    pub fn merge_flows(&mut self, flows: Vec<FlowId>) -> FlowKind {
        let mut seq = vec![];
        let mut result = 0;

        for (i, flow_id) in flows.iter().enumerate() {
            let flow = &self.flows[*flow_id];
            if let FlowKind::Seq(s, r) = &flow.kind {
                seq.append(&mut s.clone());
                if i == flows.len() - 1 {
                    result = *r;
                }
            } else {
                seq.push(*flow_id);
                if i == flows.len() - 1 {
                    result = self.new_result(self.get_flow_result(flow_id));
                }
            }
        }

        FlowKind::Seq(seq, result)
    }

    /*pub fn merge_flow(&mut self, f1_id: &FlowId, f2_id: &FlowId) -> FlowId {
        let f1 = &self.flows[*f1_id];
        let f2 = &self.flows[*f2_id];

        let flow: FlowKind = match (&f1.kind, &f2.kind) {
            (FlowKind::Seq(s1, _), FlowKind::Seq(s2, r)) => {
                let mut seq = s1.clone();
                seq.append(&mut s2.clone());
                FlowKind::Seq(seq, *r)
            }
            (FlowKind::Seq(s1, _), _) => {
                let mut seq = s1.clone();
                seq.push(*f2_id);
                let result = self.get_flow_result(f2_id);
                let result = self.new_result(result);
                FlowKind::Seq(seq, result)
            }
            (_, FlowKind::Seq(s2, r)) => {
                let mut seq = vec![*f1_id];
                seq.append(&mut s2.clone());
                FlowKind::Seq(seq, *r)
            }
            (_, _) => {
                let result = self.get_flow_result(f2_id);
                let result = self.new_result(result);
                FlowKind::Seq(vec![*f1_id, *f2_id], result)
            }
        };
    }*/

    pub fn get(&self, id: &VerticeId) -> Option<&Flow> {
        self.flows.get(*id)
    }

    pub fn get_mut(&mut self, id: &VerticeId) -> Option<&mut Flow> {
        self.flows.get_mut(*id)
    }

    /*
    Dot export
     */

    pub fn export_flow(&self, id: &FlowId) -> (Dot, (FlowId, FlowId)) {
        let sym_table = &self.sym_table;
        let mut dot = "".to_string();
        let mut start = None;
        let end;

        match &self.flows[*id].kind {
            FlowKind::Assignment(v) => {
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- {}\"];\n",
                        v.interval.format(sym_table, false),
                        v.result.format(sym_table, false),
                        v.lit.format(sym_table, false),
                    )
                    .as_str(),
                );
                start = Some(*id);
                end = Some(*id);
            }
            FlowKind::Branching(branching) => {
                write!(
                    dot,
                    "subgraph cluster_{id} {{\n
                    label = \"branching_{id}\";
                    color=blue;
                    \n"
                );
                let (cond_dot, (cond_start, cond_end)) = self.export_flow(&branching.cond_flow);
                let cond = self
                    .sym_table
                    .format_variable(&self.get_flow_result(&cond_end));
                start = Some(cond_start);

                let (true_dot, (true_start, true_end)) = self.export_flow(&branching.true_flow);
                let (false_dot, (false_start, false_end)) = self.export_flow(&branching.false_flow);
                let (result_dot, (result_start, result_end)) = self.export_flow(&branching.result);
                write!(dot, "{cond_dot}{true_dot}{false_dot}{result_dot}").unwrap();
                write!(dot, "V{cond_end} -> V{true_start} [label = \"{cond}\"];\n",).unwrap();
                write!(
                    dot,
                    "V{cond_end} -> V{false_start} [label = \"!{cond}\"];\n",
                )
                .unwrap();
                write!(dot, "V{true_end} -> V{result_start};\n").unwrap();
                write!(dot, "V{false_end}-> V{result_start};\n").unwrap();
                end = Some(result_end);
                write!(dot, "}}\n");
            }
            FlowKind::Seq(seq, r) => {
                write!(
                    dot,
                    "subgraph cluster_{id} {{\n
                    label = \"seq_{id}\";
                    color=black;
                    \n"
                );
                let mut previous_end = None;
                let mut seq = seq.clone();
                seq.push(*r);
                for f in &seq {
                    let (f_dot, (f_start, f_end)) = self.export_flow(&f);
                    write!(dot, "{}", f_dot).unwrap();
                    if let Some(end) = previous_end {
                        write!(dot, "V{end} -> V{f_start};\n").unwrap();
                    }
                    if start == None {
                        start = Some(f_start)
                    }
                    previous_end = Some(f_end);
                }
                end = previous_end;
                write!(dot, "}}\n");
            }
            FlowKind::FlowResult(fr) => {
                dot.push_str(
                    format!("V{id} [label= \"{}\"];\n", fr.format(sym_table, false),).as_str(),
                );
                start = Some(*id);
                end = Some(*id);
            }
        }

        (dot, (start.unwrap(), end.unwrap()))
    }

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        write!(dot, "{}", self.export_flow(&self.flow).0).unwrap();
        for (id, handle) in self.handles.inner() {
            let (f_dot, (start, _end)) = self.export_flow(&handle.flow);
            write!(dot, "{} -> V{start};\n", self.sym_table.format_variable(id)).unwrap();
            write!(dot, "{f_dot}").unwrap();
        }

        dot.push('}');
        dot
    }

    pub fn flat_bindings(&mut self) {
        let st = &self.sym_table.clone();
        for f in &mut self.flows {
            match &mut f.kind {
                FlowKind::Assignment(v) => {
                    v.result.flat_bindings(st);
                    v.lit.flat_bindings(st);
                    v.interval.flat_bindings(st);
                }
                FlowKind::FlowResult(fr) => {
                    fr.flat_bindings(st);
                }
                _ => {}
            }
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
