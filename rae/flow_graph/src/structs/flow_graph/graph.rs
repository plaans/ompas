use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::{FlatBindings, FormatWithSymTable, GetVariables};
use crate::structs::flow_graph::assignment::Assignment;
use crate::structs::flow_graph::flow::{
    BranchingFlow, Flow, FlowAsync, FlowId, FlowKind, FlowResult,
};
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::VarId;
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
    pub(crate) handles: HashMap<VarId, FlowId>,
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

    pub fn get_handle(&self, atom: &VarId) -> Option<&FlowId> {
        let atom = self.sym_table.get_var_parent(atom);
        let vec: Vec<&FlowId> = self
            .handles
            .iter()
            .filter_map(|(k, v)| {
                if self.sym_table.get_var_parent(k) == atom {
                    Some(v)
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(vec.len(), 1);
        vec.first().cloned()
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

    pub fn set_kind(&mut self, flow: &FlowId, kind: FlowKind) {
        self.flows[*flow].kind = kind
    }

    pub fn get_parent(&self, flow: &FlowId) -> &Option<FlowId> {
        &self.flows[*flow].parent
    }

    pub fn set_parent(&mut self, flow: &FlowId, parent: &FlowId) {
        self.flows[*flow].parent = Some(*parent)
    }

    pub fn get_flow_result(&self, flow: &FlowId) -> VarId {
        let flow = &self.flows[*flow];
        let r = match &flow.kind {
            FlowKind::Assignment(v) => v.result,
            FlowKind::Seq(_, r) => self.get_flow_result(r),
            FlowKind::Branching(branching) => self.get_flow_result(&branching.result),
            FlowKind::FlowResult(fr) => fr.result,
            FlowKind::FlowAsync(f) => f.result,
        };
        self.sym_table.get_var_parent(&r)
    }

    pub fn get_flow_interval(&self, flow: &FlowId) -> Interval {
        let flow = &self.flows[*flow];
        let mut i = match &flow.kind {
            FlowKind::Assignment(v) => v.interval,
            FlowKind::Seq(seq, r) => {
                let start = match seq.first() {
                    Some(first) => self.get_flow_start(first),
                    None => self.get_flow_start(r),
                };
                let end = self.get_flow_end(r);
                Interval::new(&start, &end)
            }
            FlowKind::Branching(branching) => {
                let start = *self.get_flow_interval(&branching.cond).get_start();
                let end = *self.get_flow_interval(&branching.result).get_end();
                Interval::new(&start, &end)
            }
            FlowKind::FlowResult(r) => Interval::new_instantaneous(&r.timepoint),
            FlowKind::FlowAsync(f) => Interval::new_instantaneous(&f.timepoint),
        };

        Interval::new(
            &self.sym_table.get_var_parent(&i.get_start()),
            &self.sym_table.get_var_parent(&i.get_end()),
        )
    }

    pub fn get_flow_start(&self, flow: &FlowId) -> VarId {
        let flow = &self.flows[*flow];
        let s = match &flow.kind {
            FlowKind::Assignment(v) => *v.interval.get_start(),
            FlowKind::Seq(seq, r) => match seq.first() {
                Some(first) => self.get_flow_start(first),
                None => self.get_flow_start(r),
            },
            FlowKind::Branching(branching) => self.get_flow_start(&branching.cond),
            FlowKind::FlowResult(r) => r.timepoint,
            FlowKind::FlowAsync(f) => f.timepoint,
        };

        self.sym_table.get_var_parent(&s)
    }

    pub fn get_flow_end(&self, flow: &FlowId) -> VarId {
        let flow = &self.flows[*flow];
        let e = match &flow.kind {
            FlowKind::Assignment(v) => *v.interval.get_end(),
            FlowKind::Seq(_, r) => self.get_flow_end(r),
            FlowKind::Branching(branching) => self.get_flow_end(&branching.result),
            FlowKind::FlowResult(r) => r.timepoint,
            FlowKind::FlowAsync(f) => f.timepoint,
        };

        self.sym_table.get_var_parent(&e)
    }

    pub fn get_atom_of_flow(&self, flow: &FlowId) -> im::HashSet<VarId> {
        let flow = &self.flows[*flow];

        let set = match &flow.kind {
            FlowKind::Assignment(a) => a.get_variables(),
            FlowKind::Seq(seq, _) => {
                let mut set = im::HashSet::default();
                for f in seq {
                    set = set.union(self.get_atom_of_flow(f));
                }
                set
            }
            FlowKind::Branching(branching) => {
                let mut set = self.get_atom_of_flow(&branching.cond);
                set = set.union(self.get_atom_of_flow(&branching.result));
                set
            }
            FlowKind::FlowResult(fr) => hashset![fr.result, fr.timepoint],
            FlowKind::FlowAsync(f) => {
                let set = hashset![f.result, f.timepoint];
                set.union(self.get_atom_of_flow(&f.flow))
            }
        };

        set.iter()
            .map(|a| self.sym_table.get_var_parent(a))
            .collect()
    }

    pub fn new_instantaneous_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let t = self.sym_table.new_timepoint();
        let r = self.sym_table.new_result();

        self.sym_table.set_declaration(&r, &t);

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

        self.sym_table.set_declaration(&r, &end);

        let vertice = Assignment {
            interval: Interval::new(&start, &end),
            result: r,
            lit: value.into(),
        };
        let id = self.new_flow(vertice);
        self.new_seq(vec![id])
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

    pub fn new_result(&mut self, result: VarId, timepoint: Option<VarId>) -> FlowId {
        let timepoint = timepoint.unwrap_or(self.sym_table.new_timepoint());

        self.new_flow(FlowKind::FlowResult(FlowResult { result, timepoint }))
    }

    pub fn new_async(&mut self, flow: FlowId) -> FlowId {
        let timepoint = self.sym_table.new_timepoint();
        let result = self.sym_table.new_handle();
        self.new_flow(FlowKind::FlowAsync(FlowAsync {
            result,
            timepoint,
            flow,
        }))
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
                self.flows[branching.cond].parent = Some(id);
                self.flows[branching.false_flow].parent = Some(id);
                self.flows[branching.true_flow].parent = Some(id);
                self.flows[branching.result].parent = Some(id);
            } //FlowKind::Result(_, _) => unreachable!(),
            FlowKind::FlowResult(fr) => {
                let var = vec![fr.timepoint, fr.result];

                for v in var {
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
            FlowKind::FlowAsync(a) => {
                self.flows[a.flow].parent = Some(id)
                //
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
                    result = self.new_result(
                        self.get_flow_result(flow_id),
                        Some(self.get_flow_end(flow_id)),
                    );
                }
            }
        }

        /*let mut previous_end: Option<AtomId> = None;

        for f in &seq {
            if let Some(prev) = previous_end {
                self.sym_table.union_atom(&prev, &self.get_flow_start(&f));
            }

            previous_end = Some(self.get_flow_end(&f))
        }*/

        FlowKind::Seq(seq, result)
    }

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
        let flow = &self.flows[*id];

        let color = match flow.valid {
            true => VALID_COLOR,
            false => INVALID_COLOR,
        };

        match &self.flows[*id].kind {
            FlowKind::Assignment(v) => {
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- {}\", color = {color}];\n",
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
                let cond_result = self.get_flow_result(&branching.cond);
                let val = self.sym_table.get_domain_of_var(&cond_result);

                let branch = if val.is_true() {
                    Some(true)
                } else if val.is_false() {
                    Some(false)
                } else {
                    None
                };

                let (_label, color_branch_true, color_branch_false) = match branch {
                    None => (format!("branching_{id}"), NEUTRAL_BRANCH, NEUTRAL_BRANCH),
                    Some(true) => (
                        format!("branching_{id}(branch = true)"),
                        GOOD_BRANCH,
                        BAD_BRANCH,
                    ),
                    Some(false) => (
                        format!("branching_{id}(branch = false)",),
                        BAD_BRANCH,
                        GOOD_BRANCH,
                    ),
                };

                /*write!(
                    dot,
                    "subgraph cluster_{id} {{\n
                    label = \"{label}\";
                    color={color};
                    \n"
                );*/

                write!(
                    dot,
                    "subgraph cluster_{id} {{\n
                    \n"
                )
                .unwrap();

                let (cond_dot, (cond_start, cond_end)) = self.export_flow(&branching.cond);
                let cond = self
                    .sym_table
                    .format_variable(&self.get_flow_result(&cond_end));
                start = Some(cond_start);

                let (true_dot, (true_start, true_end)) = self.export_flow(&branching.true_flow);
                let (false_dot, (false_start, false_end)) = self.export_flow(&branching.false_flow);
                let (result_dot, (result_start, result_end)) = self.export_flow(&branching.result);
                write!(dot, "{cond_dot}{true_dot}{false_dot}{result_dot}").unwrap();
                write!(
                    dot,
                    "V{cond_end} -> V{true_start} [label = \"{cond}\", {color_branch_true}];\n",
                )
                .unwrap();
                write!(
                    dot,
                    "V{cond_end} -> V{false_start} [label = \"!{cond}\", {color_branch_false}];\n",
                )
                .unwrap();
                write!(
                    dot,
                    "V{true_end} -> V{result_start} [{color_branch_true}];\n"
                )
                .unwrap();
                write!(
                    dot,
                    "V{false_end}-> V{result_start} [{color_branch_false}];\n"
                )
                .unwrap();
                end = Some(result_end);
                write!(dot, "}}\n").unwrap();
            }
            FlowKind::Seq(seq, r) => {
                /*write!(
                    dot,
                    "subgraph cluster_{id} {{\n
                    label = \"seq_{id}\";
                    color={color};
                    \n"
                );*/

                write!(
                    dot,
                    "subgraph cluster_{id} {{\n
                    \n"
                )
                .unwrap();
                let mut previous_end = None;
                let mut seq = seq.clone();
                seq.push(*r);
                for f in &seq {
                    let (f_dot, (f_start, f_end)) = self.export_flow(&f);
                    write!(dot, "{}", f_dot).unwrap();
                    if let Some(end) = previous_end {
                        write!(dot, "V{end} -> V{f_start} [color = {color}];\n").unwrap();
                    }
                    if start == None {
                        start = Some(f_start)
                    }
                    previous_end = Some(f_end);
                }
                end = previous_end;
                write!(dot, "}}\n").unwrap();
            }
            FlowKind::FlowResult(fr) => {
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}:{}\", color = {color}];\n",
                        fr.timepoint.format(sym_table, false),
                        fr.result.format(sym_table, false),
                    )
                    .as_str(),
                );
                start = Some(*id);
                end = Some(*id);
            }
            FlowKind::FlowAsync(f) => {
                let async_flow = f.flow;
                let (f_dot, (async_start, _)) = self.export_flow(&async_flow);
                write!(dot, "Async_{async_flow} -> V{async_start};\n",).unwrap();
                write!(dot, "{f_dot}").unwrap();
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- async({async_flow})\", color = {color}];\n",
                        f.timepoint.format(sym_table, false),
                        f.result.format(sym_table, false),
                    )
                    .as_str(),
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
        /*for (id, handle) in self.handles.inner() {
            let (f_dot, (start, _end)) = self.export_flow(&handle.flow);
            write!(dot, "{} -> V{start};\n", self.sym_table.format_variable(id)).unwrap();
            write!(dot, "{f_dot}").unwrap();
        }*/

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
                    fr.timepoint.flat_bindings(st);
                    fr.result.flat_bindings(st);
                }
                FlowKind::Seq(_, _) => {}
                FlowKind::Branching(_) => {}
                FlowKind::FlowAsync(f) => {
                    f.result.flat_bindings(st);
                    f.timepoint.flat_bindings(st);
                }
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
pub const GOOD_BRANCH: &str = "color= green";
pub const NEUTRAL_BRANCH: &str = "color = black";
pub const BAD_BRANCH: &str = "color= red, style =dashed";
pub const VALID_COLOR: &str = "green";
pub const INVALID_COLOR: &str = "red";
