use crate::conversion::chronicle::interval::Interval;
use crate::conversion::chronicle::{FlatBindings, FormatWithSymTable, GetVariables};
use crate::conversion::flow_graph::assignment::Assignment;
use crate::conversion::flow_graph::flow::{BranchingFlow, Flow, FlowId, FlowKind, FlowPause};
use crate::sym_table::lit::Lit;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::VarId;
use crate::sym_table::TYPE_RESSOURCE_HANDLE;
use im::HashSet;
use std::collections::HashMap;
use std::fmt::Write;

pub type Dot = String;

pub type VerticeId = usize;
pub type HandleId = usize;

#[derive(Clone, Default)]
pub struct FlowGraph {
    pub st: RefSymTable,
    pub flows: Vec<Flow>,
    pub map_atom_id_flow_id: HashMap<VerticeId, Vec<FlowId>>,
    pub handles: HashMap<VarId, FlowId>,
    pub flow: FlowId,
}

impl FlowGraph {
    pub fn new(st: RefSymTable) -> Self {
        Self {
            st,
            flows: vec![],
            map_atom_id_flow_id: Default::default(),
            handles: Default::default(),
            flow: 0,
        }
    }

    pub fn get_handle(&self, atom: &VarId) -> Option<&FlowId> {
        let atom = self.st.get_var_parent(atom);
        let vec: Vec<&FlowId> = self
            .handles
            .iter()
            .filter_map(|(k, v)| {
                if self.st.get_var_parent(k) == atom {
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
        self.st.get_var_parent(&self.flows[*flow].result)
    }

    pub fn get_flow_interval(&self, flow: &FlowId) -> Interval {
        let i = &self.flows[*flow].interval;

        Interval::new(
            self.st.get_var_parent(&i.get_start()),
            self.st.get_var_parent(&i.get_end()),
        )
    }

    pub fn get_flow_start(&self, flow: &FlowId) -> VarId {
        let start = &self.flows[*flow].interval.get_start();

        self.st.get_var_parent(&start)
    }

    pub fn get_flow_end(&self, flow: &FlowId) -> VarId {
        let end = &self.flows[*flow].interval.get_end();

        self.st.get_var_parent(&end)
    }

    pub fn get_atom_of_flow(&self, flow: &FlowId) -> im::HashSet<VarId> {
        let flow = &self.flows[*flow];

        let mut set = match &flow.kind {
            FlowKind::Assignment(a) => a.get_variables(),
            FlowKind::Seq(seq) => {
                let mut set = im::HashSet::default();
                for f in seq {
                    set = set.union(self.get_atom_of_flow(f));
                }
                set
            }
            FlowKind::Branching(branching) => {
                let set = self.get_atom_of_flow(&branching.true_flow);
                let mut set = set.union(self.get_atom_of_flow(&branching.false_flow));
                set.insert(branching.cond_flow);
                set
            }
            FlowKind::FlowHandle(f) => self.get_atom_of_flow(&f),
            FlowKind::FlowPause(fw) => {
                let mut set: HashSet<VarId> = Default::default();
                if let Some(duration) = fw.duration {
                    set.insert(duration);
                }
                set
            }
            FlowKind::FlowResourceHandle(fa) => self.get_atom_of_flow(&fa),
        };

        set.insert(flow.result);
        set.insert(flow.interval.get_start());
        set.insert(flow.interval.get_end());

        set.iter().map(|a| self.st.get_var_parent(a)).collect()
    }

    pub fn new_instantaneous_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let interval = Interval::new_instantaneous(self.st.new_timepoint());
        let result = self.st.new_result();

        let vertice = Assignment { lit: value.into() };
        let id = self.new_flow(vertice, interval, result);
        self.new_seq(vec![id])
    }

    pub fn new_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let vertice = Assignment { lit: value.into() };
        let result = self.st.new_result();
        let interval = Interval::new(self.st.new_timepoint(), self.st.new_timepoint());
        let id = self.new_flow(vertice, interval, result);
        self.new_seq(vec![id])
    }

    pub fn new_seq(&mut self, seq: Vec<FlowId>) -> FlowId {
        assert!(!seq.is_empty());

        let flows = self.merge_flows(seq);
        let result = self.get_flow_result(flows.last().unwrap());
        let interval = Interval::new(
            self.get_flow_start(flows.first().unwrap()),
            self.get_flow_end(flows.last().unwrap()),
        );
        let kind = FlowKind::Seq(flows);
        self.new_flow(kind, interval, result)
    }

    pub fn new_branching(&mut self, branching: BranchingFlow) -> FlowId {
        let result = self.st.new_result();
        let interval = Interval::new(
            self.get_flow_start(&branching.cond_flow),
            self.st.new_timepoint(),
        );
        let id = self.new_flow(FlowKind::Branching(branching), interval, result);
        self.new_seq(vec![id])
    }

    pub fn new_async(&mut self, flow: FlowId) -> FlowId {
        let interval = Interval::new_instantaneous(self.st.new_timepoint());
        let result = self.st.new_handle();
        self.new_flow(FlowKind::FlowHandle(flow), interval, result)
    }

    pub fn new_wait(&mut self, duration: Option<VarId>) -> FlowId {
        let interval = Interval::new(self.st.new_timepoint(), self.st.new_timepoint());
        let result = self.st.new_nil();
        self.new_flow(
            FlowKind::FlowPause(FlowPause { duration }),
            interval,
            result,
        )
    }

    pub fn new_resource_handle(&mut self, flow: FlowId) -> FlowId {
        let interval = Interval::new_instantaneous(self.st.new_timepoint());
        let result = self.st.new_result();
        let domain_result = self.st.get_domain_id(&result);
        self.st.set_domain(
            &domain_result,
            self.st.get_type_as_domain(TYPE_RESSOURCE_HANDLE),
        );
        self.new_flow(FlowKind::FlowResourceHandle(flow), interval, result)
    }

    fn new_flow(
        &mut self,
        flow_kind: impl Into<FlowKind>,
        interval: Interval,
        result: VarId,
    ) -> FlowId {
        let id = self.flows.len();
        let kind = flow_kind.into();

        self.st.set_declaration(&result, &interval.get_end());
        let flow = Flow {
            valid: true,
            interval,
            result,
            parent: None,
            kind,
        };

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
            FlowKind::Seq(seq) => {
                for f in seq {
                    self.flows[*f].parent = Some(id);
                }
            }
            FlowKind::Branching(branching) => {
                self.flows[branching.false_flow].parent = Some(id);
                self.flows[branching.true_flow].parent = Some(id);
            } //FlowKind::Result(_, _) => unreachable!(),
            FlowKind::FlowHandle(h) => {
                self.flows[*h].parent = Some(id)
                //
            }
            FlowKind::FlowPause(fw) => {
                if let Some(duration) = fw.duration {
                    match self.map_atom_id_flow_id.get_mut(&duration) {
                        None => {
                            self.map_atom_id_flow_id.insert(duration, vec![id]);
                        }
                        Some(set) => {
                            set.push(id);
                        }
                    };
                }
            }
            FlowKind::FlowResourceHandle(rh) => self.flows[*rh].parent = Some(id),
        }
        self.flows.push(flow.into());
        id
    }

    pub fn update_flow(&mut self, id: &FlowId) {
        let kind = if let FlowKind::Seq(seq) = self.flows[*id].kind.clone() {
            FlowKind::Seq(self.merge_flows(seq))
        } else {
            self.flows[*id].kind.clone()
        };

        self.flows[*id].kind = kind
    }

    pub fn merge_flows(&mut self, flows: Vec<FlowId>) -> Vec<FlowId> {
        let mut seq = vec![];

        for flow_id in &flows {
            let flow = &self.flows[*flow_id];
            if let FlowKind::Seq(s) = &flow.kind {
                seq.append(&mut s.clone());
            } else {
                seq.push(*flow_id);
            }
        }

        seq
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
        let st = &self.st;
        let mut dot = "".to_string();
        let mut start = None;
        let end;
        let flow = &self.flows[*id];

        let color = match flow.valid {
            true => VALID_COLOR,
            false => INVALID_COLOR,
        };

        let flow = &self.flows[*id];
        let interval = &flow.interval;
        let result = flow.result;

        match &self.flows[*id].kind {
            FlowKind::Assignment(v) => {
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- {}\", color = {color}];\n",
                        interval.format(st, false),
                        result.format(st, false),
                        v.lit.format(st, false),
                    )
                    .as_str(),
                );
                start = Some(*id);
                end = Some(*id);
            }
            FlowKind::Branching(branching) => {
                let cond_result = self.get_flow_result(&branching.cond_flow);
                let val = self.st.get_domain_of_var(&cond_result);

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

                let cond = st.format_variable(&self.get_flow_result(&branching.cond_flow));
                let (cond_dot, (cond_start, cond_end)) = self.export_flow(&branching.cond_flow);
                start = Some(cond_start);

                let (true_dot, (true_start, true_end)) = self.export_flow(&branching.true_flow);
                let (false_dot, (false_start, false_end)) = self.export_flow(&branching.false_flow);
                //let (result_dot, (result_start, result_end)) = self.export_flow(&branching.result);

                write!(dot, "{cond_dot}{true_dot}{false_dot}").unwrap();
                write!(dot, "V{id} [label = \"{}\"]", result.format(st, false)).unwrap();
                write!(
                    dot,
                    "V{cond_end} -> V{true_start} [label = \"{cond}\",{color_branch_true}];\n",
                )
                .unwrap();
                write!(
                    dot,
                    "V{cond_end} -> V{false_start} [label = \"!{cond}\", {color_branch_false}];\n",
                )
                .unwrap();

                write!(
                    dot,
                    "V{true_end} -> V{id} [label = \"{cond}\",{color_branch_true}];\n",
                )
                .unwrap();
                write!(
                    dot,
                    "V{false_end} -> V{id} [label = \"!{cond}\", {color_branch_false}];\n",
                )
                .unwrap();
                end = Some(*id);
                write!(dot, "}}\n").unwrap();
            }
            FlowKind::Seq(seq) => {
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
                let seq = seq.clone();
                write!(dot, "V{id} [label = \"{}\"]", result.format(st, false)).unwrap();

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
                if let Some(end) = previous_end {
                    write!(dot, "V{end} -> V{id}").unwrap();
                } else {
                    start = Some(*id);
                }
                end = Some(*id);

                write!(dot, "}}\n").unwrap();
            }
            FlowKind::FlowHandle(handle) => {
                let (f_dot, (async_start, _)) = self.export_flow(&handle);
                write!(dot, "Handle_{handle} -> V{async_start};\n",).unwrap();
                write!(dot, "{f_dot}").unwrap();
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- handle({handle})\", color = {color}];\n",
                        interval.format(st, false),
                        result.format(st, false),
                    )
                    .as_str(),
                );

                start = Some(*id);
                end = Some(*id);
            }
            FlowKind::FlowPause(fw) => {
                match &fw.duration {
                    Some(duration) => {
                        write!(
                            dot,
                            "V{id} [label= \"{}:{}={}+{}\", color = {color}];\n",
                            interval.format(st, false),
                            interval.get_end().format(st, false),
                            interval.get_start().format(st, false),
                            duration.format(st, false),
                        )
                        .unwrap();
                    }
                    None => {
                        write!(
                            dot,
                            "V{id} [label= \"{}\", color = {color}];\n",
                            interval.format(st, false),
                        )
                        .unwrap();
                    }
                }
                start = Some(*id);
                end = Some(*id);
            }
            FlowKind::FlowResourceHandle(rh) => {
                let (f_dot, (handle_start, _)) = self.export_flow(&rh);
                write!(dot, "RH_{rh} -> V{handle_start};\n",).unwrap();
                write!(dot, "{f_dot}").unwrap();
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- resource-handle({rh})\", color = {color}];\n",
                        interval.format(st, false),
                        result.format(st, false),
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
        let st = &self.st.clone();
        for f in &mut self.flows {
            f.interval.flat_bindings(st);
            f.result.flat_bindings(st);
            match &mut f.kind {
                FlowKind::Assignment(v) => {
                    v.lit.flat_bindings(st);
                }
                FlowKind::FlowPause(fw) => {
                    if let Some(duration) = &mut fw.duration {
                        duration.flat_bindings(st)
                    }
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
pub const GOOD_BRANCH: &str = "color= green";
pub const NEUTRAL_BRANCH: &str = "color = black";
pub const BAD_BRANCH: &str = "color= red, style =dashed";
pub const VALID_COLOR: &str = "green";
pub const INVALID_COLOR: &str = "red";
