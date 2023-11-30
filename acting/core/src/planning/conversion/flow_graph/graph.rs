use crate::model::chronicle::acting_process_model::ActingProcessModelCollection;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::lit::{Lit, LitSet};
use crate::model::process_ref::Label;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, GetVariables};
use crate::model::sym_table::VarId;
use crate::planning::conversion::flow_graph::flow::{BranchingFlow, Flow, FlowId, FlowKind};
use new_type::newtype;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::ops::{Index, IndexMut};

pub type Dot = String;

newtype!(VerticeId: usize);

newtype!(HandleId: usize);

impl Index<FlowId> for Vec<Flow> {
    type Output = Flow;

    fn index(&self, index: FlowId) -> &Self::Output {
        &self[index.0]
    }
}

impl IndexMut<FlowId> for Vec<Flow> {
    fn index_mut(&mut self, index: FlowId) -> &mut Self::Output {
        &mut self[index.0]
    }
}

#[derive(Clone, Default)]
pub struct FlowGraph {
    pub st: RefSymTable,
    pub flows: Vec<Flow>,
    pub var_id_location: HashMap<VarId, Vec<FlowId>>,
    pub handles: HashMap<VarId, FlowId>,
    pub resource_handles: HashMap<VarId, VarId>,
    pub flow: FlowId,
    pub bindings: ActingProcessModelCollection,
}

impl FlowGraph {
    pub fn new(st: RefSymTable) -> Self {
        Self {
            st,
            flows: vec![],
            var_id_location: Default::default(),
            handles: Default::default(),
            resource_handles: Default::default(),
            flow: FlowId(0),
            bindings: Default::default(),
        }
    }

    pub fn get_handle(&self, atom: VarId) -> Option<&FlowId> {
        let atom = self.st.get_var_parent(atom);
        let vec: Vec<&FlowId> = self
            .handles
            .iter()
            .filter_map(|(k, v)| {
                if self.st.get_var_parent(*k) == atom {
                    Some(v)
                } else {
                    None
                }
            })
            .collect();
        assert_eq!(vec.len(), 1);
        vec.first().cloned()
    }

    pub fn get_resource_handle(&self, atom: VarId) -> Option<VarId> {
        let atom = self.st.get_var_parent(atom);
        let vec: Vec<VarId> = self
            .resource_handles
            .iter()
            .filter_map(|(k, v)| {
                if self.st.get_var_parent(*k) == atom {
                    Some(*v)
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

    pub fn set_duration(&mut self, flow: FlowId, duration: VarId) {
        self.flows[flow].interval.set_duration(duration);
    }

    pub fn set_parent(&mut self, flow: FlowId, parent: FlowId) {
        self.flows[flow].parent = Some(parent)
    }

    pub fn get_flow_result(&self, flow: FlowId) -> VarId {
        self.st.get_var_parent(self.flows[flow].result)
    }

    pub fn get_flow_interval(&self, flow: FlowId) -> Interval {
        let mut i = self.flows[flow].interval;

        i.flat_bindings(&self.st);

        /*Interval::new(
            self.st.get_var_parent(&i.get_start()),
            self.st.get_var_parent(&i.get_end()),
        )*/
        i
    }

    pub fn get_flow_start(&self, flow: FlowId) -> VarId {
        let start = self.flows[flow].interval.get_start();

        self.st.get_var_parent(start)
    }

    pub fn get_flow_end(&self, flow: FlowId) -> VarId {
        let end = self.flows[flow].interval.get_end();
        self.st.get_var_parent(end)
    }

    pub fn try_get_last_flow(&self, flow: FlowId) -> Option<FlowId> {
        match &self.flows[flow].kind {
            FlowKind::Lit(_) => Some(flow),
            FlowKind::Seq(s) => s.last().copied(),
            _ => None,
        }
    }

    pub fn try_get_flow_lit(&self, flow: FlowId) -> Option<Lit> {
        match &self.flows[flow].kind {
            FlowKind::Lit(lit) => Some(lit.clone()),
            FlowKind::Seq(vec) => {
                if vec.len() == 1 {
                    self.try_get_flow_lit(vec[0])
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn get_atom_of_flow(&self, flow: FlowId) -> std::collections::HashSet<VarId> {
        let flow = &self.flows[flow];

        let mut set = match &flow.kind {
            FlowKind::Lit(lit) => lit.get_variables(),
            FlowKind::Seq(seq) => {
                let mut set = std::collections::HashSet::default();
                for f in seq {
                    set = set.union(&self.get_atom_of_flow(*f)).cloned().collect();
                }
                set
            }
            FlowKind::Branching(branching) => {
                let set = self.get_atom_of_flow(branching.true_flow);
                let set: HashSet<VarId> = set
                    .union(&self.get_atom_of_flow(branching.false_flow))
                    .cloned()
                    .collect();
                let set: HashSet<VarId> = set
                    .union(&self.get_atom_of_flow(branching.cond_flow))
                    .cloned()
                    .collect();
                set
            }
            FlowKind::FlowHandle(f) => self.get_atom_of_flow(*f),
        };

        set.insert(flow.result);
        set.insert(flow.interval.get_start());
        set.insert(flow.interval.get_end());

        set.iter().map(|a| self.st.get_var_parent(*a)).collect()
    }

    pub fn new_instantaneous_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let interval = Interval::new_instantaneous(self.st.new_timepoint());
        let vertice = value.into();
        let result = self.st.new_result();

        let id = self.new_flow(vertice, interval, result);
        self.new_seq(vec![id])
    }

    pub fn new_arbitrary(&mut self, set: impl Into<LitSet>) {
        let interval = Interval::new_instantaneous(self.st.new_timepoint());
        let vertice: Lit = Lit::Constraint(Box::new(Constraint::arbitrary(set)));
        let result = self.st.new_arbitrary();
        self.new_flow(vertice, interval, result);
    }

    pub fn new_assignment(&mut self, value: impl Into<Lit>) -> FlowId {
        let vertice = value.into();
        let result = self.st.new_result();
        let (start, end) = if vertice.is_exec() {
            let start = self.st.new_timepoint();
            let start_task = self.st.new_start_task(start);
            if !self.st.union_var(start, start_task).is_none() {
                panic!();
            }
            (start_task, self.st.new_end_task(start_task))
        } else {
            (self.st.new_timepoint(), self.st.new_timepoint())
        };
        let interval = Interval::new(start, end);
        self.new_flow(vertice, interval, result)
    }

    pub fn new_seq(&mut self, seq: Vec<FlowId>) -> FlowId {
        assert!(!seq.is_empty());

        let flows = self.merge_flows(seq);
        let result = self.get_flow_result(*flows.last().unwrap());
        let interval = Interval::new(
            self.get_flow_start(*flows.first().unwrap()),
            self.get_flow_end(*flows.last().unwrap()),
        );
        let kind = FlowKind::Seq(flows);
        self.new_flow(kind, interval, result)
    }

    pub fn new_branching(&mut self, branching: BranchingFlow) -> FlowId {
        let result = self.st.new_result();
        let interval = Interval::new(
            self.get_flow_start(branching.cond_flow),
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

    fn new_flow(
        &mut self,
        flow_kind: impl Into<FlowKind>,
        interval: Interval,
        result: VarId,
    ) -> FlowId {
        let id = FlowId(self.flows.len());
        let kind = flow_kind.into();

        let mut vars = vec![result, interval.get_start(), interval.get_end()];

        self.st.set_declaration(result, interval.get_end());
        let flow = Flow {
            valid: true,
            interval,
            result,
            parent: None,
            label: None,
            kind,
        };

        match &flow.kind {
            FlowKind::Lit(lit) => vars.append(&mut lit.get_variables().iter().cloned().collect()),
            FlowKind::Seq(seq) => {
                for f in seq {
                    self.flows[*f].parent = Some(id);
                }
            }
            FlowKind::Branching(branching) => {
                self.flows[branching.false_flow].parent = Some(id);
                self.flows[branching.true_flow].parent = Some(id);
            }
            FlowKind::FlowHandle(h) => self.flows[*h].parent = Some(id),
        }
        for v in vars {
            match self.var_id_location.get_mut(&v) {
                None => {
                    self.var_id_location.insert(v, vec![id]);
                }
                Some(set) => {
                    set.push(id);
                }
            };
        }
        self.flows.push(flow);
        id
    }

    pub fn set_label(&mut self, id: &FlowId, label: Label) {
        self.flows[*id].label = Some(label)
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

    pub fn get(&self, id: FlowId) -> Option<&Flow> {
        self.flows.get(id.0)
    }

    pub fn get_mut(&mut self, id: &FlowId) -> Option<&mut Flow> {
        self.flows.get_mut(id.0)
    }

    /*
    Dot export
     */

    pub fn export_flow(&self, id: FlowId) -> (Dot, (FlowId, FlowId)) {
        let st = &self.st;
        let mut dot = "".to_string();
        let mut start = None;
        let end;
        let flow = &self.flows[id];

        let color = match flow.valid {
            true => VALID_COLOR,
            false => INVALID_COLOR,
        };

        let flow = &self.flows[id];
        let interval = &flow.interval;
        let result = flow.result;

        match &self.flows[id].kind {
            FlowKind::Lit(lit) => {
                dot.push_str(
                    format!(
                        "V{id} [label= \"{}: {} <- {}\", color = {color}];\n",
                        interval.format(st, false),
                        result.format(st, false),
                        lit.format(st, false),
                    )
                    .as_str(),
                );
                start = Some(id);
                end = Some(id);
            }
            FlowKind::Branching(branching) => {
                let cond_result = self.get_flow_result(branching.cond_flow);
                let val = self.st.get_domain_of_var(cond_result);

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

                let cond = st.format_variable(self.get_flow_result(branching.cond_flow));
                let (cond_dot, (cond_start, cond_end)) = self.export_flow(branching.cond_flow);
                start = Some(cond_start);

                let (true_dot, (true_start, true_end)) = self.export_flow(branching.true_flow);
                let (false_dot, (false_start, false_end)) = self.export_flow(branching.false_flow);
                //let (result_dot, (result_start, result_end)) = self.export_flow(&branching.result);

                write!(dot, "{cond_dot}{true_dot}{false_dot}").unwrap();
                write!(dot, "V{id} [label = \"{}\"]", result.format(st, false)).unwrap();
                writeln!(
                    dot,
                    "V{cond_end} -> V{true_start} [label = \"{cond}\",{color_branch_true}];",
                )
                .unwrap();
                writeln!(
                    dot,
                    "V{cond_end} -> V{false_start} [label = \"!{cond}\", {color_branch_false}];",
                )
                .unwrap();

                writeln!(
                    dot,
                    "V{true_end} -> V{id} [label = \"{cond}\",{color_branch_true}];",
                )
                .unwrap();
                writeln!(
                    dot,
                    "V{false_end} -> V{id} [label = \"!{cond}\", {color_branch_false}];",
                )
                .unwrap();
                end = Some(id);
                writeln!(dot, "}}").unwrap();
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
                    let (f_dot, (f_start, f_end)) = self.export_flow(*f);
                    write!(dot, "{}", f_dot).unwrap();
                    if let Some(end) = previous_end {
                        writeln!(dot, "V{end} -> V{f_start} [color = {color}];").unwrap();
                    }
                    if start.is_none() {
                        start = Some(f_start)
                    }
                    previous_end = Some(f_end);
                }
                if let Some(end) = previous_end {
                    write!(dot, "V{end} -> V{id}").unwrap();
                } else {
                    start = Some(id);
                }
                end = Some(id);

                writeln!(dot, "}}").unwrap();
            }
            FlowKind::FlowHandle(handle) => {
                let (f_dot, (async_start, _)) = self.export_flow(*handle);
                writeln!(dot, "Handle_{handle} -> V{async_start};",).unwrap();
                write!(dot, "{f_dot}").unwrap();
                writeln!(
                    dot,
                    "V{id} [label= \"{}: {} <- handle({handle})\", color = {color}];",
                    interval.format(st, false),
                    result.format(st, false),
                )
                .unwrap();

                start = Some(id);
                end = Some(id);
            }
        }

        (dot, (start.unwrap(), end.unwrap()))
    }

    pub fn export_dot(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();

        write!(dot, "{}", self.export_flow(self.flow).0).unwrap();

        dot.push('}');
        dot
    }

    pub fn flat_bindings(&mut self) {
        let st = &self.st.clone();
        for f in &mut self.flows {
            f.interval.flat_bindings(st);
            f.result.flat_bindings(st);
            if let FlowKind::Lit(lit) = &mut f.kind {
                lit.flat_bindings(st);
            }
        }
    }
}

#[cfg(feature = "conversion_data")]
impl FlowGraph {
    pub fn n_nodes(&self, flow_id: FlowId) -> usize {
        let mut n = 0;
        let mut queue = vec![flow_id];
        while let Some(id) = queue.pop() {
            let flow = &self.flows[id];
            match &flow.kind {
                FlowKind::Lit(_) => {
                    n += 1;
                }
                FlowKind::Seq(s) => {
                    queue.append(&mut s.clone());
                }
                FlowKind::Branching(b) => {
                    queue.push(b.cond_flow);
                    let cond = self.get_flow_result(b.cond_flow);
                    let cond_domain = self.st.get_domain_of_var(cond);
                    if cond_domain.is_true() {
                        queue.push(b.true_flow)
                    } else if cond_domain.is_false() {
                        queue.push(b.false_flow)
                    } else {
                        queue.push(b.true_flow);
                        queue.push(b.false_flow);
                    }
                }
                FlowKind::FlowHandle(h) => queue.push(*h),
            }
        }
        n
    }
    pub fn n_branching(&self, flow_id: FlowId) -> usize {
        let mut n = 0;
        let mut queue = vec![flow_id];
        while let Some(id) = queue.pop() {
            let flow = &self.flows[id];
            match &flow.kind {
                FlowKind::Lit(_) => {}
                FlowKind::Seq(s) => {
                    queue.append(&mut s.clone());
                }
                FlowKind::Branching(b) => {
                    queue.push(b.cond_flow);
                    let cond = self.get_flow_result(b.cond_flow);
                    let cond_domain = self.st.get_domain_of_var(cond);
                    if cond_domain.is_true() {
                        queue.push(b.true_flow)
                    } else if cond_domain.is_false() {
                        queue.push(b.false_flow)
                    } else {
                        n += 1;
                        queue.push(b.true_flow);
                        queue.push(b.false_flow);
                    }
                }
                FlowKind::FlowHandle(h) => queue.push(*h),
            }
        }
        n
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
