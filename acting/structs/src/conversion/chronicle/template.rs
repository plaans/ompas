use crate::conversion::chronicle::condition::Condition;
use crate::conversion::chronicle::constraint::Constraint;
use crate::conversion::chronicle::effect::Effect;
use crate::conversion::chronicle::interval::Interval;
use crate::conversion::chronicle::subtask::SubTask;
use crate::conversion::chronicle::task_template::TaskTemplate;
use crate::conversion::flow_graph::graph::FlowGraph;
use crate::sym_table::domain::basic_type::BasicType;
use crate::sym_table::domain::Domain;
use crate::sym_table::lit::Lit;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::sym_table::VarId;
use im::hashset::HashSet;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::time::Duration;

#[derive(Copy, Clone)]
pub enum ChronicleKind {
    Command,
    Method,
    Task,
}

#[derive(Clone)]
pub struct ChronicleDebug {
    pub kind: ChronicleKind,
    label: String,
    pub lvalue: LValue,
    pub post_processed_lvalue: LValue,
    pub flow_graph: FlowGraph,
    pub convert_time: Duration,
}

#[derive(Clone)]
pub struct ChronicleTemplate {
    pub st: RefSymTable,
    name: Vec<VarId>,
    pub task: Vec<VarId>,
    presence: VarId,
    interval: Interval,
    result: VarId,
    pub variables: HashSet<VarId>,
    pub constraints: Vec<Constraint>,
    conditions: Vec<Condition>,
    effects: Vec<Effect>,
    subtasks: Vec<SubTask>,
    pub debug: ChronicleDebug,
    pub syntactic_chronicles: Vec<TaskTemplate>,
}

impl ChronicleTemplate {
    pub fn new(label: impl Display, kind: ChronicleKind, st: RefSymTable) -> Self {
        let interval = Interval::new(st.new_start(), st.new_end());

        let presence = st.new_presence();

        let result = st.new_chronicle_result();

        let init_var = vec![presence, result, interval.get_start(), interval.get_end()];

        let mut chronicle = Self {
            st,
            debug: ChronicleDebug {
                kind,
                label: label.to_string(),
                lvalue: Default::default(),
                post_processed_lvalue: Default::default(),
                flow_graph: Default::default(),
                convert_time: Duration::from_secs(0),
            },
            name: Default::default(),
            task: Default::default(),
            presence,
            interval,
            result,
            variables: Default::default(),
            constraints: vec![],
            conditions: vec![],
            effects: vec![],
            subtasks: vec![],
            syntactic_chronicles: vec![],
        };
        for v in init_var {
            chronicle.add_var(v);
        }

        chronicle
    }

    /*
    GETTERS
     */
    pub fn get_presence(&self) -> &VarId {
        &self.presence
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_result(&self) -> &VarId {
        &self.result
    }

    pub fn get_result_as_lit(&self) -> Lit {
        self.result.borrow().into()
    }

    pub fn get_constraints(&self) -> &Vec<Constraint> {
        &self.constraints
    }

    pub fn get_conditions(&self) -> &Vec<Condition> {
        &self.conditions
    }

    pub fn get_effects(&self) -> &Vec<Effect> {
        &self.effects
    }

    pub fn get_subtasks(&self) -> &Vec<SubTask> {
        &self.subtasks
    }

    fn build_hashset<T: GetVariables>(vec: &[T]) -> im::HashSet<VarId> {
        let mut hashset: HashSet<VarId> = Default::default();
        for e in vec {
            hashset = hashset.union(e.get_variables());
        }

        hashset
    }

    pub fn get_variables_in_set(&self, set: ChronicleSet) -> im::HashSet<VarId> {
        match set {
            ChronicleSet::Effect => Self::build_hashset(&self.effects),
            ChronicleSet::Constraint => Self::build_hashset(&self.constraints),
            ChronicleSet::Condition => Self::build_hashset(&self.conditions),
            ChronicleSet::SubTask => Self::build_hashset(&self.subtasks),
        }
    }

    pub fn get_variables_in_sets(&self, sets: Vec<ChronicleSet>) -> im::HashSet<VarId> {
        let mut hashset = HashSet::default();
        for set in sets {
            hashset = hashset.union(self.get_variables_in_set(set))
        }
        hashset
    }

    pub fn get_all_variables_in_sets(&self) -> im::HashSet<VarId> {
        self.get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Constraint,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
    }

    pub fn get_symbol_variables(&self, sym_table: &RefSymTable) -> HashSet<VarId> {
        let variables = self.get_variables();
        variables
            .iter()
            .filter(|a| {
                sym_table
                    .contained_in_domain(&sym_table.get_domain_of_var(a), &BasicType::Symbol.into())
            })
            .cloned()
            .collect()
    }

    pub fn get_debug(&self) -> &ChronicleDebug {
        &self.debug
    }

    pub fn get_name(&self) -> &Vec<VarId> {
        &self.name
    }

    pub fn get_task(&self) -> &Vec<VarId> {
        &self.task
    }

    /*
    REMOVERS
     */
    pub fn rm_var(&mut self, sym_id: &VarId) {
        self.variables.remove(sym_id);
    }

    pub fn rm_set_var(&mut self, ids: Vec<VarId>) {
        for id in ids {
            self.rm_var(&id);
        }
    }

    pub fn rm_constraint(&mut self, index: usize) {
        self.constraints.remove(index);
    }

    pub fn rm_condition(&mut self, index: usize) {
        self.conditions.remove(index);
    }

    pub fn rm_set_constraint(&mut self, mut indexes: Vec<usize>) {
        indexes.sort_unstable();
        indexes.reverse();
        for index in indexes {
            self.rm_constraint(index);
        }
    }

    /*
    ADDERS
     */
    pub fn add_var(&mut self, sym_id: VarId) {
        self.variables.insert(sym_id);
    }

    pub fn add_interval(&mut self, interval: Interval) {
        self.variables.insert(interval.get_start());
        self.variables.insert(interval.get_end());
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        for var in constraint.get_variables() {
            self.add_var(var)
        }

        /*if let Constraint::And(vec) = constraint {
            self.constraints.push(a.deref().clone());
            self.constraints.push(b.deref().clone());
        } else {

        }*/
        self.constraints.push(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        for var in cond.get_variables() {
            self.add_var(var);
        }

        self.conditions.push(cond);
    }

    pub fn add_effect(&mut self, effect: Effect) {
        for var in effect.get_variables() {
            self.add_var(var);
        }
        self.effects.push(effect);
    }

    pub fn add_subtask(&mut self, sub_task: SubTask) {
        for var in sub_task.get_variables() {
            self.add_var(var);
        }
        self.subtasks.push(sub_task);
    }

    pub fn add_task_template(&mut self, task: TaskTemplate) {
        self.syntactic_chronicles.push(task)
    }

    pub fn add_task_parameter(&mut self, param: &VarId) {
        self.task.push(*param)
    }
    pub fn add_method_parameter(&mut self, param: &VarId) {
        self.name.push(*param)
    }

    /*
    SETTERS
     */

    pub fn set_name(&mut self, name: Vec<VarId>) {
        self.name = name;
    }

    pub fn set_task(&mut self, task: Vec<VarId>) {
        self.task = task;
    }

    /*
    FORMATTERS
     */

    pub fn format(&self, sym_version: bool) -> String {
        let st = &self.st;
        let mut s = String::new();
        //name
        s.push_str(
            format!(
                "{}:\n {}: {} {}",
                self.debug.label.as_str(),
                self.presence.format(st, sym_version),
                self.interval.format(st, sym_version),
                self.result.format(st, sym_version)
            )
            .as_str(),
        );
        s.push('\n');
        s.push_str(format!("- name: {}\n", self.name.format(st.borrow(), sym_version)).as_str());
        //task
        s.push_str(format!("- task: {}\n", self.task.format(st.borrow(), sym_version)).as_str());
        s.push_str("-variable(s): {");

        let mut variables = self
            .variables
            .iter()
            .map(|id| {
                format!(
                    "{}({})",
                    id.format(st, sym_version),
                    st.format_domain_id(&st.get_domain_id(id)),
                )
            })
            .collect::<Vec<String>>();
        variables.sort();

        for (i, sym) in variables.iter().enumerate() {
            if i != 0 {
                s.push(',');
            }
            s.push_str(sym);
        }
        s.push_str("}\n");

        s.push_str("-constraint(s): {");
        for c in &self.constraints {
            write!(s, "\n\t{}", c.format(st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");

        //conditions
        s.push_str("-conditon(s): {");
        for c in &self.conditions {
            write!(s, "\n\t{}", c.format(st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");
        //effects
        s.push_str("-effect(s): {");
        for e in &self.effects {
            write!(s, "\n\t{}", e.format(st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {");
        for e in &self.subtasks {
            write!(s, "\n\t{}", e.format(st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");
        s.push_str("-synthetic task(s): {\n");
        for t in &self.syntactic_chronicles {
            writeln!(s, "task: {}", t.name.format(st, sym_version)).unwrap();
            writeln!(s, "methods:").unwrap();
            for m in &t.methods {
                write!(s, "{}", m).unwrap();
            }
        }
        s.push_str("}\n");

        //Debug
        /*if let Some(exp) = &self.debug {
            s.push_str(format!("debug: {}", exp.format("debug: ".len())).as_str());
        }*/

        s
    }

    pub fn flat_bindings(&mut self) {
        let st = &self.st;
        self.name.flat_bindings(st);
        self.task.flat_bindings(st);
        let old_variables = self.variables.clone();
        let mut new_variables: HashSet<VarId> = Default::default();
        for v in &old_variables {
            let mut v = *v;
            v.flat_bindings(st.borrow());
            new_variables.insert(v);
        }

        self.variables = new_variables;
        self.interval.flat_bindings(st);
        self.result.flat_bindings(st);
        self.presence.flat_bindings(st);
        self.constraints.flat_bindings(st);
        self.conditions.flat_bindings(st);
        self.effects.flat_bindings(st);
        self.subtasks.flat_bindings(st);
    }
}

impl GetVariables for ChronicleTemplate {
    fn get_variables(&self) -> HashSet<VarId> {
        self.variables.clone()
    }

    fn get_variables_in_domain(&self, sym_table: &RefSymTable, domain: &Domain) -> HashSet<VarId> {
        self.variables
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
            .cloned()
            .collect()
    }
}

impl Replace for ChronicleTemplate {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.variables.remove(old);
        self.variables.insert(*new);
        self.conditions.replace(old, new);
        self.constraints.replace(old, new);
        self.subtasks.replace(old, new);
        self.effects.replace(old, new);
    }
}

impl Display for ChronicleTemplate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format(true))
    }
}

pub enum ChronicleSet {
    Effect,
    Constraint,
    Condition,
    SubTask,
}
