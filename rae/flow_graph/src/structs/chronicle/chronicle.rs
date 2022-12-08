use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::task_template::TaskTemplate;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::*;
use crate::structs::flow_graph::graph::FlowGraph;
use im::hashset::HashSet;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

pub enum ChronicleKind {
    Command,
    Method,
}

pub struct ChronicleDebug {
    pub kind: ChronicleKind,
    label: String,
    pub lvalue: LValue,
    pub flow_graph: FlowGraph,
}

pub struct ChronicleTemplate {
    pub sym_table: RefSymTable,
    name: Vec<AtomId>,
    task: Vec<AtomId>,
    presence: AtomId,
    interval: Interval,
    result: AtomId,
    pub(crate) variables: HashSet<AtomId>,
    constraints: Vec<Constraint>,
    conditions: Vec<Condition>,
    effects: Vec<Effect>,
    subtasks: Vec<SubTask>,
    pub debug: ChronicleDebug,
    pub syntactic_chronicles: Vec<TaskTemplate>,
}

impl ChronicleTemplate {
    pub fn new(
        label: impl Display,
        chronicle_kind: ChronicleKind,
        mut sym_table: RefSymTable,
    ) -> Self {
        let interval = Interval::new(&sym_table.new_start(), &sym_table.new_end());

        let presence = sym_table.new_presence();

        let result = sym_table.new_chronicle_result();

        let init_var = vec![presence, result, *interval.get_start(), *interval.get_end()];

        let mut chronicle = Self {
            sym_table,
            debug: ChronicleDebug {
                kind: chronicle_kind,
                label: label.to_string(),
                lvalue: Default::default(),
                flow_graph: Default::default(),
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
        for v in &init_var {
            chronicle.add_var(v);
        }

        chronicle
    }

    /*
    GETTERS
     */
    pub fn get_presence(&self) -> &AtomId {
        &self.presence
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_result(&self) -> &AtomId {
        &self.result
    }

    pub fn get_result_as_lit(&self) -> Lit {
        self.result.borrow().into()
    }

    pub fn get_constraints(&self) -> &Vec<Constraint> {
        &self.constraints
    }

    fn build_hashset<T: GetVariables>(vec: &[T]) -> im::HashSet<AtomId> {
        let mut hashset: HashSet<AtomId> = Default::default();
        for e in vec {
            hashset = hashset.union(e.get_variables());
        }

        hashset
    }

    pub fn get_variables_in_set(&self, set: ChronicleSet) -> im::HashSet<AtomId> {
        match set {
            ChronicleSet::Effect => Self::build_hashset(&self.effects),
            ChronicleSet::Constraint => Self::build_hashset(&self.constraints),
            ChronicleSet::Condition => Self::build_hashset(&self.conditions),
            ChronicleSet::SubTask => Self::build_hashset(&self.subtasks),
        }
    }

    pub fn get_variables_in_sets(&self, sets: Vec<ChronicleSet>) -> im::HashSet<AtomId> {
        let mut hashset = HashSet::default();
        for set in sets {
            hashset = hashset.union(self.get_variables_in_set(set))
        }
        hashset
    }

    pub fn get_all_variables_in_sets(&self) -> im::HashSet<AtomId> {
        self.get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Constraint,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
    }

    pub fn get_symbol_variables(&self, sym_table: &RefSymTable) -> HashSet<AtomId> {
        let variables = self.get_variables();
        variables
            .iter()
            .filter(|a| sym_table.get_type_of(a) == AtomType::Symbol)
            .cloned()
            .collect()
    }

    pub fn get_debug(&self) -> &ChronicleDebug {
        &self.debug
    }

    pub fn get_name(&self) -> &Vec<AtomId> {
        &self.name
    }

    pub fn get_task(&self) -> &Vec<AtomId> {
        &self.task
    }

    /*
    REMOVERS
     */
    pub fn rm_var(&mut self, sym_id: &AtomId) {
        self.variables.remove(sym_id);
    }

    pub fn rm_set_var(&mut self, ids: Vec<AtomId>) {
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
    pub fn add_var(&mut self, sym_id: &AtomId) {
        self.variables.insert(*sym_id);
    }

    pub fn add_interval(&mut self, interval: Interval) {
        self.variables.insert(*interval.get_start());
        self.variables.insert(*interval.get_end());
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        for var in constraint.get_variables() {
            self.add_var(&var)
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
            self.add_var(&var);
        }

        self.conditions.push(cond);
    }

    pub fn add_effect(&mut self, effect: Effect) {
        for var in effect.get_variables() {
            self.add_var(&var);
        }
        self.effects.push(effect);
    }

    pub fn add_subtask(&mut self, sub_task: SubTask) {
        for var in sub_task.get_variables() {
            self.add_var(&var);
        }
        self.subtasks.push(sub_task);
    }

    pub fn add_task_template(&mut self, task: TaskTemplate) {
        self.syntactic_chronicles.push(task)
    }

    pub fn add_task_parameter(&mut self, param: &AtomId) {
        self.task.push(*param)
    }
    pub fn add_method_parameter(&mut self, param: &AtomId) {
        self.name.push(*param)
    }

    /*
    SETTERS
     */

    pub fn set_name(&mut self, name: Vec<AtomId>) {
        self.name = name;
    }

    pub fn set_task(&mut self, task: Vec<AtomId>) {
        self.task = task;
    }

    /*
    FORMATTERS
     */

    pub fn format(&self, sym_version: bool) -> String {
        let st = &self.sym_table;
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
        s.push_str(format!("- name: {}\n", self.name.format(&st.borrow(), sym_version)).as_str());
        //task
        s.push_str(format!("- task: {}\n", self.task.format(&st.borrow(), sym_version)).as_str());
        s.push_str("-variable(s): {");

        let mut variables = self
            .variables
            .iter()
            .map(|id| {
                format!(
                    "{}({})",
                    id.format(&st, sym_version),
                    st.get_type_of(id).format(&st, sym_version)
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
            write!(s, "\n\t{}", c.format(&st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");

        //conditions
        s.push_str("-conditon(s): {");
        for c in &self.conditions {
            write!(s, "\n\t{}", c.format(&st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");
        //effects
        s.push_str("-effect(s): {");
        for e in &self.effects {
            write!(s, "\n\t{}", e.format(&st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {");
        for e in &self.subtasks {
            write!(s, "\n\t{}", e.format(&st.borrow(), sym_version)).unwrap();
        }
        s.push_str("}\n");
        s.push_str("-synthetic task(s): {\n");
        for t in &self.syntactic_chronicles {
            writeln!(s, "task: {}", t.name.format(&st, sym_version)).unwrap();
            writeln!(s, "methods:").unwrap();
            for m in &t.methods {
                write!(s, "{}", m.to_string()).unwrap();
            }
        }
        s.push_str("}\n");

        //Debug
        /*if let Some(exp) = &self.debug {
            s.push_str(format!("debug: {}", exp.format("debug: ".len())).as_str());
        }*/

        s
    }

    pub fn format_with_parent(&mut self) {
        let st = &self.sym_table;
        self.name.flat_bindings(&st.borrow());
        self.task.flat_bindings(&st.borrow());
        let old_variables = self.variables.clone();
        let mut new_variables: HashSet<AtomId> = Default::default();
        for v in &old_variables {
            let mut v = *v;
            v.flat_bindings(&st.borrow());
            new_variables.insert(v);
        }

        self.variables = new_variables;
        self.interval.flat_bindings(&st.borrow());
        self.presence.flat_bindings(&st.borrow());
        self.constraints.flat_bindings(&st.borrow());
        self.conditions.flat_bindings(&st.borrow());
        self.effects.flat_bindings(&st.borrow());
        self.subtasks.flat_bindings(&st.borrow());
    }
}

impl GetVariables for ChronicleTemplate {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.variables.clone()
    }

    fn get_variables_of_type(
        &self,
        sym_table: &RefSymTable,
        atom_type: &AtomType,
    ) -> HashSet<AtomId> {
        self.variables
            .iter()
            .filter(|v| sym_table.get_type_of(v) == *atom_type)
            .cloned()
            .collect()
    }
}

impl Replace for ChronicleTemplate {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
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
