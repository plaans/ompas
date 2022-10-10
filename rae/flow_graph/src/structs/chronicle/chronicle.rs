use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::sym_table::SymTable;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::*;
use crate::structs::flow_graph::graph::FlowGraph;
use im::hashset::HashSet;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::fmt::Display;
use std::ops::Deref;

pub enum ChronicleKind {}

pub struct ChronicleDebug {
    pub kind: ChronicleKind,
    label: String,
    lvalue: LValue,
    flow_graph: FlowGraph,
}

pub struct ChronicleTemplate {
    sym_table: SymTable,
    pub name: Vec<AtomId>,
    pub task: Vec<AtomId>,
    pub presence: AtomId,
    pub interval: Interval,
    pub result: AtomId,
    pub variables: HashSet<AtomId>,
    pub constraints: Vec<Constraint>,
    pub conditions: Vec<Condition>,
    pub effects: Vec<Effect>,
    pub subtasks: Vec<SubTask>,
    debug: ChronicleDebug,
}

impl ChronicleTemplate {
    pub fn new(label: impl Display, chronicle_kind: ChronicleKind) -> Self {
        let mut sym_table = SymTable::default();

        let interval = Interval::new(
            &sym_table.declare_new_parameter(START, AtomType::Timepoint),
            &sym_table.declare_new_parameter(END, AtomType::Timepoint),
        );

        let presence = sym_table.declare_new_parameter(PREZ, AtomType::Presence);

        let result = sym_table.declare_new_parameter(RESULT, AtomType::Untyped);

        let init_var = vec![presence, result, *interval.start(), *interval.end()];

        let mut chronicle = Self {
            sym_table: Default::default(),
            debug: ChronicleDebug {
                kind: chronicle_kind,
                label: label.to_string(),
                lvalue: Default::default(),
                flow_graph: Default::default(),
            },
            name: Default::default(),
            task: Default::default(),
            presence: 0,
            interval,
            result: Default::default(),
            variables: Default::default(),
            constraints: vec![],
            conditions: vec![],
            effects: vec![],
            subtasks: vec![],
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

    pub fn get_symbol_variables(&self, sym_table: &SymTable) -> HashSet<AtomId> {
        let variables = self.get_variables();
        variables
            .iter()
            .filter(|a| sym_table.get_type_of(a).unwrap() == &AtomType::Symbol)
            .cloned()
            .collect()
    }

    pub fn get_debug(&self) -> &ChronicleDebug {
        &self.debug
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
        self.variables.insert(*interval.start());
        self.variables.insert(*interval.end());
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        if let Constraint::And(Lit::Constraint(a), Lit::Constraint(b)) = constraint {
            self.constraints.push(a.deref().clone());
            self.constraints.push(b.deref().clone());
        } else {
            self.constraints.push(constraint);
        }
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.conditions.push(cond);
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.effects.push(effect);
    }

    pub fn add_subtask(&mut self, sub_task: SubTask) {
        self.subtasks.push(sub_task);
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
}

impl GetVariables for ChronicleTemplate {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.variables.clone()
    }

    fn get_variables_of_type(&self, sym_table: &SymTable, atom_type: &AtomType) -> HashSet<AtomId> {
        self.variables
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap() == atom_type)
            .cloned()
            .collect()
    }
}

impl FormatWithSymTable for ChronicleTemplate {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut s = String::new();
        //name
        s.push_str(self.debug.label.as_str());
        s.push('\n');
        s.push_str(format!("- name: {}\n", self.name.format(st, sym_version)).as_str());
        //task
        s.push_str(format!("- task: {}\n", self.task.format(st, sym_version)).as_str());
        /*s.push_str(
            format!(
                "{}: {} {}\n",
                self.presence.format(st, sym_version),
                self.interval.format(st, sym_version),
                self.result.format(st, sym_version)
            )
                .as_str(),
        );*/
        s.push_str("-variable(s): {");

        let mut variables = self
            .variables
            .iter()
            .map(|id| {
                /*format!(
                    "({}){}({})",
                    id,
                    id.format_with_sym_table(st, sym_version),
                    st.get_type_of(id)
                        .unwrap()
                        .a_type
                        .format_with_sym_table(st, sym_version)
                )*/

                format!(
                    "{}({})",
                    id.format(st, sym_version),
                    st.get_type_of(id).unwrap().format(st, sym_version)
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

        s.push_str("-constraint(s): {\n");
        for c in &self.constraints {
            s.push('\t');
            s.push_str(c.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");

        //conditions
        s.push_str("-conditon(s): {\n");
        for e in &self.conditions {
            s.push('\t');
            s.push_str(e.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //effects
        s.push_str("-effect(s): {\n");
        for e in &self.effects {
            s.push('\t');
            s.push_str(e.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {\n");
        for e in &self.subtasks {
            s.push('\t');
            s.push_str(e.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");

        //Debug
        /*if let Some(exp) = &self.debug {
            s.push_str(format!("debug: {}", exp.format("debug: ".len())).as_str());
        }*/

        s
    }
}

impl FormatWithParent for ChronicleTemplate {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.name.format_with_parent(st);
        self.task.format_with_parent(st);
        let old_variables = self.variables.clone();
        let mut new_variables: HashSet<AtomId> = Default::default();
        for v in &old_variables {
            let mut v = *v;
            v.format_with_parent(st);
            new_variables.insert(v);
        }

        self.variables = new_variables;
        self.interval.format_with_parent(st);
        self.presence.format_with_parent(st);
        self.constraints.format_with_parent(st);
        self.conditions.format_with_parent(st);
        self.effects.format_with_parent(st);
        self.subtasks.format_with_parent(st);
    }
}

pub enum ChronicleSet {
    Effect,
    Constraint,
    Condition,
    SubTask,
}
