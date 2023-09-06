use crate::model::chronicle::acting_process_model::{
    AcquireModel, ActingProcessModel, ActingProcessModelCollection, ActingProcessModelLabel,
};
use crate::model::chronicle::condition::Condition;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::effect::Effect;
use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::lit::Lit;
use crate::model::chronicle::subtask::SubTask;
use crate::model::chronicle::task_template::TaskTemplate;
use crate::model::process_ref::Label;
use crate::model::sym_domain::basic_type::BasicType;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, GetVariables, Replace};
use crate::model::sym_table::VarId;
use crate::planning::conversion::chronicle::post_processing::post_processing;
use crate::planning::conversion::flow_graph::graph::FlowGraph;
use im::HashSet;
use std::borrow::Borrow;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::time::Duration;

pub mod acting_process_model;
pub mod computation;
pub mod condition;
pub mod constraint;
pub mod effect;
pub mod interval;
pub mod lit;
pub mod subtask;
pub mod task_template;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ChronicleKind {
    Root,
    Command,
    Method,
    Task,
}

#[derive(Clone)]
pub struct ChronicleMetaData {
    pub kind: ChronicleKind,
    label: String,
    pub flow_graph: FlowGraph,
    pub convert_time: Duration,
}

#[derive(Clone)]
pub struct Chronicle {
    pub meta_data: ChronicleMetaData,
    name: Vec<VarId>,
    pub task: Vec<VarId>,
    presence: VarId,
    pub interval: Interval,
    result: VarId,
    pub variables: HashSet<VarId>,
    pub constraints: Vec<Constraint>,
    pub conditions: Vec<Condition>,
    effects: Vec<Effect>,
    subtasks: Vec<SubTask>,
    pub acting_process_models: ActingProcessModelCollection,
    pub sub_chronicles: Vec<TaskTemplate>,
    pub st: RefSymTable,
}

impl Chronicle {
    pub fn new(label: impl Display, kind: ChronicleKind, st: RefSymTable) -> Self {
        let interval = Interval::new(st.new_start(), st.new_end());

        let presence = st.new_presence();

        let result = st.new_chronicle_result();

        let init_var = vec![presence, result, interval.get_start(), interval.get_end()];

        let mut chronicle = Self {
            st,
            meta_data: ChronicleMetaData {
                kind,
                label: label.to_string(),
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
            acting_process_models: Default::default(),
            sub_chronicles: vec![],
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

    pub fn get_debug(&self) -> &ChronicleMetaData {
        &self.meta_data
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

    pub fn remove_constraint(&mut self, index: usize) {
        self.constraints.remove(index);
    }

    pub fn remove_condition(&mut self, index: usize) {
        self.conditions.remove(index);
    }

    pub fn remove_effect(&mut self, index: usize) {
        self.effects.remove(index);
    }

    pub fn remove_subtask(&mut self, index: usize) {
        self.subtasks.remove(index);
    }

    pub fn remove_constraints(&mut self, mut indexes: Vec<usize>) {
        indexes.sort_unstable();
        indexes.reverse();
        for index in indexes {
            self.remove_constraint(index);
        }
    }

    pub fn remove_conditions(&mut self, mut indexes: Vec<usize>) {
        indexes.sort_unstable();
        indexes.reverse();
        for index in indexes {
            self.remove_condition(index);
        }
    }

    pub fn remove_effects(&mut self, mut indexes: Vec<usize>) {
        indexes.sort_unstable();
        indexes.reverse();
        for index in indexes {
            self.remove_effect(index)
        }
    }

    pub fn remove_subtasks(&mut self, mut indexes: Vec<usize>) {
        indexes.sort_unstable();
        indexes.reverse();
        for index in indexes {
            self.remove_subtask(index);
        }
    }

    /*
    ADDERS
     */
    pub fn add_var(&mut self, var_id: VarId) {
        let is_variable = !self.st.get_domain_of_var(&var_id).is_constant();
        let is_param = self.st.get_variable(&var_id).is_parameter();
        if is_variable || is_param {
            self.variables.insert(var_id);
        }
    }

    pub fn add_interval(&mut self, interval: Interval) {
        self.variables.insert(interval.get_start());
        self.variables.insert(interval.get_end());
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        for var in constraint.get_variables() {
            self.add_var(var)
        }
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
        self.sub_chronicles.push(task)
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
                self.meta_data.label.as_str(),
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
        s.push_str("-condition(s): {");
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
        for t in &self.sub_chronicles {
            writeln!(s, "task: {}", t.name.format(st, sym_version)).unwrap();
            writeln!(s, "methods:").unwrap();
            for m in &t.methods {
                write!(s, "{}", m).unwrap();
            }
        }
        s.push_str("}\n");
        write!(s, "{}", self.acting_process_models.format(st, sym_version)).unwrap();

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
        self.acting_process_models.flat_bindings(st);
    }

    pub fn instantiate(&self, instantiations: Vec<Instantiation>) -> Self {
        let mut new = self.clone();
        for Instantiation { var, value } in instantiations {
            new.replace(&var, &value);
            new.variables.remove(&var);
            new.variables.remove(&value);
        }
        new
    }

    pub fn instantiate_and_clean(&self, runtime_info: RuntimeInfo) -> Self {
        let mut new = self.clone();

        let RuntimeInfo {
            to_remove,
            instantiations,
        } = runtime_info;

        let models = &mut new.acting_process_models.inner;

        to_remove.iter().for_each(|l| match l {
            ActingProcessModelLabel::Label(_) => {
                models.remove(l);
            }
            ActingProcessModelLabel::Acquire(i) => {
                if let Some(acquire) = models.get_mut(&ActingProcessModelLabel::Label(
                    Label::ResourceAcquisition(*i),
                )) {
                    acquire.as_mut_acquire().unwrap().acquire = AcquireModel::default();
                }
            }
            ActingProcessModelLabel::Release(i) => {
                if let Some(acquire) = models.get_mut(&ActingProcessModelLabel::Label(
                    Label::ResourceAcquisition(*i),
                )) {
                    acquire.as_mut_acquire().unwrap().release = AcquireModel::default();
                }
            }
        });

        let models: Vec<_> = models.values().cloned().collect();
        for model in models {
            new.absorb_acting_process_model(model.clone());
        }

        for Instantiation { var, value } in instantiations {
            new.replace(&var, &value);
            new.variables.remove(&var);
            new.variables.remove(&value);
        }

        new.remove_instantiated_elements();

        post_processing(&mut new).unwrap();

        new
    }

    pub fn absorb_acting_process_model(&mut self, model: impl Into<ActingProcessModel>) {
        let model = model.into();
        match model {
            ActingProcessModel::Arbitrary(a) => {
                if let Some(c) = a.constraints {
                    self.add_constraint(c)
                }
            }
            ActingProcessModel::Action(a) => {
                self.add_subtask(a.task);
                for c in a.constraints {
                    self.add_constraint(c)
                }
            }
            ActingProcessModel::Resource(acq) => {
                self.conditions.push(acq.condition_max_q);
                self.absorb_acquire_model(acq.acquire);
                self.absorb_acquire_model(acq.release);
            }
        }
    }

    pub fn absorb_acquire_model(&mut self, model: AcquireModel) {
        for c in model.constraints {
            self.add_constraint(c)
        }
        for e in model.effects {
            self.add_effect(e)
        }
        for c in model.conditions {
            self.add_condition(c)
        }
    }

    fn remove_instantiated_elements(&mut self) {
        let st = self.st.clone();
        let mut free_variables: HashSet<VarId> = Default::default();
        for var in &self.variables {
            if !st.get_domain_of_var(var).is_constant() {
                free_variables.insert(*var);
            }
        }

        // Remove instantiated constraints
        {
            let mut to_remove = vec![];

            'loop_constraint: for (i, constraint) in self.constraints.iter().enumerate() {
                let variables = constraint.get_variables();
                for var in &variables {
                    if free_variables.contains(var) {
                        continue 'loop_constraint;
                    }
                }
                to_remove.push(i)
            }
            self.remove_constraints(to_remove);
        }

        // Remove instantiated condition
        {
            let mut to_remove = vec![];

            'loop_condition: for (i, condition) in self.conditions.iter().enumerate() {
                let variables = condition.get_variables();
                for var in &variables {
                    if free_variables.contains(var) {
                        continue 'loop_condition;
                    }
                }
                to_remove.push(i)
            }
            self.remove_conditions(to_remove)
        }

        // Remove instantiated effects
        {
            let mut to_remove = vec![];

            'loop_condition: for (i, effect) in self.effects.iter().enumerate() {
                let variables = effect.get_variables();
                for var in &variables {
                    if free_variables.contains(var) {
                        continue 'loop_condition;
                    }
                }
                to_remove.push(i)
            }
            self.remove_effects(to_remove)
        }

        // Remove instantiated tasks
        {
            let mut to_remove = vec![];
            'loop_condition: for (i, subtask) in self.subtasks.iter().enumerate() {
                let variables = subtask.get_variables();
                for var in &variables {
                    if free_variables.contains(var) {
                        continue 'loop_condition;
                    }
                }
                to_remove.push(i)
            }

            self.remove_subtasks(to_remove)
        }

        /*// Remove bindings
        {
            let mut to_remove = vec![];
            'loop_binding: for (label, binding) in &self.acting_process_models.inner {
                match binding {
                    ActingProcessModel::Resource(acq) => {
                        if free_variables.contains(&acq.request)
                            && free_variables.contains(&acq.acquisition.get_start())
                        {
                            to_remove.push(*label)
                        }
                    }
                    ActingProcessModel::Action(_) => {}
                    _ => {
                        let variables = binding.get_variables();
                        for var in &variables {
                            if free_variables.contains(var) {
                                continue 'loop_binding;
                            }
                        }
                        to_remove.push(*label)
                    }
                }
            }

            for label in to_remove {
                self.acting_process_models.remove_process_model(label);
            }
        }*/
    }
}

#[derive(Default, Clone)]
pub struct RuntimeInfo {
    to_remove: Vec<ActingProcessModelLabel>,
    instantiations: Vec<Instantiation>,
}

impl From<Vec<Instantiation>> for RuntimeInfo {
    fn from(instantiations: Vec<Instantiation>) -> Self {
        Self {
            to_remove: vec![],
            instantiations,
        }
    }
}

impl RuntimeInfo {
    pub fn add_instantiation(&mut self, instantiation: Instantiation) {
        self.instantiations.push(instantiation)
    }

    pub fn add_to_remove(&mut self, label: impl Into<ActingProcessModelLabel>) {
        self.to_remove.push(label.into())
    }
}

#[derive(Copy, Clone)]
pub struct Instantiation {
    var: VarId,
    value: VarId,
}

impl Instantiation {
    pub fn new(var: VarId, value: VarId) -> Self {
        Self { var, value }
    }
}

impl GetVariables for Chronicle {
    fn get_variables(&self) -> HashSet<VarId> {
        self.variables.clone()
    }
}

impl Replace for Chronicle {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.presence.replace(old, new);
        self.interval.replace(old, new);
        self.name.replace(old, new);
        self.task.replace(old, new);
        for sub in &mut self.sub_chronicles {
            sub.name.replace(old, new);
            for method in &mut sub.methods {
                method.replace(old, new);
            }
        }
        self.variables.remove(old);
        self.variables.insert(*new);
        self.conditions.replace(old, new);
        self.constraints.replace(old, new);
        self.subtasks.replace(old, new);
        self.effects.replace(old, new);
        self.acting_process_models.replace(old, new);
    }
}

impl Display for Chronicle {
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
