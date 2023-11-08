use crate::model::chronicle::condition::Condition;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::effect::Effect;
use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::subtask::SubTask;
use crate::model::process_ref::Label;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::model::sym_table::VarId;
use map_macro::hash_set;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter, Write};

#[derive(Default, Clone)]
pub struct ActingProcessModelCollection {
    pub inner: HashMap<ActingProcessModelLabel, ActingProcessModel>,
}

impl ActingProcessModelCollection {
    pub fn add_binding(
        &mut self,
        label: impl Into<ActingProcessModelLabel>,
        binding: impl Into<ActingProcessModel>,
    ) {
        let binding = binding.into();
        let label = label.into();
        if let std::collections::hash_map::Entry::Vacant(e) = self.inner.entry(label.clone()) {
            e.insert(binding);
        } else {
            println!(
                "WARNING! (label = {}, binding = {:?}) has already been added to chronicle",
                label, binding
            )
        }
    }

    pub fn get_actions(&self) -> Vec<&ActionModel> {
        self.inner
            .values()
            .filter_map(|b| {
                if let ActingProcessModel::Action(a) = b {
                    Some(a)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_arbitraries(&self) -> Vec<&ArbitraryModel> {
        self.inner
            .values()
            .filter_map(|b| {
                if let ActingProcessModel::Arbitrary(a) = b {
                    Some(a)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_acquires(&self) -> Vec<&ResourceModel> {
        self.inner
            .values()
            .filter_map(|b| {
                if let ActingProcessModel::Resource(a) = b {
                    Some(a)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_process_model(
        &self,
        label: impl Into<ActingProcessModelLabel>,
    ) -> Option<&ActingProcessModel> {
        self.inner.get(&label.into())
    }

    pub fn remove_process_model(&mut self, label: impl Into<ActingProcessModelLabel>) {
        self.inner.remove(&label.into());
    }
}

impl Replace for ActingProcessModelCollection {
    fn replace(&mut self, old: VarId, new: VarId) {
        for e in self.inner.values_mut() {
            match e {
                ActingProcessModel::Arbitrary(a) => {
                    a.var_id.replace(old, new);
                    a.timepoint.replace(old, new);
                    if let Some(c) = a.constraints.as_mut() {
                        c.replace(old, new);
                    }
                }
                ActingProcessModel::Action(s) => {
                    s.task.replace(old, new);
                    s.constraints.replace(old, new);
                }
                ActingProcessModel::Resource(acq) => {
                    acq.request.replace(old, new);
                    acq.acquisition.replace(old, new);
                    acq.resource.replace(old, new);
                    acq.quantity.replace(old, new);
                    acq.condition_max_q.replace(old, new);
                    acq.acquire.replace(old, new);
                    acq.release.replace(old, new);
                }
            }
        }
    }
}

impl FlatBindings for ActingProcessModelCollection {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        for e in self.inner.values_mut() {
            match e {
                ActingProcessModel::Arbitrary(a) => {
                    a.var_id.flat_bindings(st);
                    a.timepoint.flat_bindings(st);
                    if let Some(c) = &mut a.constraints {
                        c.flat_bindings(st);
                    }
                }
                ActingProcessModel::Action(s) => {
                    s.task.flat_bindings(st);
                    s.constraints.flat_bindings(st);
                }
                ActingProcessModel::Resource(acq) => {
                    acq.request.flat_bindings(st);
                    acq.acquisition.flat_bindings(st);
                    acq.resource.flat_bindings(st);
                    acq.quantity.flat_bindings(st);
                    acq.condition_max_q.flat_bindings(st);
                    acq.acquire.flat_bindings(st);
                    acq.release.flat_bindings(st);
                }
            }
        }
    }
}

impl FormatWithSymTable for ActingProcessModelCollection {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "OperationalModelBindings:\n".to_string();

        for (label, binding) in &self.inner {
            writeln!(str, "{}:{}", label, binding.format(st, sym_version)).unwrap();
        }
        str
    }
}

#[derive(Clone, Debug)]
pub enum ActingProcessModel {
    Arbitrary(ArbitraryModel),
    Action(ActionModel),
    Resource(ResourceModel),
}

impl GetVariables for ActingProcessModel {
    fn get_variables(&self) -> std::collections::HashSet<VarId> {
        match self {
            ActingProcessModel::Arbitrary(a) => {
                let set = hash_set! {a.var_id, a.timepoint};
                if let Some(c) = &a.constraints {
                    set.union(&c.get_variables()).cloned().collect()
                } else {
                    set
                }
            }
            ActingProcessModel::Action(a) => {
                let set = a.task.get_variables();
                set.union(&a.constraints.get_variables()).cloned().collect()
            }
            ActingProcessModel::Resource(acq) => {
                let mut set = acq.acquisition.get_variables();
                set.insert(acq.request);
                set.insert(acq.resource);
                set.insert(acq.quantity);
                let set: HashSet<_> = set
                    .union(&acq.condition_max_q.get_variables())
                    .cloned()
                    .collect();
                set.union(
                    &acq.acquire
                        .get_variables()
                        .union(&acq.release.get_variables())
                        .cloned()
                        .collect(),
                )
                .cloned()
                .collect()
            }
        }
    }
}

impl ActingProcessModel {
    pub fn as_arbitrary(&self) -> Option<&ArbitraryModel> {
        if let Self::Arbitrary(a) = self {
            Some(a)
        } else {
            None
        }
    }

    pub fn as_action(&self) -> Option<&ActionModel> {
        if let Self::Action(a) = self {
            Some(a)
        } else {
            None
        }
    }

    pub fn as_acquire(&self) -> Option<&ResourceModel> {
        if let Self::Resource(a) = self {
            Some(a)
        } else {
            None
        }
    }

    pub fn as_mut_acquire(&mut self) -> Option<&mut ResourceModel> {
        if let Self::Resource(a) = self {
            Some(a)
        } else {
            None
        }
    }
}

impl From<ArbitraryModel> for ActingProcessModel {
    fn from(value: ArbitraryModel) -> Self {
        Self::Arbitrary(value)
    }
}

impl From<ActionModel> for ActingProcessModel {
    fn from(value: ActionModel) -> Self {
        Self::Action(value)
    }
}

impl From<ResourceModel> for ActingProcessModel {
    fn from(value: ResourceModel) -> Self {
        Self::Resource(value)
    }
}
impl FormatWithSymTable for ActingProcessModel {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            ActingProcessModel::Arbitrary(a) => {
                write!(str, "{}", a.var_id.format(st, sym_version)).unwrap();
            }
            ActingProcessModel::Action(s) => {
                write!(
                    str,
                    "{}: {}",
                    s.task.interval.format(st, sym_version),
                    s.task.name.format(st, sym_version)
                )
                .unwrap();
            }
            ActingProcessModel::Resource(acq) => {
                write!(
                    str,
                    "[{},{},{}]",
                    acq.request.format(st, sym_version),
                    acq.acquisition.get_start().format(st, sym_version),
                    acq.acquisition.get_end().format(st, sym_version),
                )
                .unwrap();
            }
        }
        str
    }
}

#[derive(Clone, Debug)]
pub struct ArbitraryModel {
    pub timepoint: VarId,
    pub var_id: VarId,
    pub constraints: Option<Constraint>,
}

impl ArbitraryModel {
    pub fn new(timepoint: VarId, var_id: VarId, constraints: Option<Constraint>) -> Self {
        Self {
            timepoint,
            var_id,
            constraints,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ActionModel {
    pub task: SubTask,
    pub constraints: Vec<Constraint>,
}

impl ActionModel {
    pub fn new(task: SubTask, constraints: Vec<Constraint>) -> Self {
        Self { task, constraints }
    }
}

#[derive(Debug, Default, Clone)]
pub struct AcquireModel {
    pub constraints: Vec<Constraint>,
    pub conditions: Vec<Condition>,
    pub effects: Vec<Effect>,
}

impl FlatBindings for AcquireModel {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.constraints.flat_bindings(st);
        self.conditions.flat_bindings(st);
        self.effects.flat_bindings(st);
    }
}

impl Replace for AcquireModel {
    fn replace(&mut self, old: VarId, new: VarId) {
        self.constraints.replace(old, new);
        self.conditions.replace(old, new);
        self.effects.replace(old, new);
    }
}

impl GetVariables for AcquireModel {
    fn get_variables(&self) -> std::collections::HashSet<VarId> {
        self.constraints
            .get_variables()
            .union(
                &self
                    .conditions
                    .get_variables()
                    .union(&self.effects.get_variables())
                    .cloned()
                    .collect(),
            )
            .cloned()
            .collect()
    }
}

pub type ReleaseModel = AcquireModel;

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum ActingProcessModelLabel {
    Label(Label),
    Acquire(usize),
    Release(usize),
}

impl Display for ActingProcessModelLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ActingProcessModelLabel::Label(l) => {
                write!(f, "{l}")
            }
            ActingProcessModelLabel::Acquire(acq) => {
                write!(f, "acquisition({acq})",)
            }
            ActingProcessModelLabel::Release(release) => {
                write!(f, "release({release})")
            }
        }
    }
}

impl From<Label> for ActingProcessModelLabel {
    fn from(value: Label) -> Self {
        Self::Label(value)
    }
}

#[derive(Clone, Debug)]
pub struct ResourceModel {
    pub resource: VarId,
    pub quantity: VarId,
    pub request: VarId,
    pub acquisition: Interval,
    pub condition_max_q: Condition,
    pub acquire: AcquireModel,
    pub release: ReleaseModel,
}

impl ResourceModel {
    pub fn new(
        resource: VarId,
        quantity: VarId,
        request: VarId,
        acquisition: Interval,
        condition_max_q: Condition,
        acquire: AcquireModel,
        release: ReleaseModel,
    ) -> Self {
        Self {
            resource,
            quantity,
            request,
            acquisition,
            condition_max_q,
            acquire,
            release,
        }
    }
}
