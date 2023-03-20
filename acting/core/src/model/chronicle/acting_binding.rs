use crate::model::chronicle::interval::Interval;
use crate::model::process_ref::Label;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, Replace};
use crate::model::sym_table::VarId;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Default, Clone)]
pub struct ActingBindingCollection {
    pub inner: HashMap<Label, ActingBinding>,
}

impl ActingBindingCollection {
    pub fn add_binding(&mut self, label: Label, binding: impl Into<ActingBinding>) {
        if let Some(_) = self.inner.insert(label, binding.into()) {
            panic!()
        }
    }

    pub fn get_binding(&self, label: &Label) -> Option<&ActingBinding> {
        self.inner.get(label)
    }
}

impl Replace for ActingBindingCollection {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        for e in self.inner.values_mut() {
            match e {
                ActingBinding::Arbitrary(a) => a.var_id.replace(old, new),
                ActingBinding::Action(s) => {
                    s.name.replace(old, new);
                    s.interval.replace(old, new);
                }
                ActingBinding::Acquire(acq) => {
                    acq.request.replace(old, new);
                    acq.acquisition.replace(old, new);
                    acq.quantity.replace(old, new);
                    acq.resource.replace(old, new);
                }
            }
        }
    }
}

impl FlatBindings for ActingBindingCollection {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        for e in self.inner.values_mut() {
            match e {
                ActingBinding::Arbitrary(a) => a.var_id.flat_bindings(st),
                ActingBinding::Action(s) => {
                    s.name.flat_bindings(st);
                    s.interval.flat_bindings(st);
                }
                ActingBinding::Acquire(acq) => {
                    acq.request.flat_bindings(st);
                    acq.acquisition.flat_bindings(st);
                    acq.quantity.flat_bindings(st);
                    acq.resource.flat_bindings(st);
                }
            }
        }
    }
}

impl FormatWithSymTable for ActingBindingCollection {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "OperationalModelBindings:\n".to_string();

        for (label, binding) in &self.inner {
            writeln!(str, "{}:{}", label, binding.format(st, sym_version)).unwrap();
        }
        str
    }
}

#[derive(Clone)]
pub enum ActingBinding {
    Arbitrary(ArbitraryBinding),
    Action(ActionBinding),
    Acquire(AcquireBinding),
}

impl ActingBinding {
    pub fn as_arbitrary(&self) -> Option<&ArbitraryBinding> {
        if let Self::Arbitrary(a) = self {
            Some(a)
        } else {
            None
        }
    }

    pub fn as_action(&self) -> Option<&ActionBinding> {
        if let Self::Action(a) = self {
            Some(a)
        } else {
            None
        }
    }

    pub fn as_acquire(&self) -> Option<&AcquireBinding> {
        if let Self::Acquire(a) = self {
            Some(a)
        } else {
            None
        }
    }
}

impl From<ArbitraryBinding> for ActingBinding {
    fn from(value: ArbitraryBinding) -> Self {
        Self::Arbitrary(value)
    }
}

impl From<ActionBinding> for ActingBinding {
    fn from(value: ActionBinding) -> Self {
        Self::Action(value)
    }
}

impl From<AcquireBinding> for ActingBinding {
    fn from(value: AcquireBinding) -> Self {
        Self::Acquire(value)
    }
}
impl FormatWithSymTable for ActingBinding {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            ActingBinding::Arbitrary(a) => {
                write!(str, "{}", a.var_id.format(st, sym_version)).unwrap();
            }
            ActingBinding::Action(s) => {
                write!(
                    str,
                    "({}){}: {}",
                    s.index,
                    s.interval.format(st, sym_version),
                    s.name.format(st, sym_version)
                )
                .unwrap();
            }
            ActingBinding::Acquire(acq) => {
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

#[derive(Copy, Clone)]
pub struct ArbitraryBinding {
    pub timepoint: VarId,
    pub var_id: VarId,
}

#[derive(Clone)]
pub struct ActionBinding {
    pub name: Vec<VarId>,
    pub index: usize,
    pub interval: Interval,
}

#[derive(Copy, Clone)]
pub struct AcquireBinding {
    pub resource: VarId,
    pub quantity: VarId,
    pub request: VarId,
    pub acquisition: Interval,
}
