use crate::acting_manager::process::process_ref::Label;
use crate::conversion::chronicle::interval::Interval;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, Replace};
use crate::sym_table::VarId;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Default, Clone)]
pub struct OperationalModelBindings {
    pub inner: HashMap<Label, ChronicleBinding>,
}

impl OperationalModelBindings {
    pub fn add_binding(&mut self, label: Label, binding: impl Into<ChronicleBinding>) {
        if let Some(_) = self.inner.insert(label, binding.into()) {
            panic!()
        }
    }

    pub fn get_binding(&self, label: &Label) -> Option<&ChronicleBinding> {
        self.inner.get(label)
    }
}

impl Replace for OperationalModelBindings {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        for e in self.inner.values_mut() {
            match e {
                ChronicleBinding::Arbitrary(a) => a.var_id.replace(old, new),
                ChronicleBinding::Action(s) => {
                    s.name.replace(old, new);
                    s.interval.replace(old, new);
                }
                ChronicleBinding::Acquire(acq) => {
                    acq.request.replace(old, new);
                    acq.acquisition.replace(old, new);
                    acq.quantity.replace(old, new);
                    acq.resource.replace(old, new);
                }
            }
        }
    }
}

impl FlatBindings for OperationalModelBindings {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        for e in self.inner.values_mut() {
            match e {
                ChronicleBinding::Arbitrary(a) => a.var_id.flat_bindings(st),
                ChronicleBinding::Action(s) => {
                    s.name.flat_bindings(st);
                    s.interval.flat_bindings(st);
                }
                ChronicleBinding::Acquire(acq) => {
                    acq.request.flat_bindings(st);
                    acq.acquisition.flat_bindings(st);
                    acq.quantity.flat_bindings(st);
                    acq.resource.flat_bindings(st);
                }
            }
        }
    }
}

impl FormatWithSymTable for OperationalModelBindings {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "OperationalModelBindings:\n".to_string();

        for (label, binding) in &self.inner {
            writeln!(str, "{}:{}", label, binding.format(st, sym_version)).unwrap();
        }
        str
    }
}

#[derive(Clone)]
pub enum ChronicleBinding {
    Arbitrary(ArbitraryBinding),
    Action(ActionBinding),
    Acquire(AcquireBinding),
}

impl ChronicleBinding {
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

impl From<ArbitraryBinding> for ChronicleBinding {
    fn from(value: ArbitraryBinding) -> Self {
        Self::Arbitrary(value)
    }
}

impl From<ActionBinding> for ChronicleBinding {
    fn from(value: ActionBinding) -> Self {
        Self::Action(value)
    }
}

impl From<AcquireBinding> for ChronicleBinding {
    fn from(value: AcquireBinding) -> Self {
        Self::Acquire(value)
    }
}
impl FormatWithSymTable for ChronicleBinding {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            ChronicleBinding::Arbitrary(a) => {
                write!(str, "{}", a.var_id.format(st, sym_version)).unwrap();
            }
            ChronicleBinding::Action(s) => {
                write!(
                    str,
                    "({}){}: {}",
                    s.index,
                    s.interval.format(st, sym_version),
                    s.name.format(st, sym_version)
                )
                .unwrap();
            }
            ChronicleBinding::Acquire(acq) => {
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