use crate::conversion::chronicle::interval::Interval;
use crate::supervisor::process::process_ref::Label;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::{FlatBindings, FormatWithSymTable, Replace};
use crate::sym_table::VarId;
use std::collections::HashMap;
use std::fmt::Write;

#[derive(Default, Clone)]
pub struct OperationalModelBindings {
    table: HashMap<Label, ChronicleBinding>,
}

impl OperationalModelBindings {
    pub fn add_binding(&mut self, label: Label, binding: ChronicleBinding) {
        if let Some(_) = self.table.insert(label, binding) {
            panic!()
        }
    }
}

impl Replace for OperationalModelBindings {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        for e in self.table.values_mut() {
            match e {
                ChronicleBinding::Arbitrary(a) => a.var_id.replace(old, new),
                ChronicleBinding::Subtask(s) => s.interval.replace(old, new),
                ChronicleBinding::Acquire(acq) => {
                    acq.end.replace(old, new);
                    acq.start.replace(old, new);
                    acq.acquisition.replace(old, new);
                }
            }
        }
    }
}

impl FlatBindings for OperationalModelBindings {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        for e in self.table.values_mut() {
            match e {
                ChronicleBinding::Arbitrary(a) => a.var_id.flat_bindings(st),
                ChronicleBinding::Subtask(s) => s.interval.flat_bindings(st),
                ChronicleBinding::Acquire(acq) => {
                    acq.end.flat_bindings(st);
                    acq.start.flat_bindings(st);
                    acq.acquisition.flat_bindings(st);
                }
            }
        }
    }
}

impl FormatWithSymTable for OperationalModelBindings {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "OperationalModelBindings:\n".to_string();

        for (label, binding) in &self.table {
            writeln!(str, "{}:{}", label, binding.format(st, sym_version)).unwrap();
        }
        str
    }
}

#[derive(Clone)]
pub enum ChronicleBinding {
    Arbitrary(ArbitraryBinding),
    Subtask(SubTaskBinding),
    Acquire(AcquireBinding),
}

impl FormatWithSymTable for ChronicleBinding {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "".to_string();
        match self {
            ChronicleBinding::Arbitrary(a) => {
                write!(str, "{}", a.var_id.format(st, sym_version)).unwrap();
            }
            ChronicleBinding::Subtask(s) => {
                write!(str, "({}){}", s.index, s.interval.format(st, sym_version)).unwrap();
            }
            ChronicleBinding::Acquire(acq) => {
                write!(
                    str,
                    "[{},{},{}]",
                    acq.start.format(st, sym_version),
                    acq.acquisition.format(st, sym_version),
                    acq.end.format(st, sym_version)
                )
                .unwrap();
            }
        }
        str
    }
}

#[derive(Clone)]
pub struct ArbitraryBinding {
    pub var_id: VarId,
}

#[derive(Clone)]
pub struct SubTaskBinding {
    pub index: usize,
    pub interval: Interval,
}

#[derive(Clone)]
pub struct AcquireBinding {
    pub start: VarId,
    pub acquisition: VarId,
    pub end: VarId,
}
