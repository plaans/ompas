use crate::conversion::chronicle::interval::Interval;
use crate::supervisor::process::process_ref::Label;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::FormatWithSymTable;
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
    Command(CommandBinding),
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
            ChronicleBinding::Command(c) => {
                write!(str, "({}){}", c.index, c.interval.format(st, sym_version)).unwrap();
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
pub struct CommandBinding {
    pub index: usize,
    pub interval: Interval,
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
