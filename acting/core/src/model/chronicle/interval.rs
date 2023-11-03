use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FlatBindings, FormatWithSymTable, GetVariables, Replace};
use crate::model::sym_table::VarId;
use im::{hashset, HashSet};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Interval {
    start: VarId,
    end: VarId,
    duration: Option<VarId>,
}

impl Interval {
    pub fn new(start: VarId, end: VarId) -> Self {
        Self {
            start,
            end,
            duration: None,
        }
    }

    pub fn new_instantaneous(t: VarId) -> Self {
        Self {
            start: t,
            end: t,
            duration: None,
        }
    }

    pub fn new_with_duration(start: VarId, end: VarId, duration: VarId) -> Self {
        Self {
            start,
            end,
            duration: Some(duration),
        }
    }

    pub fn get_start(&self) -> VarId {
        self.start
    }

    pub fn get_end(&self) -> VarId {
        self.end
    }

    pub fn get_duration(&self) -> Option<VarId> {
        self.duration
    }

    pub fn set_end(&mut self, end: VarId) {
        self.end = end;
    }

    pub fn set_duration(&mut self, duration: VarId) {
        self.duration = Some(duration)
    }

    pub fn is_instantaneous(&self) -> bool {
        self.start == self.end
    }
}

impl FormatWithSymTable for Interval {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        if self.is_instantaneous() {
            format!("[{}]", self.start.format(st, sym_version))
        } else {
            match self.duration {
                Some(duration) => {
                    format!(
                        "[{},{}={}+{}]",
                        self.start.format(st, sym_version),
                        self.end.format(st, sym_version),
                        self.start.format(st, sym_version),
                        duration.format(st, sym_version)
                    )
                }
                None => {
                    format!(
                        "[{},{}]",
                        self.start.format(st, sym_version),
                        self.end.format(st, sym_version),
                    )
                }
            }
        }
    }
}

impl FlatBindings for Interval {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.start.flat_bindings(st);
        self.end.flat_bindings(st);
        if let Some(d) = &mut self.duration {
            d.flat_bindings(st)
        }
    }
}

impl GetVariables for Interval {
    fn get_variables(&self) -> HashSet<VarId> {
        let mut set = hashset![self.start, self.end];
        if let Some(d) = self.duration {
            set.insert(d);
        }
        set
    }
}

impl Replace for Interval {
    fn replace(&mut self, old: VarId, new: VarId) {
        self.end.replace(old, new);
        self.start.replace(old, new);
        if let Some(d) = &mut self.duration {
            d.replace(old, new)
        }
    }
}
