use crate::structs::chronicle::forest::NodeId;
use crate::structs::chronicle::sym_table::RefSymTable;
use crate::structs::chronicle::type_table::AtomType;
use std::collections::HashSet;

pub mod atom;
pub mod chronicle;
pub mod condition;
pub mod constraint;
pub mod effect;
pub mod forest;
pub mod interval;
pub mod lit;
pub mod subtask;
pub mod sym_table;
pub mod task_template;
pub mod type_table;

pub const START: &str = "start";
pub const END: &str = "end";
pub const PREZ: &str = "prez";
pub const RESULT: &str = "result";
pub const COND: &str = "cond";
pub const IF_TASK_PROTOTYPE: &str = "t_if";

pub type AtomId = NodeId;

impl FormatWithSymTable for Vec<AtomId> {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "(".to_string();
        let mut first = true;
        for e in self {
            if first {
                first = false
            } else {
                str.push(' ');
            }
            str.push_str(e.format(st, sym_version).as_str());
        }
        str.push(')');
        str
    }
}

impl FormatWithSymTable for HashSet<AtomId> {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "(".to_string();
        let mut first = true;
        for e in self {
            if first {
                first = false
            } else {
                str.push(' ');
            }
            str.push_str(e.format(st, sym_version).as_str());
        }
        str.push(')');
        str
    }
}

impl FormatWithSymTable for &[AtomId] {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "(".to_string();
        let mut first = true;
        for e in self.iter() {
            if first {
                first = false
            } else {
                str.push(' ');
            }
            str.push_str(e.format(st, sym_version).as_str());
        }
        str.push(')');
        str
    }
}

impl FormatWithSymTable for AtomId {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        st.get_atom(self, sym_version)
            .unwrap()
            .format(st, sym_version)
    }
}

impl FlatBindings for AtomId {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        *self = st.get_parent(self);
    }
}

pub trait FormatWithSymTable {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String;
}

pub trait GetVariables {
    fn get_variables(&self) -> im::HashSet<AtomId>;

    fn get_variables_of_type(
        &self,
        sym_table: &RefSymTable,
        atom_type: &AtomType,
    ) -> im::HashSet<AtomId>;
}

/*
Transforms all literals by replacing all atomid by the atomid of their parents.
 */
pub trait FlatBindings {
    fn flat_bindings(&mut self, st: &RefSymTable);
}

impl<T> FlatBindings for Vec<T>
where
    T: FlatBindings,
{
    fn flat_bindings(&mut self, st: &RefSymTable) {
        self.iter_mut().for_each(|e| e.flat_bindings(st))
    }
}

pub trait Replace {
    fn replace(&mut self, old: &AtomId, new: &AtomId);
}

impl<T> Replace for Vec<T>
where
    T: Replace,
{
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        self.iter_mut().for_each(|e| e.replace(old, new))
    }
}

impl Replace for AtomId {
    fn replace(&mut self, old: &AtomId, new: &AtomId) {
        if self == old {
            *self = *new
        }
    }
}
