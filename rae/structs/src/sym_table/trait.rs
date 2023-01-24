use crate::sym_table::domain::Domain;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::VarId;
use std::collections::HashSet;

impl FormatWithSymTable for Vec<VarId> {
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

impl FormatWithSymTable for HashSet<VarId> {
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

impl FormatWithSymTable for &[VarId] {
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

impl FormatWithSymTable for VarId {
    fn format(&self, st: &RefSymTable, _: bool) -> String {
        st.format_variable(self).to_string()
        /*st.get_domain(self, sym_version)
        .unwrap()
        .format(st.)*/
    }
}

impl FlatBindings for VarId {
    fn flat_bindings(&mut self, st: &RefSymTable) {
        *self = st.get_var_parent(self);
    }
}

pub trait FormatWithSymTable {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String;
}

pub trait GetVariables {
    fn get_variables(&self) -> im::HashSet<VarId>;

    fn get_variables_in_domain(
        &self,
        sym_table: &RefSymTable,
        domain: &Domain,
    ) -> im::HashSet<VarId>;
}

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
    fn replace(&mut self, old: &VarId, new: &VarId);
}

impl<T> Replace for Vec<T>
where
    T: Replace,
{
    fn replace(&mut self, old: &VarId, new: &VarId) {
        self.iter_mut().for_each(|e| e.replace(old, new))
    }
}

impl Replace for VarId {
    fn replace(&mut self, old: &VarId, new: &VarId) {
        if self == old {
            *self = *new
        }
    }
}
