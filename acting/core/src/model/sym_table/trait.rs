use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
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
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let id = match sym_version {
            true => st.get_var_parent(self),
            false => *self,
        };
        st.format_variable(&id)
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
    ) -> im::HashSet<VarId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.contained_in_domain(&sym_table.get_domain_of_var(v), domain))
            .cloned()
            .collect()
    }
}

impl<T> GetVariables for Vec<T>
where
    T: GetVariables,
{
    fn get_variables(&self) -> im::HashSet<VarId> {
        let mut set: im::HashSet<VarId> = Default::default();
        for e in self {
            set = set.union(e.get_variables())
        }
        set
    }
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
