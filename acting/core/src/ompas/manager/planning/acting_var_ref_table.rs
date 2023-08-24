use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::ompas::manager::acting::acting_var::ActingVarRef;
use aries::model::lang::{Atom, Variable};

#[derive(Default)]
pub struct ActingVarRefTable {
    inner: im::HashMap<ActingVarRef, Variable>,
    reverse: im::HashMap<Variable, ActingVarRef>,
}

impl ActingVarRefTable {
    pub fn clear(&mut self) {
        self.inner.clear();
        self.reverse.clear();
    }

    pub fn add_binding(&mut self, id: impl Into<ActingVarRef>, var: Variable) {
        let id = id.into();
        self.inner.insert(id, var);
        self.reverse.insert(var, id);
    }

    pub fn contains(&mut self, id: impl Into<ActingVarRef>) -> bool {
        let id = id.into();
        self.inner.contains_key(&id)
    }

    pub fn get_var(&self, id: impl Into<ActingVarRef>) -> Option<&Variable> {
        let id = id.into();
        self.inner.get(&id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&ActingVarRef> {
        self.reverse.get(var)
    }
}

impl FormatWithSymTable for ActingVarRefTable {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "#BINDINGS: \n".to_string();
        for (var, r#ref) in &self.reverse {
            str.push_str(
                format!(
                    "{:?} <- {}\n",
                    Atom::from(*var),
                    r#ref.var_id().format(st, sym_version)
                )
                .as_str(),
            )
        }
        str
    }
}
