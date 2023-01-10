use crate::structs::domain::Domain;
use crate::structs::sym_table::closure::Update;
use crate::structs::sym_table::VarId;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Default)]
pub struct VarDomain {
    pub domain: Domain,
    pub updates: Vec<Update>,
    pub vars: Vec<VarId>,
}

impl VarDomain {
    pub fn new(domain: impl Into<Domain>) -> Self {
        Self {
            domain: domain.into(),
            updates: vec![],
            vars: vec![],
        }
    }

    pub fn add_var(&mut self, var: VarId) {
        self.vars.push(var)
    }
}

impl Display for VarDomain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.domain)
    }
}

impl Debug for VarDomain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
