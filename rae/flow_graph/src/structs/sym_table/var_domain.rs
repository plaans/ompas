use crate::structs::domain::Domain;
use crate::structs::sym_table::closure::UpdateClosure;
use crate::structs::sym_table::AtomId;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Default)]
pub struct VarDomain {
    pub label: String,
    pub domain: Domain,
    pub updates: Vec<UpdateClosure>,
    pub dependents: Vec<AtomId>,
}

impl VarDomain {
    pub fn new(label: impl Display, domain: impl Into<Domain>) -> Self {
        Self {
            label: label.to_string(),
            domain: domain.into(),
            updates: vec![],
            dependents: vec![],
        }
    }
}

impl Display for VarDomain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.label, self.domain)
    }
}

impl Debug for VarDomain {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
