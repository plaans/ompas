use crate::structs::domain::Domain;
use crate::structs::sym_table::closure::{ConstraintClosure, UpdateClosure};
use crate::structs::sym_table::AtomId;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Default)]
pub struct VarDomain {
    pub label: String,
    pub domain: Domain,
    pub constraints: Vec<ConstraintClosure>,
    pub updates: Vec<UpdateClosure>,
    pub depends: Vec<AtomId>,
}

impl VarDomain {
    pub fn new(label: impl Display, domain: impl Into<Domain>) -> Self {
        Self {
            label: label.to_string(),
            domain: domain.into(),
            constraints: vec![],
            updates: vec![],
            depends: vec![],
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
