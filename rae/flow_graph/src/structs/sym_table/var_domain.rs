use crate::structs::domain::Domain;
use crate::structs::sym_table::AtomId;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Default)]
pub struct VarDomain {
    pub label: String,
    pub domain: Domain,
    pub union: Vec<AtomId>,
    pub parents: Vec<AtomId>,
}

impl VarDomain {
    pub fn new(label: impl Display, domain: impl Into<Domain>) -> Self {
        Self {
            label: label.to_string(),
            domain: domain.into(),
            union: vec![],
            parents: vec![],
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
        write!(
            f,
            "{}, union: {:?}, parent: {:?}",
            self, self.union, self.parents
        )
    }
}
