use crate::structs::domain::Domain;
use crate::structs::sym_table::closure::Update;
use crate::structs::sym_table::AtomId;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

pub type UpdateId = usize;
#[derive(Clone, Default)]
pub struct VarDomain {
    pub label: String,
    pub domain: Domain,
    pub updates: Vec<Update>,
}

impl VarDomain {
    pub fn new(label: impl Display, domain: impl Into<Domain>) -> Self {
        Self {
            label: label.to_string(),
            domain: domain.into(),
            updates: vec![],
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
