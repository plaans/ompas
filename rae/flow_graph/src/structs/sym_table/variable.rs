use crate::structs::sym_table::{DomainId, VarId};
use std::fmt::{Display, Formatter};

#[derive(Clone, Default)]
pub struct Variable {
    pub domain: DomainId,
    pub label: String,
    pub declaration: Option<VarId>,
    pub drop: Option<VarId>,
}

impl Variable {
    pub fn new(label: impl Display, domain: VarId) -> Self {
        Self {
            domain,
            label: label.to_string(),
            declaration: None,
            drop: None,
        }
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.label)
    }
}
