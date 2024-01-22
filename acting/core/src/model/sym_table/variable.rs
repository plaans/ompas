use crate::model::sym_table::{DomainId, VarId};
use std::fmt::{Display, Formatter};

#[derive(Clone, Default)]
pub struct Variable {
    pub domain: DomainId,
    pub parameter: bool,
    pub label: String,
    pub symbol: String,
    pub declaration: Option<VarId>,
    pub drop: Option<VarId>,
}

impl Variable {
    pub fn new(symbol: impl Display, label: impl Display, domain: DomainId) -> Self {
        Self {
            domain,
            parameter: false,
            label: label.to_string(),
            symbol: symbol.to_string(),
            declaration: None,
            drop: None,
        }
    }

    pub fn new_parameter(symbol: impl Display, label: impl Display, domain: DomainId) -> Self {
        Self {
            domain,
            parameter: true,
            label: label.to_string(),
            symbol: symbol.to_string(),
            declaration: None,
            drop: None,
        }
    }

    pub fn is_parameter(&self) -> bool {
        self.parameter
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.label)
    }
}
