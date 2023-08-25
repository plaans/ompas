use crate::ompas::manager::acting::acting_var::ActingVarRef;
use crate::ompas::manager::acting::process::ActingProcessInner;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub struct ArbitraryProcess {
    pub(crate) var: ActingVarRef<LValue>,
    pub(crate) set: Vec<LValue>,
}

impl ArbitraryProcess {
    pub fn new(var: ActingVarRef<LValue>) -> Self {
        Self { var, set: vec![] }
    }
    pub fn set_set(&mut self, set: Vec<LValue>) {
        self.set = set
    }
}

impl From<ArbitraryProcess> for ActingProcessInner {
    fn from(value: ArbitraryProcess) -> Self {
        Self::Arbitrary(value)
    }
}

impl Display for ArbitraryProcess {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}
