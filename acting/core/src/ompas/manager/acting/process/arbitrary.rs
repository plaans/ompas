use crate::ompas::manager::acting::acting_var::{ActingValUpdate, ActingVarId, ExecutionVar};
use crate::ompas::manager::acting::process::ActingProcessInner;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub struct ArbitraryProcess {
    pub(crate) var: ExecutionVar<LValue>,
    pub(crate) set: Vec<LValue>,
}

impl ArbitraryProcess {
    pub fn new(var: ExecutionVar<LValue>) -> Self {
        Self { var, set: vec![] }
    }
    pub fn set_set(&mut self, set: Vec<LValue>) {
        self.set = set
    }

    pub fn set_var(&mut self, var: LValue) -> Option<ActingValUpdate> {
        self.var.set_val(var)
    }

    pub fn get_plan_var_id(&self) -> &Option<ActingVarId> {
        self.var.get_plan_var_id()
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
