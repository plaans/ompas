use crate::acting_manager::process::plan_var::{ExecutionVar, PlanVarId};
use crate::acting_manager::process::ActingProcessInner;
use crate::sym_table::domain::cst;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

pub struct ArbitraryProcess {
    var: ExecutionVar<LValue>,
    set: Vec<LValue>,
}

impl ArbitraryProcess {
    pub fn new(var: ExecutionVar<LValue>) -> Self {
        Self { var, set: vec![] }
    }
    pub fn set_set(&mut self, set: Vec<LValue>) {
        self.set = set
    }

    pub fn set_var(&mut self, var: LValue) -> (PlanVarId, cst::Cst) {
        self.var.set_val(var)
    }

    pub fn get_plan_var_id(&self) -> PlanVarId {
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

/*
#[derive(Clone)]
pub struct ArbitraryTrace {
    pub possibilities: Vec<LValue>,
    pub choice: ArbitraryChoice,
    pub instant: Timepoint,
}

#[derive(Clone)]
pub enum ArbitraryChoice {
    Planning(LValue),
    Execution(LValue),
}

impl ArbitraryChoice {
    pub fn inner(&self) -> &LValue {
        match self {
            Self::Planning(lv) | Self::Execution(lv) => lv,
        }
    }

    pub fn is_planned(&self) -> bool {
        matches!(self, Self::Planning(..))
    }
}

impl Display for ArbitraryChoice {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ArbitraryChoice::Planning(lv) => write!(f, "planning({lv})"),
            ArbitraryChoice::Execution(lv) => write!(f, "execution({lv})"),
        }
    }
}

impl Display for ArbitraryTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\narb{}\n => {}",
            self.instant,
            LValue::from(&self.possibilities),
            self.choice
        )
    }
}*/
