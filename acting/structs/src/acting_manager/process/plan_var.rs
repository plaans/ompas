use crate::acting_manager::interval::Timepoint;
use crate::acting_manager::AMId;
use crate::sym_table::domain::cst;
use crate::sym_table::domain::cst::Cst;
use crate::sym_table::VarId;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct PlanVar {
    var_id: VarId,
    om_id: AMId,
    value: ActingVal,
}

impl PlanVar {
    pub fn new(var_id: VarId, om_id: AMId) -> PlanVar {
        Self {
            var_id,
            om_id,
            value: ActingVal::None,
        }
    }

    pub fn set_execution_val(&mut self, val: cst::Cst) {
        self.value = ActingVal::Execution(val)
    }

    pub fn get_val(&self) -> &ActingVal {
        &self.value
    }

    pub fn set_planned_val(&mut self, val: cst::Cst) {
        self.value = ActingVal::Planned(val)
    }
}

#[derive(Clone)]
pub enum ActingVal {
    Execution(cst::Cst),
    Planned(cst::Cst),
    None,
}

pub type PlanVarId = usize;

pub struct PlanVal {
    pub(crate) plan_var_id: PlanVarId,
    pub(crate) val: cst::Cst,
}

#[derive(Clone, Default)]
pub struct ExecutionVar<T: Display + Clone + AsCst> {
    plan_var_id: Option<PlanVarId>,
    val: Option<T>,
}

pub trait AsCst {
    fn as_cst(&self) -> cst::Cst;
}

impl<T: Display + Clone + AsCst> ExecutionVar<T> {
    pub fn new(plan_var_id: Option<PlanVarId>) -> Self {
        Self {
            plan_var_id,
            val: None,
        }
    }

    pub fn get_plan_var_id(&self) -> Option<PlanVarId> {
        self.plan_var_id
    }

    pub fn set_val(&mut self, val: T) -> Option<PlanVal> {
        let cst = val.as_cst();
        self.val = Some(val);
        if let Some(plan_var_id) = self.plan_var_id {
            Some(PlanVal {
                plan_var_id,
                val: cst,
            })
        } else {
            None
        }
    }

    pub fn get_val(&mut self) -> &Option<T> {
        &self.val
    }
}

impl<T: Display + Clone + AsCst> Display for ExecutionVar<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.val {
            Some(val) => write!(f, "{}", val),
            None => write!(f, ""),
        }
    }
}

impl AsCst for LValue {
    fn as_cst(&self) -> Cst {
        match self {
            LValue::Symbol(s) => Cst::Symbol(s.to_string()),
            LValue::Number(n) => match n {
                LNumber::Int(i) => Cst::Int(*i),
                LNumber::Float(f) => Cst::Float(*f),
            },
            LValue::True => Cst::Bool(true),
            LValue::Nil => Cst::Bool(false),
            lv => panic!("{} cannot be converted as cst", lv),
        }
    }
}

impl AsCst for Timepoint {
    ///Transform the timepoint as float representing seconds
    fn as_cst(&self) -> Cst {
        Cst::Float(self.as_secs())
    }
}

impl AsCst for usize {
    fn as_cst(&self) -> Cst {
        Cst::Int(*self as i64)
    }
}

impl AsCst for String {
    fn as_cst(&self) -> Cst {
        Cst::Symbol(self.to_string())
    }
}
