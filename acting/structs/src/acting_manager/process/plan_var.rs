use crate::acting_manager::interval::Timepoint;
use crate::acting_manager::AMId;
use crate::sym_table::domain::cst;
use crate::sym_table::domain::cst::Cst;
use crate::sym_table::VarId;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone)]
pub struct PlanRef {
    var_id: VarId,
    am_id: AMId,
}

#[derive(Clone)]
pub struct PlanVar {
    refs: Vec<PlanRef>,
    value: ActingVal,
}

impl Display for PlanVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            ActingVal::Execution(e) => write!(f, "{e}"),
            ActingVal::Planned(p) => write!(f, "~{p}"),
            ActingVal::None => write!(f, ""),
        }
    }
}

impl PlanVar {
    pub fn new() -> PlanVar {
        Self {
            refs: vec![],
            value: ActingVal::None,
        }
    }

    pub fn new_with_ref(var_id: &VarId, am_id: &AMId) -> Self {
        Self {
            refs: vec![PlanRef {
                var_id: *var_id,
                am_id: *am_id,
            }],
            value: ActingVal::None,
        }
    }

    pub fn add_ref(&mut self, var_id: VarId, am_id: AMId) {
        self.refs.push(PlanRef { var_id, am_id })
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

pub struct PlanValUpdate {
    pub(crate) plan_var_id: PlanVarId,
    pub(crate) val: cst::Cst,
}

#[derive(Debug, Clone, Default)]
pub struct ExecutionVar<T: Display + Clone + AsCst> {
    pub(crate) plan_var_id: Option<PlanVarId>,
    pub(crate) val: Option<T>,
}

pub trait AsCst {
    fn as_cst(&self) -> Option<cst::Cst>;
}

impl AsCst for Cst {
    fn as_cst(&self) -> Option<Cst> {
        Some(self.clone())
    }
}

impl<T: Display + Clone + AsCst> ExecutionVar<T> {
    pub fn new() -> Self {
        Self {
            plan_var_id: None,
            val: None,
        }
    }
    pub fn new_with_ref(plan_var_id: PlanVarId) -> Self {
        Self {
            plan_var_id: Some(plan_var_id),
            val: None,
        }
    }

    pub fn get_plan_var_id(&self) -> &Option<PlanVarId> {
        &self.plan_var_id
    }

    pub fn set_val(&mut self, val: T) -> Option<PlanValUpdate> {
        let cst = val.as_cst().unwrap();
        self.val = Some(val);
        self.plan_var_id.as_ref().map(|&plan_var_id| PlanValUpdate {
            plan_var_id,
            val: cst.clone(),
        })
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
    fn as_cst(&self) -> Option<Cst> {
        Some(match self {
            LValue::Symbol(s) => Cst::Symbol(s.to_string()),
            LValue::Number(n) => match n {
                LNumber::Int(i) => Cst::Int(*i),
                LNumber::Float(f) => Cst::Float(*f),
            },
            LValue::True => Cst::Bool(true),
            LValue::Nil => Cst::Bool(false),
            lv => panic!("{} cannot be converted as cst", lv),
        })
    }
}

impl AsCst for Timepoint {
    ///Transform the timepoint as float representing seconds
    fn as_cst(&self) -> Option<Cst> {
        Some(Cst::Float(self.as_secs()))
    }
}

impl AsCst for usize {
    fn as_cst(&self) -> Option<Cst> {
        Some(Cst::Int(*self as i64))
    }
}

impl AsCst for String {
    fn as_cst(&self) -> Option<Cst> {
        Some(Cst::Symbol(self.to_string()))
    }
}
