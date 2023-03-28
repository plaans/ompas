use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::AMId;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub type ActingVarId = usize;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct ActingVarRef {
    pub(in crate::ompas::manager::acting) var_id: VarId,
    pub(in crate::ompas::manager::acting) am_id: AMId,
}

impl From<VarId> for ActingVarRef {
    fn from(value: VarId) -> Self {
        Self {
            var_id: value,
            am_id: 0,
        }
    }
}

impl From<&VarId> for ActingVarRef {
    fn from(value: &VarId) -> Self {
        Self::from(*value)
    }
}

impl ActingVarRef {
    pub fn new(var_id: VarId, am_id: AMId) -> Self {
        Self { var_id, am_id }
    }

    pub fn var_id(&self) -> &VarId {
        &self.var_id
    }

    pub fn am_id(&self) -> &AMId {
        &self.am_id
    }
}

#[derive(Clone)]
pub struct ActingVar {
    refs: Vec<ActingVarRef>,
    value: ActingVal,
}

impl Display for ActingVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            ActingVal::Execution(e) => write!(f, "{e}"),
            ActingVal::Planned(p) => write!(f, "~{p}"),
            ActingVal::None => write!(f, ""),
        }
    }
}

impl ActingVar {
    pub fn new(refs: Vec<ActingVarRef>) -> Self {
        Self {
            refs,
            value: ActingVal::None,
        }
    }

    pub fn refs(&self) -> &Vec<ActingVarRef> {
        &self.refs
    }

    pub fn add_ref(&mut self, var_id: VarId, am_id: AMId) {
        self.refs.push(ActingVarRef { var_id, am_id })
    }

    pub fn set_execution_val(&mut self, val: Cst) {
        self.value = ActingVal::Execution(val)
    }

    pub fn get_val(&self) -> &ActingVal {
        &self.value
    }

    pub fn set_planned_val(&mut self, val: Cst) {
        self.value = ActingVal::Planned(val)
    }
}

#[derive(Clone)]
pub enum ActingVal {
    Execution(Cst),
    Planned(Cst),
    None,
}

pub struct ActingValUpdate {
    pub(crate) plan_var_id: ActingVarId,
    pub(crate) val: Cst,
}

#[derive(Debug, Clone, Default)]
pub struct ExecutionVar<T: Display + Clone + AsCst> {
    pub(crate) plan_var_id: Option<ActingVarId>,
    pub(crate) val: Option<T>,
}

pub trait AsCst {
    fn as_cst(&self) -> Option<Cst>;
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
    pub fn new_with_ref(plan_var_id: ActingVarId) -> Self {
        Self {
            plan_var_id: Some(plan_var_id),
            val: None,
        }
    }

    pub fn get_plan_var_id(&self) -> &Option<ActingVarId> {
        &self.plan_var_id
    }

    pub fn set_val(&mut self, val: T) -> Option<ActingValUpdate> {
        if self.val.is_none() {
            let cst = val.as_cst().unwrap();
            self.val = Some(val);
            self.plan_var_id
                .as_ref()
                .map(|&plan_var_id| ActingValUpdate {
                    plan_var_id,
                    val: cst.clone(),
                })
        } else {
            None
        }
    }

    pub fn get_val(&self) -> &Option<T> {
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

#[derive(Default)]
pub struct ActingVarCollection {
    pub(crate) plan_vars: Vec<ActingVar>,
    pub(crate) map_ref: HashMap<ActingVarRef, ActingVarId>,
}

impl ActingVarCollection {
    pub fn new_acting_var(&mut self, var_ref: ActingVarRef) -> ActingVarId {
        let id = self.plan_vars.len();
        self.plan_vars.push(ActingVar::new(vec![var_ref]));
        self.map_ref.insert(var_ref, id);

        id
    }

    pub fn get_var(&self, id: &ActingVarId) -> Option<&ActingVar> {
        self.plan_vars.get(*id)
    }

    pub fn get_mut_var(&mut self, id: &ActingVarId) -> Option<&mut ActingVar> {
        self.plan_vars.get_mut(*id)
    }

    pub fn get_id(&self, var_ref: &ActingVarRef) -> Option<&ActingVarId> {
        self.map_ref.get(var_ref)
    }

    pub fn get_var_by_ref(&self, var_ref: &ActingVarRef) -> Option<&ActingVar> {
        self.get_id(var_ref).and_then(|id| self.get_var(id))
    }

    pub fn get_mut_var_by_ref(&mut self, var_ref: &ActingVarRef) -> Option<&mut ActingVar> {
        if let Some(&id) = self.get_id(var_ref) {
            self.get_mut_var(&id)
        } else {
            None
        }
    }

    pub fn format_execution_var<T: Display + Clone + AsCst>(
        &self,
        execution_var: &ExecutionVar<T>,
    ) -> String {
        if let Some(val) = &execution_var.val {
            val.as_cst().unwrap().to_string()
        } else {
            if let Some(var) = &execution_var.plan_var_id {
                self.plan_vars[*var].to_string()
            } else {
                "".to_string()
            }
        }
    }

    pub async fn clear(&mut self) {
        self.plan_vars.clear();
        self.map_ref.clear();
    }
}
