use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::AMId;
use sompas_structs::contextcollection::AsyncLTrait;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;

pub type ActingVarId = usize;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct PlanVarRef {
    pub(in crate::ompas::manager::acting) var_id: VarId,
    pub(in crate::ompas::manager::acting) am_id: AMId,
}

impl From<VarId> for PlanVarRef {
    fn from(value: VarId) -> Self {
        Self {
            var_id: value,
            am_id: 0,
        }
    }
}

impl From<&VarId> for PlanVarRef {
    fn from(value: &VarId) -> Self {
        Self::from(*value)
    }
}

impl PlanVarRef {
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

pub struct ActingVar {
    refs: Vec<PlanVarRef>,
    value: ActingVal,
    val_t: Option<Box<AsyncLTrait>>,
}

impl Display for ActingVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            ActingVal::Execution(e) => write!(f, "{}", e),
            ActingVal::Planned(p) => write!(f, "~{}", p),
            ActingVal::None => write!(f, ""),
        }
    }
}

impl ActingVar {
    pub fn new<T: Clone + AsCst + Any + PartialEq + Display + Debug + Sync + Send>(
        refs: Vec<PlanVarRef>,
    ) -> Self {
        Self {
            refs,
            value: ActingVal::None,
            val_t: None,
        }
    }

    pub fn refs(&self) -> &Vec<PlanVarRef> {
        &self.refs
    }

    pub fn add_ref(&mut self, var_id: VarId, am_id: AMId) {
        self.refs.push(PlanVarRef { var_id, am_id })
    }

    pub fn set_execution_val<T: Clone + AsCst + Any + Send + Sync + PartialEq + Display + Debug>(
        &mut self,
        val: T,
    ) {
        match &self.val_t {
            None => {
                self.value = ActingVal::Execution(val.as_cst().unwrap());
                self.val_t = Some(Box::new(val));
            }
            Some(v) => {
                let v: T = v.downcast_ref::<T>().cloned().unwrap();
                assert_eq!(v, val)
            }
        }

        //self.value = ActingVal::Execution(val)
    }

    pub fn get_acting_val(&self) -> &ActingVal {
        &self.value
    }

    pub fn get_val<T: Clone + AsCst + Any + Send + Sync + PartialEq + Display + Debug>(
        &self,
    ) -> Option<T> {
        self.val_t
            .as_ref()
            .map(|t| t.downcast_ref::<T>().cloned().unwrap())
    }

    pub fn set_planned_val(&mut self, val: Cst) {
        match self.value {
            ActingVal::None | ActingVal::Planned(_) => self.value = ActingVal::Planned(val),
            ActingVal::Execution(_) => {}
        }
    }
}

#[derive(Clone)]
pub enum ActingVal {
    Execution(Cst),
    Planned(Cst),
    None,
}

impl AsCst for ActingVal {
    fn as_cst(&self) -> Option<Cst> {
        match self {
            ActingVal::Execution(c) => Some(c.clone()),
            ActingVal::Planned(c) => Some(c.clone()),
            ActingVal::None => None,
        }
    }
}

pub struct ActingValUpdate {
    pub(crate) acting_var_id: ActingVarId,
    pub(crate) val: Cst,
}

#[derive(Debug, Clone, Default)]
pub struct ActingVarRef<T: Any + Send + Sync + AsCst + Display + Debug + PartialEq> {
    pub id: ActingVarId,
    phantom: PhantomData<T>,
}

pub trait AsCst {
    fn as_cst(&self) -> Option<Cst>;
}

impl AsCst for Cst {
    fn as_cst(&self) -> Option<Cst> {
        Some(self.clone())
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

impl AsCst for LValueS {
    fn as_cst(&self) -> Option<Cst> {
        Some(match self {
            LValueS::Symbol(s) => Cst::Symbol(s.to_string()),
            LValueS::Int(i) => Cst::Int(*i),
            LValueS::Float(f) => Cst::Float(*f),
            LValueS::Bool(b) => Cst::Bool(*b),
            lvs => panic!("{} cannot be converted as cst", lvs),
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
    pub(crate) acting_vars: Vec<ActingVar>,
    pub(crate) map_ref: HashMap<PlanVarRef, ActingVarId>,
}

impl ActingVarCollection {
    pub fn new_acting_var_with_ref<
        T: Any + Send + Sync + AsCst + Display + Debug + PartialEq + Clone,
    >(
        &mut self,
        var_ref: PlanVarRef,
    ) -> ActingVarRef<T> {
        let id = self.new_acting_var::<T>();
        self.add_new_plan_var_ref(&id.id, var_ref);
        id
    }

    pub fn new_acting_var<T: Any + Send + Sync + AsCst + Display + Debug + PartialEq + Clone>(
        &mut self,
    ) -> ActingVarRef<T> {
        let id = self.acting_vars.len();
        self.acting_vars.push(ActingVar::new::<T>(vec![]));

        ActingVarRef {
            id,
            phantom: Default::default(),
        }
    }

    pub(crate) fn add_new_plan_var_ref(
        &mut self,
        acting_var_id: &ActingVarId,
        plan_var_ref: PlanVarRef,
    ) {
        self.acting_vars
            .get_mut(*acting_var_id)
            .unwrap()
            .refs
            .push(plan_var_ref);
        self.map_ref.insert(plan_var_ref, *acting_var_id);
    }

    pub(crate) fn set_execution_val<
        T: Clone + AsCst + Any + Send + Sync + PartialEq + Display + Debug,
    >(
        &mut self,
        acting_var_ref: &ActingVarRef<T>,
        val: T,
    ) -> Vec<PlanVarRef> {
        let var = self.acting_vars.get_mut(acting_var_ref.id).unwrap();
        var.set_execution_val::<T>(val);
        var.refs.clone()
    }

    pub fn set_planned_val(&mut self, update: ActingValUpdate) {
        self.acting_vars
            .get_mut(update.acting_var_id)
            .unwrap()
            .set_planned_val(update.val);
    }

    pub fn get_var(&self, id: &ActingVarId) -> Option<&ActingVar> {
        self.acting_vars.get(*id)
    }

    pub fn get_mut_var(&mut self, id: &ActingVarId) -> Option<&mut ActingVar> {
        self.acting_vars.get_mut(*id)
    }

    pub fn get_id(&self, var_ref: &PlanVarRef) -> Option<&ActingVarId> {
        self.map_ref.get(var_ref)
    }

    pub fn get_var_by_ref(&self, var_ref: &PlanVarRef) -> Option<&ActingVar> {
        self.get_id(var_ref).and_then(|id| self.get_var(id))
    }

    pub fn get_mut_var_by_ref(&mut self, var_ref: &PlanVarRef) -> Option<&mut ActingVar> {
        if let Some(&id) = self.get_id(var_ref) {
            self.get_mut_var(&id)
        } else {
            None
        }
    }

    pub fn get_val<T: Any + Sync + Send + Display + Clone + AsCst + PartialEq + std::fmt::Debug>(
        &self,
        r: &ActingVarRef<T>,
    ) -> Option<T> {
        self.acting_vars[r.id].get_val()
    }

    pub fn get_acting_val(&self, r: &ActingVarId) -> &ActingVal {
        self.acting_vars[*r].get_acting_val()
    }

    pub fn format_acting_var<
        T: Any + Sync + Send + Display + Clone + AsCst + PartialEq + std::fmt::Debug,
    >(
        &self,
        acting_var_ref: &ActingVarRef<T>,
    ) -> String {
        let var = &self.acting_vars[acting_var_ref.id];
        var.to_string()
    }

    pub fn format_slice_acting_var<
        T: Any + Sync + Send + Display + Clone + AsCst + PartialEq + std::fmt::Debug,
    >(
        &self,
        slice: &[ActingVarRef<T>],
    ) -> String {
        let mut out = "".to_string();
        for (i, var) in slice.iter().enumerate() {
            if i != 0 {
                out.push(' ');
            }
            let var = &self.acting_vars[var.id];
            out.push_str(var.to_string().as_str());
        }

        out
    }

    pub async fn clear(&mut self) {
        self.acting_vars.clear();
        self.map_ref.clear();
    }
}
