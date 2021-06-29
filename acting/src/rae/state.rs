use crate::rae::context::Status;
use crate::rae::refinement::Assignment;
use im::HashMap;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LValue, LValueS};
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::ptr::write_bytes;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};

pub const KEY_DYNAMIC: &str = "dynamic";
pub const KEY_STATIC: &str = "static";
pub const KEY_INNER_WORLD: &str = "inner-world";

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq)]
pub enum StateType {
    Static,
    Dynamic,
    InnerWorld,
}

#[derive(Clone, Default, Debug)]
pub struct LState {
    pub inner: im::HashMap<LValueS, LValueS>,
    pub _type: Option<StateType>,
}

impl From<&LState> for im::HashMap<LValueS, LValueS> {
    fn from(ls: &LState) -> Self {
        ls.inner.clone()
    }
}
impl From<LState> for im::HashMap<LValueS, LValueS> {
    fn from(ls: LState) -> Self {
        (&ls).into()
    }
}
impl From<&im::HashMap<LValueS, LValueS>> for LState {
    fn from(m: &HashMap<LValueS, LValueS>) -> Self {
        Self {
            inner: m.clone(),
            _type: None,
        }
    }
}
impl From<im::HashMap<LValueS, LValueS>> for LState {
    fn from(m: HashMap<LValueS, LValueS>) -> Self {
        (&m).into()
    }
}

impl LState {
    pub fn get(&self, key: &LValueS) -> Option<&LValueS> {
        self.inner.get(key)
    }

    pub fn set_type(&mut self, _type: StateType) {
        self._type = Some(_type)
    }

    pub fn get_type(&self) -> &Option<StateType> {
        &self._type
    }

    pub fn insert(&mut self, key: LValueS, value: LValueS) {
        self.inner.insert(key, value);
    }

    pub fn remove(&mut self, key: &LValueS) -> Option<LValueS> {
        self.inner.remove(&key)
    }

    pub fn union(&self, other: &Self) -> Self {
        LState {
            inner: self.inner.clone().union(other.inner.clone()),
            _type: None,
        }
    }

    pub fn append(&mut self, other: &LState) {
        let _ = self.inner.clone().union(other.inner.clone());
    }

    pub fn into_map(self) -> LValue {
        let mut map: im::HashMap<LValue, LValue> = Default::default();
        for e in &self.inner {
            map.insert(e.0.into(), e.1.into());
        }
        map.into()
    }
}

#[derive(Default, Debug, Clone)]
pub struct RAEState {
    _static: Arc<RwLock<LState>>,
    dynamic: Arc<RwLock<LState>>,
    inner_world: Arc<RwLock<LState>>,
}

impl RAEState {
    pub fn get_state(&self, _type: Option<StateType>) -> LState {
        match _type {
            None => self.inner_world.read().unwrap().union(
                &self
                    ._static
                    .read()
                    .unwrap()
                    .union(&self.dynamic.read().unwrap()),
            ),
            Some(_type) => match _type {
                StateType::Static => self._static.read().unwrap().deref().clone(),
                StateType::Dynamic => self.dynamic.read().unwrap().deref().clone(),
                StateType::InnerWorld => self.inner_world.read().unwrap().deref().clone(),
            },
        }
    }

    pub fn set_state(&self, state: LState) {
        match &state._type {
            None => {}
            Some(_type) => match _type {
                StateType::Static => {
                    let mut _ref = self._static.write().unwrap();
                    _ref.inner = state.inner;
                }
                StateType::Dynamic => {
                    let mut _ref = self.dynamic.write().unwrap();
                    _ref.inner = state.inner;
                }
                StateType::InnerWorld => {
                    let mut _ref = self.inner_world.write().unwrap();
                    _ref.inner = state.inner;
                }
            },
        }
    }

    pub fn add_fact(&self, key: LValueS, value: LValueS) {
        self.inner_world.write().unwrap().insert(key, value)
    }

    pub fn retract_fact(&self, key: LValueS, value: LValueS) -> Result<LValue, LError> {
        let old_value = self.inner_world.read().unwrap().get(&key).cloned();
        match old_value {
            None => Err(SpecialError("key is not in state".to_string())),
            Some(old_value) => {
                if old_value == value {
                    self.inner_world.write().unwrap().remove(&key);
                    Ok(LValue::Nil)
                } else {
                    Err(SpecialError("there is no such fact in state".to_string()))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ActionStatus {
    ActionPending,
    ActionResponse(usize),
    ActionFeedback(f64), //Progress of the action
    ActionResult(bool),  //True the action is a success, false the action is a failure
    ActionPreempt,
    ActionCancel(bool), //True the action has been successfully stopped, false it was a failure to cancel
}

impl From<ActionStatus> for Status {
    fn from(_as: ActionStatus) -> Self {
        match _as {
            ActionStatus::ActionPending => Status::Running,
            ActionStatus::ActionResponse(_) => Status::Running,
            ActionStatus::ActionFeedback(_) => Status::Running,
            ActionStatus::ActionResult(b) => match b {
                true => Status::Done,
                false => Status::Failure,
            },
            ActionStatus::ActionPreempt => Status::Running,
            ActionStatus::ActionCancel(b) => match b {
                true => Status::Done,
                false => Status::Failure,
            },
        }
    }
}

impl Display for ActionStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ActionStatus::ActionPending => write!(f, "action pending"),
            ActionStatus::ActionResponse(id) => write!(f, "action response: {}", id),
            ActionStatus::ActionFeedback(fl) => write!(f, "action feedback: {}", fl),
            ActionStatus::ActionResult(r) => write!(f, "action result: {}", r),
            ActionStatus::ActionPreempt => write!(f, "action preempt"),
            ActionStatus::ActionCancel(r) => write!(f, "action cancel {}", r),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ActionStatusSet {
    pub server_id_internal_id: Arc<RwLock<im::HashMap<usize, usize>>>,
    pub status: Arc<RwLock<im::HashMap<usize, ActionStatus>>>,
    next_id: Arc<AtomicUsize>,
}

impl ActionStatusSet {
    pub fn get_new_id(&self) -> usize {
        let result = self.next_id.load(Ordering::Relaxed);
        let new_value = result + 1;
        self.next_id.store(new_value, Ordering::Relaxed);
        result
    }

    pub fn set_status(&mut self, internal_id: usize, status: ActionStatus) {
        self.status.write().unwrap().insert(internal_id, status);
    }

    pub fn set_status_from_server(&mut self, server_id: usize, status: ActionStatus) {
        let id = *self
            .server_id_internal_id
            .read()
            .unwrap()
            .get(&server_id)
            .unwrap();
        self.status.write().unwrap().insert(id, status);
    }

    pub fn get_status(&self, internal_id: &usize) -> Option<ActionStatus> {
        self.status.read().unwrap().get(internal_id).cloned()
    }

    pub fn get_status_from_server(&self, server_id: usize) -> Option<ActionStatus> {
        match self.server_id_internal_id.read().unwrap().get(&server_id) {
            None => None,
            Some(id) => self.status.read().unwrap().get(id).cloned(),
        }
    }

    pub fn pretty_print(&self) -> String {
        let mut str = String::new();
        str.push_str("Action(s) Status:\n");
        let status = self.status.read().unwrap().clone();
        let server_id_internal_id = self.server_id_internal_id.read().unwrap().clone();
        for e in &server_id_internal_id {
            str.push_str(format!("- {}({}): {:?}\n", e.1, e.0, status.get(&e.1).unwrap()).as_str());
        }
        str
    }
}
