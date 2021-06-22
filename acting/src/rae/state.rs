use crate::rae::refinement::Assignment;
use ompas_lisp::structs::{LValue, LValueS};
use im::HashMap;
use std::fmt::{Display, Formatter};
use std::ptr::write_bytes;

#[derive(Clone, Debug)]
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
    pub fn set_type(&mut self, _type: StateType) {
        self._type = Some(_type)
    }

    pub fn get_type(&self) -> &Option<StateType> {
        &self._type
    }

    pub fn insert(&mut self, key: LValueS, value: LValueS) {
        self.inner.insert(key, value);
    }

    pub fn union(&self, other: &Self) -> Self {
        LState {
            inner: self.inner.clone().union(other.inner.clone()),
            _type: None
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
    _static : LState,
    dynamic: LState,
    inner_world: LState,
}


#[derive(Debug, Clone, Copy)]
pub enum ActionStatus {
    ActionPending,
    ActionResponse(usize),
    ActionFeedback(f64), //Progress of the action
    ActionResult(bool), //True the action is a success, false the action is a failure
    ActionPreempt,
    ActionCancel(bool), //True the action has been successfully stopped, false it was a failure to cancel
}

impl Display for ActionStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ActionStatus::ActionPending => write!(f, "action pending"),
            ActionStatus::ActionResponse(id) => write!(f, "action response: {}", id),
            ActionStatus::ActionFeedback(fl) => write!(f, "action feedback: {}", fl),
            ActionStatus::ActionResult(r) => write!(f, "action result: {}", r),
            ActionStatus::ActionPreempt => write!(f, "action preempt"),
            ActionStatus::ActionCancel(r) => write!(f, "action cancel {}", r)
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ActionStatusSet {
    pub server_id_interal_id: im::HashMap<usize, usize>,
    pub status: im::HashMap<usize, ActionStatus>
}

impl ActionStatusSet {
    pub fn set_status(&mut self, internal_id: usize, status: ActionStatus) {
        self.status.insert(internal_id, status);
    }

    pub fn set_status_from_server(&mut self, server_id: usize,status: ActionStatus) {
        let id = self.server_id_interal_id.get(&server_id).unwrap();
        self.status.insert(*id, status);
    }

    pub fn get_status(&self, internal_id: usize) -> Option<ActionStatus> {
        self.status.get(&internal_id).cloned()
    }

    pub fn get_status_from_server(&self, server_id: usize) -> Option<&ActionStatus> {
        match self.server_id_interal_id.get(&server_id) {
            None => None,
            Some(id) => self.status.get(id)
        }
    }

    pub fn pretty_print(&self) -> String {
        let mut str = String::new();
        str.push_str("Action(s) Status:\n");
        for e in &self.server_id_interal_id {
            str.push_str(format!("- {}({}): {:?}\n", e.1, e.0, self.status.get(&e.1).unwrap()).as_str());
        }
        str
    }
}
