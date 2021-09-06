use crate::rae::context::actions_progress::Status;
use im::HashMap;
use ompas_lisp::core::LEnv;
use ompas_lisp::functions::cons;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LValue, LValueS};
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use std::ptr::write_bytes;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::{broadcast, Mutex, RwLock};

pub const KEY_DYNAMIC: &str = "dynamic";
pub const KEY_STATIC: &str = "static";
pub const KEY_INNER_WORLD: &str = "inner-world";
const INSTANCE: &str = "instance";

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
        self.inner.remove(key)
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
    sem_update: Arc<Mutex<Option<broadcast::Sender<bool>>>>,
}

const RAE_STATE_SEM_UPDATE_CHANNEL_SIZE: usize = 64;

impl RAEState {
    pub async fn subscribe_on_update(&mut self) -> broadcast::Receiver<bool> {
        let mut sem_update = self.sem_update.lock().await;
        let (tx, rx) = match sem_update.deref() {
            None => broadcast::channel(RAE_STATE_SEM_UPDATE_CHANNEL_SIZE),
            Some(b) => return b.subscribe(),
        };
        sem_update.insert(tx);
        rx
    }

    async fn trigger_update_event(&self) {
        if let Some(b) = self.sem_update.lock().await.deref() {
            b.send(true).expect("todo!");
        }
    }

    pub async fn get_state(&self, _type: Option<StateType>) -> LState {
        match _type {
            None => self.inner_world.read().await.union(
                &self
                    ._static
                    .read()
                    .await
                    .union(self.dynamic.read().await.deref()),
            ),
            Some(_type) => match _type {
                StateType::Static => self._static.read().await.clone(),
                StateType::Dynamic => self.dynamic.read().await.clone(),
                StateType::InnerWorld => self.inner_world.read().await.clone(),
            },
        }
    }

    pub async fn update_state(&self, state: LState) {
        match &state._type {
            None => {}
            Some(_type) => match _type {
                StateType::Static => {
                    let new_state = self._static.write().await.union(&state).inner;
                    match self.check_for_instance_declaration(&state).await {
                        None => {}
                        Some(inner_world) => {
                            self.inner_world.write().await.inner = inner_world.inner;
                        }
                    }
                    self._static.write().await.inner = new_state;
                }
                StateType::Dynamic => {
                    let new_state = self.dynamic.write().await.union(&state).inner;
                    self.dynamic.write().await.inner = new_state;
                }
                StateType::InnerWorld => {
                    let new_state = self.dynamic.write().await.union(&state).inner;
                    self.dynamic.write().await.inner = new_state;
                }
            },
        }
        self.trigger_update_event().await;
    }

    pub async fn set_state(&self, state: LState) {
        match &state._type {
            None => {}
            Some(_type) => match _type {
                StateType::Static => {
                    let mut _ref = self._static.write().await;
                    _ref.inner = state.inner;
                }
                StateType::Dynamic => {
                    let mut _ref = self.dynamic.write().await;
                    _ref.inner = state.inner;
                }
                StateType::InnerWorld => {
                    let mut _ref = self.inner_world.write().await;
                    _ref.inner = state.inner;
                }
            },
        }
        self.trigger_update_event().await;
    }

    pub async fn add_fact(&self, key: LValueS, value: LValueS) {
        self.inner_world.write().await.insert(key, value)
    }

    pub async fn retract_fact(&self, key: LValueS, value: LValueS) -> Result<LValue, LError> {
        let old_value = self.inner_world.read().await.get(&key).cloned();
        match old_value {
            None => Err(SpecialError(
                "RAEState::retract_fact",
                "key is not in state".to_string(),
            )),
            Some(old_value) => {
                if old_value == value {
                    self.inner_world.write().await.remove(&key);
                    Ok(LValue::True)
                } else {
                    Err(SpecialError(
                        "RAEState::retract_fact",
                        "there is no such fact in state".to_string(),
                    ))
                }
            }
        }
    }

    async fn check_for_instance_declaration(&self, state: &LState) -> Option<LState> {
        for element in &state.inner {
            //let string_key = element.0.to_string();
            //println!("{}", string_key);
            let lvalue: LValue = element.0.into();

            if let LValue::List(list) = lvalue {
                if list.len() == 2 {
                    let first = &list[0];
                    let second = &list[1];
                    if let LValue::Symbol(string_key) = first {
                        match string_key.find(format!(".{}", INSTANCE).as_str()) {
                            None => {}
                            Some(index) => {
                                let string = &string_key[..index];
                                //println!("found new declaration of instance: {}", string);
                                let list_name = format!("{}s", string);
                                let list_name = LValue::from(list_name).into();
                                let list_instance =
                                    self.inner_world.read().await.get(&list_name).cloned();
                                let value = match list_instance {
                                    None => {
                                        //println!("list of instance not yet created");
                                        let value: LValue = vec![second.clone()].into();
                                        value.into()
                                    }
                                    Some(list_instance) => {
                                        //println!("list of instance found");
                                        let lvalue: LValue = list_instance.into();
                                        let list =
                                            cons(&[second.clone(), lvalue], &LEnv::default(), &())
                                                .unwrap();
                                        list.into()
                                    }
                                };
                                let key: LValue = list_name.into();
                                self.inner_world.write().await.insert(key.into(), value);
                            }
                        }
                    }
                }
            }
        }

        None
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
            ActionStatus::ActionPending => Status::Pending,
            ActionStatus::ActionResponse(_) => Status::Running,
            ActionStatus::ActionFeedback(_) => Status::Running,
            ActionStatus::ActionResult(b) => match b {
                true => Status::Done,
                false => Status::Failure,
            },
            ActionStatus::ActionPreempt => Status::Pending,
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
        loop {
            let id = self.next_id.load(Ordering::Relaxed);
            if self
                .next_id
                .compare_exchange(id, id + 1, Ordering::Acquire, Ordering::Relaxed) //Equivalent to compare_and_swap
                .is_ok()
            {
                return id;
            }
        }
    }

    pub async fn set_status(&mut self, internal_id: usize, status: ActionStatus) {
        self.status.write().await.insert(internal_id, status);
    }

    pub async fn set_status_from_server(&mut self, server_id: usize, status: ActionStatus) {
        let id = *self
            .server_id_internal_id
            .read()
            .await
            .get(&server_id)
            .unwrap();
        self.status.write().await.insert(id, status);
    }

    pub async fn get_status(&self, internal_id: &usize) -> Option<ActionStatus> {
        self.status.read().await.get(internal_id).cloned()
    }

    pub async fn get_status_from_server(&self, server_id: usize) -> Option<ActionStatus> {
        match self.server_id_internal_id.read().await.get(&server_id) {
            None => None,
            Some(id) => self.status.read().await.get(id).cloned(),
        }
    }

    pub async fn pretty_print(&self) -> String {
        let mut str = String::new();
        str.push_str("Action(s) Status:\n");
        let status = self.status.read().await.clone();
        let server_id_internal_id = self.server_id_internal_id.read().await.clone();
        for e in &server_id_internal_id {
            str.push_str(format!("- {}({}): {:?}\n", e.1, e.0, status.get(e.1).unwrap()).as_str());
        }
        str
    }
}
