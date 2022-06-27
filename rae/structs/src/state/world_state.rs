use crate::state::partial_state::PartialState;
use sompas_core::modules::map::union_map;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::ops::Deref;
use std::sync::Arc;
use tokio::sync::{broadcast, Mutex, RwLock};

pub const KEY_DYNAMIC: &str = "dynamic";
pub const KEY_STATIC: &str = "static";
pub const KEY_INNER_WORLD: &str = "inner-world";
pub const KEY_INSTANCE: &str = "instance";

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq)]
pub enum StateType {
    Static,
    Dynamic,
    InnerWorld,
    Instance,
}

#[derive(Default, Debug, Clone)]
pub struct WorldState {
    _static: Arc<RwLock<PartialState>>,
    dynamic: Arc<RwLock<PartialState>>,
    inner_world: Arc<RwLock<PartialState>>,
    instance: Arc<RwLock<PartialState>>,
    sem_update: Arc<Mutex<Option<broadcast::Sender<bool>>>>,
}

#[derive(Default, Clone, Debug)]
pub struct WorldStateSnapshot {
    pub _static: PartialState,
    pub dynamic: PartialState,
    pub inner_world: PartialState,
    pub instance: PartialState,
}

impl From<WorldStateSnapshot> for LValue {
    fn from(r: WorldStateSnapshot) -> Self {
        let env = Default::default();
        union_map(
            &env,
            &[
                union_map(&env, &[r.instance.into_map(), r.dynamic.into_map()]).unwrap(),
                union_map(&env, &[r._static.into_map(), r.inner_world.into_map()]).unwrap(),
            ],
        )
        .unwrap()
    }
}

const RAE_STATE_SEM_UPDATE_CHANNEL_SIZE: usize = 64;

impl WorldState {
    pub async fn get_snapshot(&self) -> WorldStateSnapshot {
        WorldStateSnapshot {
            _static: self._static.read().await.clone(),
            dynamic: self.dynamic.read().await.clone(),
            inner_world: self.inner_world.read().await.clone(),
            instance: self.instance.read().await.clone(),
        }
    }

    pub async fn subscribe_on_update(&mut self) -> broadcast::Receiver<bool> {
        let mut sem_update = self.sem_update.lock().await;
        let (tx, rx) = match sem_update.deref() {
            None => broadcast::channel(RAE_STATE_SEM_UPDATE_CHANNEL_SIZE),
            Some(b) => return b.subscribe(),
        };
        let _ = sem_update.insert(tx);
        rx
    }

    async fn trigger_update_event(&self) {
        if let Some(b) = self.sem_update.lock().await.deref() {
            if b.receiver_count() > 0 && b.send(true).is_err() {
                println!("could not broadcast update on state")
            }
        }
    }

    pub async fn get_state(&self, _type: Option<StateType>) -> PartialState {
        match _type {
            None => self.inner_world.read().await.union(
                &self._static.read().await.union(
                    &self
                        .dynamic
                        .read()
                        .await
                        .union(self.instance.read().await.deref()),
                ),
            ),
            Some(_type) => match _type {
                StateType::Static => self._static.read().await.clone(),
                StateType::Dynamic => self.dynamic.read().await.clone(),
                StateType::InnerWorld => self.inner_world.read().await.clone(),
                StateType::Instance => self.instance.read().await.clone(),
            },
        }
    }

    pub async fn update_state(&self, state: PartialState) {
        match &state._type {
            None => {}
            Some(_type) => match _type {
                StateType::Static => {
                    let new_state = self._static.write().await.union(&state).inner;
                    self._static.write().await.inner = new_state;
                }
                StateType::Dynamic => {
                    let new_state = self.dynamic.write().await.union(&state).inner;
                    self.dynamic.write().await.inner = new_state;
                }
                StateType::InnerWorld => {
                    let new_state = self.inner_world.write().await.union(&state).inner;
                    self.inner_world.write().await.inner = new_state;
                }
                StateType::Instance => {
                    let new_state = self.instance.write().await.union(&state).inner;
                    self.instance.write().await.inner = new_state;
                }
            },
        }
        self.trigger_update_event().await;
    }

    pub async fn set_state(&self, state: PartialState) {
        match &state._type {
            None => panic!("no type for state"),
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
                StateType::Instance => {
                    let mut _ref = self.instance.write().await;
                    _ref.inner = state.inner;
                }
            },
        }
        self.trigger_update_event().await;
    }

    pub async fn add_fact(&self, key: LValueS, value: LValueS) {
        self.inner_world.write().await.insert(key, value)
    }

    pub async fn retract_fact(
        &self,
        key: LValueS,
        value: LValueS,
    ) -> Result<LValue, LRuntimeError> {
        let old_value = self.inner_world.read().await.get(&key).cloned();
        match old_value {
            None => Err(lruntimeerror!(
                "RAEState::retract_fact",
                "key is not in state"
            )),
            Some(old_value) => {
                if old_value == value {
                    self.inner_world.write().await.remove(&key);
                    Ok(LValue::True)
                } else {
                    Err(lruntimeerror!(
                        "RAEState::retract_fact",
                        "there is no such fact in state"
                    ))
                }
            }
        }
    }
}
