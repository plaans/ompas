use crate::state::instance::InstanceCollection;
use crate::state::partial_state::PartialState;
use crate::sym_table::domain::ref_type_lattice::RefTypeLattice;
use crate::sym_table::domain::type_lattice::TypeLattice;
use sompas_core::modules::map::union_map;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::ops::Deref;
use std::sync::Arc;
use tokio::sync::{broadcast, Mutex, RwLock};

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq)]
pub enum StateType {
    Static,
    Dynamic,
    InnerStatic,
    InnerDynamic,
    Instance,
}

#[derive(Default, Debug, Clone)]
pub struct WorldState {
    r#static: Arc<RwLock<PartialState>>,
    dynamic: Arc<RwLock<PartialState>>,
    inner_static: Arc<RwLock<PartialState>>,
    inner_dynamic: Arc<RwLock<PartialState>>,
    instance: Arc<RwLock<InstanceCollection>>,
    sem_update: Arc<Mutex<Option<broadcast::Sender<bool>>>>,
}

#[derive(Default, Clone, Debug)]
pub struct WorldStateSnapshot {
    pub r#static: PartialState,
    pub dynamic: PartialState,
    pub inner_static: PartialState,
    pub inner_dynamic: PartialState,
    pub instance: InstanceCollection,
}

impl WorldStateSnapshot {
    pub fn get_state(&self, _type: Option<StateType>) -> PartialState {
        match _type {
            None => self.inner_static.union(
                &self.inner_dynamic.union(
                    &self.r#static.union(
                        &self
                            .dynamic
                            .union(&PartialState::from(self.instance.clone())),
                    ),
                ),
            ),
            Some(_type) => match _type {
                StateType::Static => self.r#static.union(&self.inner_static.clone()),
                StateType::Dynamic => self.dynamic.union(&self.inner_dynamic.clone()),
                StateType::Instance => self.instance.clone().into(),
                StateType::InnerStatic => self.inner_static.clone(),
                StateType::InnerDynamic => self.inner_dynamic.clone(),
            },
        }
    }
}

impl From<WorldStateSnapshot> for WorldState {
    fn from(w: WorldStateSnapshot) -> Self {
        Self {
            r#static: Arc::new(RwLock::new(w.r#static)),
            dynamic: Arc::new(RwLock::new(w.dynamic)),
            inner_static: Arc::new(RwLock::new(w.inner_static)),
            inner_dynamic: Arc::new(RwLock::new(w.inner_dynamic)),
            instance: Arc::new(RwLock::new(w.instance)),
            sem_update: Arc::new(Mutex::new(None)),
        }
    }
}

impl From<WorldStateSnapshot> for LValue {
    fn from(r: WorldStateSnapshot) -> Self {
        let env = Default::default();
        union_map(
            &env,
            &[
                union_map(
                    &env,
                    &[
                        union_map(
                            &env,
                            &[
                                PartialState::from(r.instance).into_map(),
                                r.dynamic.into_map(),
                            ],
                        )
                        .unwrap(),
                        r.r#static.into_map(),
                    ],
                )
                .unwrap(),
                union_map(
                    &env,
                    &[r.inner_static.into_map(), r.inner_dynamic.into_map()],
                )
                .unwrap(),
            ],
        )
        .unwrap()
    }
}

const RAE_STATE_SEM_UPDATE_CHANNEL_SIZE: usize = 64;

impl WorldState {
    pub async fn add_type(&self, t: &str, p: Option<&str>) {
        self.instance.write().await.add_type(t, p).await;
    }

    pub async fn add_instance(&self, instance: &str, r#type: &str) {
        self.instance
            .write()
            .await
            .add_instance(instance, r#type)
            .await;
    }

    pub async fn instance(&self, i: &str, t: &str) -> LValue {
        self.instance.read().await.is_of_type(i, t).await.into()
    }

    pub async fn instances(&self, t: &str) -> LValue {
        self.instance.read().await.get_instances(t).await.into()
    }

    pub async fn clear(&self) {
        self.r#static.write().await.inner = Default::default();
        self.dynamic.write().await.inner = Default::default();
        *self.instance.write().await = Default::default();
        self.inner_static.write().await.inner = Default::default();
        self.inner_dynamic.write().await.inner = Default::default();
    }

    pub async fn get_snapshot(&self) -> WorldStateSnapshot {
        WorldStateSnapshot {
            r#static: self.r#static.read().await.clone(),
            dynamic: self.dynamic.read().await.clone(),
            inner_static: self.inner_static.read().await.clone(),
            inner_dynamic: self.inner_dynamic.read().await.clone(),
            instance: self.instance.read().await.clone(),
        }
    }

    pub async fn subscribe_on_update(&self) -> broadcast::Receiver<bool> {
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
            None => self.inner_static.read().await.union(
                &self.inner_dynamic.read().await.union(
                    &self.r#static.read().await.union(
                        &self
                            .dynamic
                            .read()
                            .await
                            .union(&PartialState::from(self.instance.read().await.clone())),
                    ),
                ),
            ),
            Some(_type) => match _type {
                StateType::Static => self
                    .r#static
                    .read()
                    .await
                    .union(&self.inner_static.read().await.clone()),
                StateType::Dynamic => self
                    .dynamic
                    .read()
                    .await
                    .union(&self.inner_dynamic.read().await.clone()),
                StateType::Instance => self.instance.read().await.clone().into(),
                StateType::InnerStatic => self.inner_static.read().await.clone(),
                StateType::InnerDynamic => self.inner_dynamic.read().await.clone(),
            },
        }
    }

    pub async fn update_state(&self, state: PartialState) {
        match &state._type {
            None => {}
            Some(_type) => match _type {
                StateType::Static => {
                    let mut r#static = self.r#static.write().await;
                    let r#static = &mut r#static.inner;
                    for (k, v) in state.inner {
                        r#static.insert(k, v);
                    }
                }
                StateType::Dynamic => {
                    let mut dynamic = self.dynamic.write().await;
                    let dynamic = &mut dynamic.inner;
                    for (k, v) in state.inner {
                        dynamic.insert(k, v);
                    }
                }
                StateType::InnerStatic => {
                    let mut inner_world = self.inner_static.write().await;
                    let inner_world = &mut inner_world.inner;
                    for (k, v) in state.inner {
                        inner_world.insert(k, v);
                    }
                }
                StateType::InnerDynamic => {
                    let mut inner_world = self.inner_dynamic.write().await;
                    let inner_world = &mut inner_world.inner;
                    for (k, v) in state.inner {
                        inner_world.insert(k, v);
                    }
                }
                StateType::Instance => {
                    panic!()
                    /*let new_state = self.instance.write().await.inner.union(&state).inner;
                    self.instance.write().await.inner.inner = new_state;*/
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
                    let mut _ref = self.r#static.write().await;
                    _ref.inner = state.inner;
                }
                StateType::Dynamic => {
                    let mut _ref = self.dynamic.write().await;
                    _ref.inner = state.inner;
                }
                StateType::InnerStatic => {
                    let mut _ref = self.inner_static.write().await;
                    _ref.inner = state.inner;
                }
                StateType::InnerDynamic => {
                    let mut _ref = self.inner_dynamic.write().await;
                    _ref.inner = state.inner;
                }
                StateType::Instance => {
                    panic!()
                    //self.instance.write().await.inner.inner = state.inner;
                }
            },
        }
        self.trigger_update_event().await;
    }

    pub async fn add_fact(&self, key: LValueS, value: LValueS) {
        self.inner_dynamic.write().await.insert(key, value)
    }

    pub async fn retract_fact(&self, key: LValueS, value: LValueS) -> Result<(), LRuntimeError> {
        let old_value = self.inner_dynamic.read().await.get(&key).cloned();
        match old_value {
            None => Err(lruntimeerror!(
                "RAEState::retract_fact",
                "key is not in state"
            )),
            Some(old_value) => {
                if old_value == value {
                    self.inner_dynamic.write().await.remove(&key);
                    Ok(())
                } else {
                    Err(lruntimeerror!(
                        "RAEState::retract_fact",
                        "there is no such fact in state"
                    ))
                }
            }
        }
    }

    pub async fn get_lattice(&self) -> TypeLattice {
        self.instance.read().await.get_lattice().await
    }

    pub async fn get_ref_lattice(&self) -> RefTypeLattice {
        self.instance.read().await.get_ref_lattice()
    }
}
