use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::state::instance::InstanceCollection;
use crate::ompas::manager::state::partial_state::{Fact, PartialState};
use crate::ompas::manager::state::state_update_manager::{
    StateRule, StateUpdate, StateUpdateManager, StateUpdateSubscriber, SubscriberId,
};
use crate::ompas::manager::state::world_state_snapshot::{WorldState, WorldStateSnapshot};
use im::hashmap::Entry;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::sync::Arc;
use tokio::sync::RwLock;

pub mod action_status;
pub mod instance;
pub mod partial_state;
pub mod state_update_manager;
pub mod world_state_snapshot;

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq)]
pub enum StateType {
    Static,
    Dynamic,
    InnerStatic,
    InnerDynamic,
    Instance,
}

#[derive(Clone)]
pub struct StateManager {
    clock_manager: ClockManager,
    world_state: Arc<RwLock<WorldState>>,
    // r#static: Arc<RwLock<PartialState>>,
    // dynamic: Arc<RwLock<PartialState>>,
    // inner_static: Arc<RwLock<PartialState>>,
    // inner_dynamic: Arc<RwLock<PartialState>>,
    instance: Arc<RwLock<InstanceCollection>>,
    state_update_manager: Arc<RwLock<StateUpdateManager>>,
}

impl StateManager {
    pub fn new(clock_manager: ClockManager, st: RefSymTable) -> Self {
        Self {
            clock_manager,
            world_state: Arc::new(Default::default()),
            // r#static: Arc::new(Default::default()),
            // dynamic: Arc::new(Default::default()),
            // inner_static: Arc::new(Default::default()),
            // inner_dynamic: Arc::new(Default::default()),
            instance: Arc::new(RwLock::new(InstanceCollection::new(st))),
            state_update_manager: Arc::new(Default::default()),
        }
    }
}

impl From<WorldStateSnapshot> for StateManager {
    fn from(w: WorldStateSnapshot) -> Self {
        Self {
            clock_manager: Default::default(),
            world_state: Arc::new(RwLock::new(WorldState {
                r#static: w.r#static,
                dynamic: w.dynamic,
                inner_static: w.inner_static,
                inner_dynamic: w.inner_dynamic,
            })),
            // r#static: Arc::new(RwLock::new(w.r#static)),
            // dynamic: Arc::new(RwLock::new(w.dynamic)),
            // inner_static: Arc::new(RwLock::new(w.inner_static)),
            // inner_dynamic: Arc::new(RwLock::new(w.inner_dynamic)),
            instance: Arc::new(RwLock::new(w.instance)),
            state_update_manager: Arc::new(Default::default()),
        }
    }
}

impl StateManager {
    pub async fn add_type(&self, t: &str, p: Option<&str>) {
        self.instance.write().await.add_type(t, p);
    }

    pub async fn get_unk_of_type(&self, t: &str) -> String {
        self.instance.read().await.get_unk_of_type(t)
    }

    pub async fn add_instance(&self, instance: &str, r#type: &str) {
        self.instance.write().await.add_instance(instance, r#type);
        self.trigger_state_update(vec![LValueS::List(vec![
            "new-instance".into(),
            r#type.into(),
            instance.into(),
        ])])
        .await
    }

    pub async fn remove_instance(&self, instance: &str) {
        self.instance.write().await.remove_instance(instance)
    }

    pub async fn instance(&self, i: &str, t: &str) -> LValue {
        self.instance.write().await.is_of_type(i, t).into()
    }

    pub async fn instances(&self, t: &str) -> LValue {
        self.instance.write().await.get_instances(t).into()
    }

    pub async fn clear(&self) {
        self.world_state.write().await.clear();
        // self.r#static.write().await.inner = Default::default();
        // self.dynamic.write().await.inner = Default::default();
        self.instance.write().await.clear();
        // self.inner_static.write().await.inner = Default::default();
        // self.inner_dynamic.write().await.inner = Default::default();
    }

    pub async fn get_snapshot(&self) -> WorldStateSnapshot {
        let world_state: WorldState = self.world_state.read().await.clone();
        WorldStateSnapshot {
            r#static: world_state.r#static,
            dynamic: world_state.dynamic,
            inner_static: world_state.inner_static,
            inner_dynamic: world_state.inner_dynamic,
            instance: self.instance.read().await.get_snapshot(),
        }
    }

    pub async fn get_instance_collection(&self) -> InstanceCollection {
        self.instance.read().await.get_snapshot()
    }

    pub async fn new_subscriber(&self, rule: StateRule) -> StateUpdateSubscriber {
        self.state_update_manager.write().await.new_subscriber(rule)
    }

    pub async fn update_subscriber_rule(&self, subscriber_id: &SubscriberId, rule: StateRule) {
        self.state_update_manager
            .write()
            .await
            .set_rule(subscriber_id, rule)
    }

    async fn trigger_state_update(&self, updated: StateUpdate) {
        if !updated.is_empty() {
            self.state_update_manager
                .read()
                .await
                .check_updates_and_send_notifications(updated);
        }
    }

    pub async fn get_fact(&self, key: &LValueS, st: Option<StateType>) -> Option<Fact> {
        let world_state = self.world_state.read().await;
        match st {
            None => {
                if let Some(v) = world_state.dynamic.get(key) {
                    Some(v.clone())
                } else {
                    if let Some(v) = world_state.r#static.get(key) {
                        Some(v.clone())
                    } else {
                        if let Some(v) = world_state.inner_dynamic.get(key) {
                            Some(v.clone())
                        } else {
                            if let Some(v) = world_state.inner_static.get(key) {
                                Some(v.clone())
                            } else {
                                None
                            }
                        }
                    }
                }
            }
            Some(_type) => match _type {
                StateType::Static => {
                    if let Some(v) = world_state.r#static.get(key) {
                        Some(v.clone())
                    } else {
                        if let Some(v) = world_state.inner_static.get(key) {
                            Some(v.clone())
                        } else {
                            None
                        }
                    }
                }
                StateType::Dynamic => {
                    if let Some(v) = world_state.dynamic.get(key) {
                        Some(v.clone())
                    } else {
                        if let Some(v) = world_state.inner_dynamic.get(key) {
                            Some(v.clone())
                        } else {
                            None
                        }
                    }
                }
                //StateType::Instance => self.instance.read().await.inner.get(key.to).cloned(),
                StateType::InnerStatic => world_state.inner_static.get(key).cloned(),
                StateType::InnerDynamic => world_state.inner_dynamic.get(key).cloned(),
                _ => todo!(),
            },
        }
    }

    pub async fn get_state(&self, st: Option<StateType>) -> PartialState {
        let world_state = self.world_state.read().await;
        match st {
            None => world_state.dynamic.union(
                &world_state.inner_dynamic.union(
                    &world_state.r#static.union(
                        &world_state
                            .inner_static
                            .union(&PartialState::from(self.instance.read().await.clone())),
                    ),
                ),
            ),
            Some(_type) => match _type {
                StateType::Static => world_state.r#static.union(&world_state.inner_static),
                StateType::Dynamic => world_state.dynamic.union(&world_state.inner_dynamic),
                StateType::Instance => self.instance.read().await.clone().into(),
                StateType::InnerStatic => world_state.inner_static.clone(),
                StateType::InnerDynamic => world_state.inner_dynamic.clone(),
            },
        }
    }

    pub async fn update_state(&self, state: PartialState) {
        let mut updated = vec![];
        let time = self.clock_manager.now();
        let mut world_state = self.world_state.write().await;

        match &state._type {
            None => {}
            Some(_type) => match _type {
                StateType::Static => {
                    let r#static = &mut world_state.r#static.inner;
                    for (k, mut v) in state.inner {
                        v.date = Some(time);
                        let value = v.value.clone();
                        if let Some(old) = r#static.insert(k.clone(), v) {
                            if old.value != value {
                                updated.push(k.clone())
                            }
                        };
                    }
                }
                StateType::Dynamic => {
                    let dynamic = &mut world_state.dynamic.inner;
                    for (k, mut v) in state.inner {
                        v.date = Some(time);
                        let value = v.value.clone();
                        if let Some(old) = dynamic.insert(k.clone(), v) {
                            if old.value != value {
                                updated.push(k.clone())
                            }
                        };
                    }
                }
                StateType::InnerStatic => {
                    let inner_world = &mut world_state.inner_static.inner;
                    for (k, mut v) in state.inner {
                        v.date = Some(time);
                        let value = v.value.clone();
                        if let Some(old) = inner_world.insert(k.clone(), v) {
                            if old.value != value {
                                updated.push(k.clone())
                            }
                        };
                    }
                }
                StateType::InnerDynamic => {
                    let inner_world = &mut world_state.inner_dynamic.inner;
                    for (k, mut v) in state.inner {
                        v.date = Some(time);
                        let value = v.value.clone();
                        if let Some(old) = inner_world.insert(k.clone(), v) {
                            if old.value != value {
                                updated.push(k.clone())
                            }
                        };
                    }
                }
                StateType::Instance => {
                    panic!()
                    /*let new_state = self.instance.write().await.inner.union(&state).inner;
                    self.instance.write().await.inner.inner = new_state;*/
                }
            },
        }
        self.trigger_state_update(updated).await;
    }

    pub async fn set_state(&self, state: PartialState) {
        let updated = state.inner.keys().cloned().collect();
        let mut world_state = self.world_state.write().await;
        match &state._type {
            None => panic!("no type for state"),
            Some(_type) => match _type {
                StateType::Static => {
                    world_state.r#static.inner = state.inner;
                }
                StateType::Dynamic => {
                    world_state.dynamic.inner = state.inner;
                }
                StateType::InnerStatic => {
                    world_state.inner_static.inner = state.inner;
                }
                StateType::InnerDynamic => {
                    world_state.inner_dynamic.inner = state.inner;
                }
                StateType::Instance => {
                    panic!()
                }
            },
        }
        self.trigger_state_update(updated).await;
    }

    pub async fn add_fact(&self, key: LValueS, fact: Fact) {
        self.world_state
            .write()
            .await
            .inner_dynamic
            .insert(key.clone(), fact);
        self.trigger_state_update(vec![key]).await;
    }

    pub async fn add_value_with_date(&self, key: LValueS, value: LValueS) {
        let date = self.clock_manager.now();
        self.world_state
            .write()
            .await
            .inner_dynamic
            .insert(key.clone(), Fact::new(value, Some(date)));
        self.trigger_state_update(vec![key]).await;
    }

    pub async fn add_static(&self, key: LValueS, value: LValueS) -> Result<(), LRuntimeError> {
        match self
            .world_state
            .write()
            .await
            .inner_static
            .inner
            .entry(key.clone())
        {
            Entry::Occupied(_) => Err(LRuntimeError::new(
                "",
                format!("{} is already defined in the static state", key),
            )),
            Entry::Vacant(v) => {
                v.insert(Fact::new(value, None));
                Ok(())
            }
        }
    }

    pub async fn retract_fact(&self, key: LValueS, value: LValueS) -> Result<(), LRuntimeError> {
        let mut world_state = self.world_state.write().await;
        let old_value = world_state.inner_dynamic.get(&key).cloned();
        let v = match old_value {
            None => Err(lruntimeerror!(
                "RAEState::retract_fact",
                "key is not in state"
            )),
            Some(old_value) => {
                if old_value.value == value {
                    world_state.inner_dynamic.remove(&key);
                    Ok(())
                } else {
                    Err(lruntimeerror!(
                        "RAEState::retract_fact",
                        "there is no such fact in state"
                    ))
                }
            }
        };
        self.trigger_state_update(vec![key]).await;
        v
    }
}
