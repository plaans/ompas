use crate::state::partial_state::PartialState;
use crate::state::world_state::{StateType, WorldStateSnapshot};
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_utils::other::get_and_update_id_counter;
use sompas_structs::list;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::sync::Mutex;

const UNARY: &str = "unary";
const ALL: &str = "all";
const NONE: &str = "none";

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Capacity {
    Unary,
    All,
    None,
    Some(usize),
}

impl Display for Capacity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Capacity::Unary => write!(f, "{}", UNARY),
            Capacity::All => write!(f, "{}", ALL),
            Capacity::None => write!(f, "{}", NONE),
            Capacity::Some(u) => write!(f, "{}", u),
        }
    }
}

impl From<usize> for Capacity {
    fn from(u: usize) -> Self {
        Self::Some(u)
    }
}

impl From<Capacity> for usize {
    fn from(value: Capacity) -> Self {
        match value {
            Capacity::Unary => 1,
            Capacity::All => 1,
            Capacity::None => 0,
            Capacity::Some(u) => u,
        }
    }
}

pub type ResourceId = usize;

pub struct ResourceHandler {
    label: String,
    resource_id: ResourceId,
    acquire_id: usize,
}

impl ResourceHandler {
    pub fn get_label(&self) -> &str {
        &self.label
    }
}

pub struct Resource {
    label: String,
    id: ResourceId,
    max_capacity: Capacity,
    capacity: Capacity,
    acquirers: HashMap<usize, Capacity>,
    acquire_id: usize,
    waiters: HashMap<usize, Waiter>,
    waiter_id: usize,
}

impl Resource {
    pub fn update_remaining_capacity(&mut self) -> Result<(), LRuntimeError> {
        let mut capacity = self.max_capacity;
        //let old_capacity = self.capacity;

        for c in self.acquirers.values() {
            match c {
                Capacity::Unary => panic!("An acquire cannot have acquired Unary"),
                Capacity::All => {
                    assert_eq!(self.acquirers.len(), 1);
                    capacity = Capacity::None;
                }
                Capacity::None => panic!("An acquire cannot have acquired None"),
                Capacity::Some(n) => {
                    capacity = match capacity {
                        Capacity::Unary => panic!("Cannot acquire some part of a unary resource"),
                        Capacity::Some(o) => Capacity::Some(o - n),
                        _ => panic!(""),
                    }
                }
            }
        }

        self.capacity = capacity;
        //println!("capacity {} -> {}", old_capacity, self.capacity);

        Ok(())
    }

    pub async fn update_waiters(&mut self) {
        let waiters: Vec<_> = self.waiters.iter_mut().map(|(k, v)| (k, v)).collect();
        let mut waiters: Vec<_> = waiters
            .iter()
            .filter(|(_, w)| match (self.capacity, w.capacity) {
                (Capacity::Unary, Capacity::All) => true,
                (Capacity::Some(u1), Capacity::Some(u2)) => u1 >= u2,
                _ => false,
            })
            .collect();

        if !waiters.is_empty() {
            waiters.sort_by(|(k1, w1), (k2, w2)| match w2.priority.cmp(&w1.priority) {
                Ordering::Equal => k1.cmp(k2),
                o => o,
            });
            //waiters.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
            let keys: Vec<usize> = waiters.iter().map(|(k, _)| **k).collect();
            drop(waiters);
            for key in &keys {
                let waiter = self.waiters.remove(key).unwrap();
                let acquire: ResourceHandler = self.add_acquire(waiter.capacity);
                let acquire_id = acquire.acquire_id;
                match waiter.tx.send(acquire).await {
                    Ok(()) => break,
                    Err(_) => {
                        self.remove_acquire(acquire_id);
                        self.update_remaining_capacity().expect("");
                    }
                }
            }
        }
    }

    pub fn add_acquire(&mut self, capacity: Capacity) -> ResourceHandler {
        let id = self.acquire_id;
        self.acquire_id += 1;
        self.acquirers.insert(id, capacity);
        self.update_remaining_capacity().unwrap();
        ResourceHandler {
            label: self.label.clone(),
            resource_id: self.id,
            acquire_id: id,
        }
    }

    pub fn remove_acquire(&mut self, acquire_id: usize) {
        self.acquirers.remove(&acquire_id);
    }

    pub fn add_waiter(
        &mut self,
        capacity: Capacity,
        priority: usize,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let id = self.waiter_id;
        self.waiter_id += 1;
        let (tx, rx) = channel(1);

        self.waiters.insert(
            id,
            Waiter {
                capacity,
                priority,
                tx,
            },
        );

        Ok(WaitAcquire {
            rx,
            waiter_id: id,
            resource_id: self.id,
        })
    }

    pub async fn remove_waiter(&mut self, mut wa: WaitAcquire) {
        match self.waiters.remove(&wa.waiter_id) {
            None => {
                let rh: ResourceHandler = wa.rx.recv().await.unwrap();
                self.remove_acquire(rh.acquire_id);
            }
            Some(_) => {}
        }
    }
}

pub struct Waiter {
    capacity: Capacity,
    priority: usize,
    tx: Sender<ResourceHandler>,
}

#[derive(Clone, Default)]
pub struct ResourceCollection {
    labels: Arc<Mutex<HashMap<String, ResourceId>>>,
    inner: Arc<Mutex<HashMap<ResourceId, Resource>>>,
    id: Arc<AtomicUsize>,
}

pub enum AcquireResponse {
    Ok(ResourceHandler),
    Wait(WaitAcquire),
}

pub struct WaitAcquire {
    rx: Receiver<ResourceHandler>,
    waiter_id: usize,
    resource_id: usize,
}

impl WaitAcquire {
    pub async fn recv(&mut self) -> ResourceHandler {
        self.rx.recv().await.unwrap()
    }
}

pub type ResourceError = LRuntimeError;

impl ResourceCollection {
    pub async fn clear(&self) {
        *self.inner.lock().await = Default::default();
        assert!(self.inner.lock().await.is_empty());
        println!("mutex map cleared");
    }

    pub async fn new_resource(&self, label: String, capacity: Option<Capacity>) {
        let id = get_and_update_id_counter(self.id.clone());
        let mut labels = self.labels.lock().await;
        labels.insert(label.clone(), id);
        drop(labels);
        let mut map = self.inner.lock().await;
        let capacity = capacity.unwrap_or(Capacity::Unary);
        let resource = Resource {
            label,
            id,
            max_capacity: capacity,
            capacity,
            acquirers: Default::default(),
            acquire_id: 0,
            waiters: Default::default(),
            waiter_id: 0,
        };
        map.insert(id, resource);
    }

    pub async fn acquire(
        &self,
        label: String,
        capacity: Capacity,
        priority: usize,
    ) -> Result<AcquireResponse, LRuntimeError> {
        let labels = self.labels.lock().await;
        let id = *labels.get(&label).ok_or_else(|| {
            LRuntimeError::new("acquire", format!("Resource {} does not exist.", label))
        })?;
        drop(labels);
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&id).unwrap();
        let response: AcquireResponse = match (resource.capacity, capacity) {
            (Capacity::Unary, Capacity::All) => AcquireResponse::Ok(resource.add_acquire(capacity)),

            (Capacity::None, Capacity::All) => {
                AcquireResponse::Wait(resource.add_waiter(capacity, priority)?)
            }

            (Capacity::Some(u1), Capacity::Some(u2)) => {
                if u1 >= u2 {
                    AcquireResponse::Ok(resource.add_acquire(capacity))
                } else {
                    AcquireResponse::Wait(resource.add_waiter(capacity, priority)?)
                }
            }
            (Capacity::Some(_), Capacity::All) => {
                if resource.capacity == resource.max_capacity {
                    AcquireResponse::Ok(resource.add_acquire(capacity))
                } else {
                    AcquireResponse::Wait(resource.add_waiter(capacity, priority)?)
                }
            }
            _ => Err(LRuntimeError::new("acquire", "acquisition illegal"))?,
        };
        Ok(response)
    }

    pub async fn is_locked(&self, label: String) -> Result<bool, LRuntimeError> {
        let id = *self
            .labels
            .lock()
            .await
            .get(&label)
            .ok_or_else(|| LRuntimeError::new("is_locked", "resource does not exist"))?;
        Ok(!self
            .inner
            .lock()
            .await
            .get(&id)
            .unwrap()
            .acquirers
            .is_empty())
    }

    pub async fn release(&self, rh: ResourceHandler) -> Result<(), LRuntimeError> {
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&rh.resource_id).unwrap();
        resource.remove_acquire(rh.acquire_id);
        resource.update_remaining_capacity()?;
        resource.update_waiters().await;
        Ok(())
    }

    pub async fn remove_waiter(&self, wa: WaitAcquire) {
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&wa.resource_id).unwrap();
        resource.remove_waiter(wa).await;
    }

    pub async fn get_list_resources(&self) -> Vec<String> {
        self.labels
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<String>>()
    }

    pub async fn get_debug(&self) -> String {
        let labels: HashMap<String, ResourceId> = self.labels.lock().await.clone();
        let inner = self.inner.lock().await;
        let mut str = "resources:\n".to_string();

        for (label, id) in labels {
            let r: &Resource = inner.get(&id).unwrap();

            let mut w_str = "".to_string();
            for w in r.waiters.values() {
                w_str.push_str(
                    format!("\n\t\t-capacity = {};priority= {}", w.capacity, w.priority).as_str(),
                );
            }

            str.push_str(
                format!(
                    "- {}:\n\
                    \t- max_capacity: {}\n\
                    \t- capacity: {}\n\
                    \t- acquirers: {}\n\
                    \t- waiters: {} \n",
                    label,
                    r.max_capacity,
                    r.capacity,
                    r.acquirers.len(),
                    w_str,
                )
                .as_str(),
            );
        }
        str
    }

    pub async fn get_snapshot(&self) -> WorldStateSnapshot {
        let mut r#static: im::HashMap<LValueS, LValueS> = Default::default();
        let mut r#dynamic: im::HashMap<LValueS, LValueS> = Default::default();
        for (_, resource) in self.inner.lock().await.iter() {
            r#static.insert(
                list![MAX_Q.into(), resource.label.clone().into()]
                    .try_into()
                    .unwrap(),
                LValue::from(usize::from(resource.max_capacity))
                    .try_into()
                    .unwrap(),
            );
            r#dynamic.insert(
                list![QUANTITY.into(), resource.label.clone().into()]
                    .try_into()
                    .unwrap(),
                LValue::from(usize::from(resource.capacity))
                    .try_into()
                    .unwrap(),
            );
        }

        WorldStateSnapshot {
            r#static: Default::default(),
            dynamic: Default::default(),
            inner_static: PartialState {
                inner: r#static,
                _type: Some(StateType::InnerStatic),
            },
            inner_dynamic: PartialState {
                inner: r#dynamic,
                _type: Some(StateType::InnerDynamic),
            },
            instance: Default::default(),
        }
    }
}
