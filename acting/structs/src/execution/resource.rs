use crate::execution::resource::WaiterPriority::{Execution, Planner};
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
use std::mem;
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

pub type AcquisitionId = usize;

pub struct Resource {
    label: String,
    id: ResourceId,
    max_capacity: Capacity,
    capacity: Capacity,
    acquirers: HashMap<AcquisitionId, Acquire>,
    waiter_queue: Vec<WaiterTicket>,
    waiters: HashMap<AcquisitionId, Waiter>,
    n_acquisition: usize,
}

pub struct Acquire {
    pub kind: AcquireKind,
    id: AcquisitionId,
    pub capacity: Capacity,
}

pub const PLANNER_PRIORITY: usize = 10;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WaiterPriority {
    Planner(usize),
    Execution(usize),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct WaiterTicket {
    id: AcquisitionId,
    priority: WaiterPriority,
}

impl WaiterTicket {
    pub fn new(id: AcquisitionId, priority: WaiterPriority) -> Self {
        Self { id, priority }
    }
}

impl Ord for WaiterTicket {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.priority.cmp(&other.priority) {
            Ordering::Equal => other.id.cmp(&self.id),
            other => other,
        }
    }
}

impl PartialOrd for WaiterTicket {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for WaiterPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Planner(o1), Planner(o2)) => o2.cmp(o1),
            (Planner(o), Execution(e)) => PLANNER_PRIORITY.cmp(e),
            (Execution(e), Planner(o)) => e.cmp(&PLANNER_PRIORITY),
            (Execution(e1), Execution(e2)) => e1.cmp(e2),
        }
    }
}

impl PartialOrd for WaiterPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

impl Display for WaiterPriority {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            WaiterPriority::Planner(id) => {
                write!(f, "planner({id})")
            }
            WaiterPriority::Execution(p) => {
                write!(f, "execution({p})")
            }
        }
    }
}

impl Resource {
    pub fn _acquire(
        &mut self,
        capacity: Capacity,
        priority: WaiterPriority,
        kind: AcquireKind,
    ) -> Result<AcquireResponse, LRuntimeError> {
        let acquisition_id = self.n_acquisition;
        self.n_acquisition += 1;

        let response = match (self.capacity, capacity) {
            (Capacity::Unary, Capacity::All) => {
                AcquireResponse::Ok(self.add_acquire(acquisition_id, kind, capacity))
            }

            (Capacity::None, Capacity::All) => {
                AcquireResponse::Wait(self.add_waiter(acquisition_id, kind, capacity, priority)?)
            }

            (Capacity::Some(u1), Capacity::Some(u2)) => {
                if u1 >= u2 {
                    AcquireResponse::Ok(self.add_acquire(acquisition_id, kind, capacity))
                } else {
                    AcquireResponse::Wait(self.add_waiter(
                        acquisition_id,
                        kind,
                        capacity,
                        priority,
                    )?)
                }
            }
            (Capacity::Some(_), Capacity::All) => {
                if self.capacity == self.max_capacity {
                    AcquireResponse::Ok(self.add_acquire(acquisition_id, kind, capacity))
                } else {
                    AcquireResponse::Wait(self.add_waiter(
                        acquisition_id,
                        kind,
                        capacity,
                        priority,
                    )?)
                }
            }
            _ => Err(LRuntimeError::new("acquire", "acquisition illegal"))?,
        };
        Ok(response)
    }

    pub fn update_remaining_capacity(&mut self) -> Result<(), LRuntimeError> {
        let mut capacity = self.max_capacity;
        //let old_capacity = self.capacity;

        for c in self.acquirers.values() {
            match c.capacity {
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

    pub fn update_waiter_queue(&mut self) {
        self.waiter_queue.sort_by(|k1, k2| k1.cmp(k2))
    }

    pub async fn update_waiters(&mut self) {
        let mut queue = mem::take(&mut self.waiter_queue);

        while let Some(ticket) = queue.pop() {
            let waiter = self.waiters.get(&ticket.id).unwrap();

            let can_acquire = match (self.capacity, waiter.capacity) {
                (Capacity::Unary, Capacity::All) => true,
                (Capacity::Some(u1), Capacity::Some(u2)) => u1 >= u2,
                _ => false,
            };

            if can_acquire {
                let waiter = self.waiters.remove(&ticket.id).unwrap();
                let acquire: ResourceHandler =
                    self.add_acquire(ticket.id, waiter.kind, waiter.capacity);
                let acquire_id = ticket.id;
                match waiter.tx.send(acquire).await {
                    Ok(()) => break,
                    Err(_) => {
                        self.remove_acquire(acquire_id);
                        self.update_remaining_capacity().expect("");
                        continue;
                    }
                }
            } else {
                queue.push(ticket);
                break;
            }
        }
        self.waiter_queue = queue
    }

    pub fn add_acquire(
        &mut self,
        id: AcquisitionId,
        kind: AcquireKind,
        capacity: Capacity,
    ) -> ResourceHandler {
        self.acquirers.insert(id, Acquire { kind, id, capacity });
        self.update_remaining_capacity().unwrap();
        ResourceHandler {
            label: self.label.clone(),
            resource_id: self.id,
            acquire_id: id,
        }
    }

    pub fn remove_ticket(&mut self, id: AcquisitionId) {
        self.waiter_queue.retain(|t| t.id != id)
    }

    pub fn remove_acquire(&mut self, acquire_id: usize) {
        self.acquirers.remove(&acquire_id);
    }

    pub fn add_waiter(
        &mut self,
        id: AcquisitionId,
        kind: AcquireKind,
        capacity: Capacity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let (tx, rx) = channel(1);

        self.waiters.insert(
            id,
            Waiter {
                kind,
                capacity,
                priority,
                tx,
            },
        );

        self.waiter_queue.push(WaiterTicket { id, priority });

        self.update_waiter_queue();

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
        self.remove_ticket(wa.waiter_id);
    }
}

pub enum AcquireKind {
    Direct,
    Reservation,
}

pub struct Waiter {
    kind: AcquireKind,
    capacity: Capacity,
    priority: WaiterPriority,
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
            waiters: Default::default(),
            n_acquisition: 0,
            waiter_queue: Default::default(),
        };
        map.insert(id, resource);
    }

    pub async fn acquire(
        &self,
        label: String,
        capacity: Capacity,
        priority: WaiterPriority,
        kind: AcquireKind,
    ) -> Result<AcquireResponse, LRuntimeError> {
        let labels = self.labels.lock().await;
        let id = *labels.get(&label).ok_or_else(|| {
            LRuntimeError::new("acquire", format!("Resource {} does not exist.", label))
        })?;
        drop(labels);
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&id).unwrap();
        resource._acquire(capacity, priority, kind)
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
