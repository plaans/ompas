use crate::ompas::manager::resource::WaiterPriority::{Execution, Planner};
use crate::ompas::manager::state::partial_state::PartialState;
use crate::ompas::manager::state::world_state::{StateType, WorldStateSnapshot};
use im::HashSet;
use itertools::Itertools;
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
use std::sync::{atomic, Arc};
use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::sync::Mutex;

const ALL: &str = "all";

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Quantity {
    All,
    Some(usize),
}

impl Display for Quantity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Quantity::All => write!(f, "{}", ALL),
            Quantity::Some(u) => write!(f, "{}", u),
        }
    }
}

impl From<usize> for Quantity {
    fn from(u: usize) -> Self {
        Self::Some(u)
    }
}

impl From<Quantity> for usize {
    fn from(value: Quantity) -> Self {
        match value {
            Quantity::All => 1,
            Quantity::Some(u) => u,
        }
    }
}

pub type ResourceId = usize;

pub struct ResourceHandler {
    label: String,
    resource_id: ResourceId,
    client_id: usize,
}

impl ResourceHandler {
    pub fn get_label(&self) -> &str {
        &self.label
    }
}

pub type ClientId = usize;
pub type Capacity = usize;

pub struct Resource {
    label: String,
    id: ResourceId,
    max_capacity: Capacity,
    capacity: Capacity,
    in_service: HashSet<ClientId>,
    queue: Vec<ClientId>,
    clients: HashMap<ClientId, Client>,
    n_acquisition: usize,
}

pub const PLANNER_PRIORITY: usize = 10;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum WaiterPriority {
    Planner(usize),
    Execution(usize),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Ticket {
    id: ClientId,
    priority: WaiterPriority,
}

impl Ticket {
    pub fn new(id: ClientId, priority: WaiterPriority) -> Self {
        Self { id, priority }
    }
}

impl Ord for Ticket {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.priority.cmp(&other.priority) {
            Ordering::Equal => other.id.cmp(&self.id),
            other => other,
        }
    }
}

impl PartialOrd for Ticket {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WaiterPriority {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Planner(o1), Planner(o2)) => o2.cmp(o1),
            (Planner(_), Execution(e)) => PLANNER_PRIORITY.cmp(e),
            (Execution(e), Planner(_)) => e.cmp(&PLANNER_PRIORITY),
            (Execution(e1), Execution(e2)) => e1.cmp(e2),
        }
    }
}

impl PartialOrd for WaiterPriority {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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
    pub fn new_ticket(
        &mut self,
        quantity: Quantity,
        priority: WaiterPriority,
        kind: TicketKind,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let acquisition_id = self.n_acquisition;
        self.n_acquisition += 1;
        if match quantity {
            Quantity::All => true,
            Quantity::Some(u) => u <= self.capacity,
        } {
            self.add_client(acquisition_id, kind, quantity, priority)
        } else {
            Err(LRuntimeError::new("acquire", "acquisition illegal"))
        }
    }

    pub fn _reserve(
        &mut self,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let r = self.new_ticket(quantity, priority, TicketKind::Reservation)?;
        Ok(r)
    }

    pub async fn _acquire(
        &mut self,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let r = self.new_ticket(quantity, priority, TicketKind::Reservation)?;
        self.update_queue().await;
        Ok(r)
    }

    pub fn update_remaining_capacity(&mut self) -> Result<(), LRuntimeError> {
        let mut capacity = self.max_capacity;
        //let old_capacity = self.capacity;

        for c in &self.in_service {
            capacity -= self.clients.get(c).unwrap().quantity;
        }

        self.capacity = capacity;
        //println!("capacity {} -> {}", old_capacity, self.capacity);

        Ok(())
    }

    pub fn update_waiter_queue(&mut self) {
        self.queue = self
            .queue
            .iter()
            .map(|id| (id, self.clients.get(id).unwrap().priority))
            .sorted_by(|(id1, p1), (id2, p2)| match p1.cmp(p2) {
                Ordering::Equal => id1.cmp(id2),
                other => other,
            })
            .map(|(id, _)| *id)
            .collect();
    }

    pub async fn update_queue(&mut self) {
        let mut queue = mem::take(&mut self.queue);

        while let Some(ticket_id) = queue.pop() {
            let waiter = self.clients.get(&ticket_id).unwrap();
            if waiter.quantity <= self.capacity {
                let acquire: ResourceHandler = self.add_acquire(&ticket_id);
                let waiter = self.clients.get(&ticket_id).unwrap();
                match waiter.tx.send(acquire).await {
                    Ok(()) => break,
                    Err(_) => {
                        self._remove_client(&ticket_id);
                        self.update_remaining_capacity().expect("");
                        continue;
                    }
                }
            } else {
                queue.push(ticket_id);
                break;
            }
        }
        self.queue = queue
    }

    pub fn add_acquire(&mut self, id: &ClientId) -> ResourceHandler {
        self.in_service.insert(*id);
        self.update_remaining_capacity().unwrap();
        ResourceHandler {
            label: self.label.clone(),
            resource_id: self.id,
            client_id: *id,
        }
    }

    pub fn remove_ticket(&mut self, id: &ClientId) {
        self.queue.retain(|c_id| c_id != id)
    }

    pub fn remove_in_service(&mut self, ticket_id: &ClientId) {
        self.in_service.remove(ticket_id);
        self._remove_client(ticket_id);
    }

    pub fn _remove_client(&mut self, acquire_id: &ClientId) {
        self.clients.remove(acquire_id);
    }

    pub fn quantity_as_usize(&self, quantity: &Quantity) -> usize {
        match quantity {
            Quantity::Some(u) => *u,
            Quantity::All => self.max_capacity,
        }
    }

    pub fn add_client(
        &mut self,
        id: ClientId,
        _kind: TicketKind,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let (tx, rx) = channel(1);
        let quantity = self.quantity_as_usize(&quantity);

        self.clients.insert(
            id,
            Client {
                _kind,
                quantity,
                priority,
                tx,
            },
        );

        self.queue.push(id);

        self.update_waiter_queue();

        Ok(WaitAcquire {
            rx,
            ticket_id: id,
            resource_id: self.id,
        })
    }

    pub async fn remove_client(&mut self, mut wa: WaitAcquire) {
        match self.clients.remove(&wa.ticket_id) {
            None => {
                let rh: ResourceHandler = wa.rx.recv().await.unwrap();
                self._remove_client(&rh.client_id);
            }
            Some(_) => {}
        }
        self.remove_ticket(&wa.ticket_id);
    }

    pub fn update_priority(&mut self, client_id: &ClientId, priority: WaiterPriority) {
        if let Some(client) = self.clients.get_mut(client_id) {
            client.priority = priority;
            self.update_waiter_queue()
        }
    }

    pub fn get_client_quantity(&self, client_id: &ClientId) -> usize {
        self.clients.get(client_id).unwrap().quantity
    }
}

pub enum TicketKind {
    Direct,
    Reservation,
}

pub struct Client {
    _kind: TicketKind,
    quantity: usize,
    priority: WaiterPriority,
    tx: Sender<ResourceHandler>,
}

#[derive(Clone, Default)]
pub struct ResourceManager {
    labels: Arc<Mutex<HashMap<String, ResourceId>>>,
    inner: Arc<Mutex<HashMap<ResourceId, Resource>>>,
    id: Arc<AtomicUsize>,
    max_capacity: Arc<AtomicUsize>,
}

pub enum AcquireResponse {
    Ok(ResourceHandler),
    Wait(WaitAcquire),
}

pub struct WaitAcquire {
    rx: Receiver<ResourceHandler>,
    ticket_id: usize,
    resource_id: usize,
}

impl WaitAcquire {
    pub async fn recv(&mut self) -> ResourceHandler {
        self.rx.recv().await.unwrap()
    }

    pub(crate) fn get_client_id(&self) -> usize {
        self.ticket_id
    }

    pub(crate) fn get_resource_id(&self) -> usize {
        self.resource_id
    }
}

pub type ResourceError = LRuntimeError;

impl ResourceManager {
    pub async fn clear(&self) {
        *self.inner.lock().await = Default::default();
        assert!(self.inner.lock().await.is_empty());
        println!("mutex map cleared");
    }

    async fn get_id(&self, label: &str) -> Option<ResourceId> {
        let labels = self.labels.lock().await;
        labels.get(label).copied()
    }

    pub fn get_max_capacity(&self) -> usize {
        self.max_capacity.load(atomic::Ordering::Relaxed)
    }

    pub async fn new_resource(&self, label: String, capacity: Option<Capacity>) {
        let id = get_and_update_id_counter(self.id.clone());
        let mut labels = self.labels.lock().await;
        labels.insert(label.clone(), id);
        drop(labels);
        let mut map = self.inner.lock().await;
        let capacity: Capacity = capacity.unwrap_or(1);
        let resource = Resource {
            label,
            id,
            max_capacity: capacity,
            capacity,
            in_service: Default::default(),
            clients: Default::default(),
            n_acquisition: 0,
            queue: Default::default(),
        };
        map.insert(id, resource);
        let value = self.max_capacity.load(atomic::Ordering::Relaxed);
        if capacity > value {
            self.max_capacity
                .compare_exchange(
                    value,
                    capacity,
                    atomic::Ordering::Release,
                    atomic::Ordering::Acquire,
                )
                .unwrap_or_else(|b| {
                    eprintln!("error on compare_exchange in new_resource");
                    b
                });
        }
    }

    pub async fn acquire(
        &self,
        label: &str,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let id = self.get_id(label).await.ok_or_else(|| {
            LRuntimeError::new("acquire", format!("Resource {} does not exist.", label))
        })?;
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&id).unwrap();
        resource._acquire(quantity, priority).await
    }

    pub async fn reserve(
        &self,
        label: &str,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let id = self.get_id(label).await.ok_or_else(|| {
            LRuntimeError::new("acquire", format!("Resource {} does not exist.", label))
        })?;
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&id).unwrap();
        resource._reserve(quantity, priority)
    }

    pub async fn update_queue(&self, resource: &str) -> Result<(), LRuntimeError> {
        let id = self.get_id(resource).await.ok_or_else(|| {
            LRuntimeError::new("acquire", format!("Resource {} does not exist.", resource))
        })?;
        self.inner
            .lock()
            .await
            .get_mut(&id)
            .unwrap()
            .update_queue()
            .await;
        Ok(())
    }

    pub async fn is_locked(&self, label: &str) -> Result<bool, LRuntimeError> {
        let id = self.get_id(label).await.ok_or_else(|| {
            LRuntimeError::new("acquire", format!("Resource {} does not exist.", label))
        })?;
        Ok(!self
            .inner
            .lock()
            .await
            .get(&id)
            .unwrap()
            .in_service
            .is_empty())
    }

    pub async fn release(&self, rh: ResourceHandler) -> Result<(), LRuntimeError> {
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&rh.resource_id).unwrap();
        resource.remove_in_service(&rh.client_id);
        resource.update_remaining_capacity()?;
        resource.update_queue().await;
        Ok(())
    }

    pub async fn remove_waiter(&self, wa: WaitAcquire) {
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(&wa.resource_id).unwrap();
        resource.remove_client(wa).await;
    }

    pub async fn update_priority(
        &self,
        resource_id: &ResourceId,
        client_id: &ClientId,
        priority: WaiterPriority,
    ) {
        let mut map = self.inner.lock().await;
        let resource: &mut Resource = map.get_mut(resource_id).unwrap();
        resource.update_priority(client_id, priority)
    }

    pub async fn get_client_quantity(
        &self,
        resource_id: &ResourceId,
        client_id: &ClientId,
    ) -> usize {
        let map = self.inner.lock().await;
        let resource: &Resource = map.get(resource_id).unwrap();
        resource.get_client_quantity(client_id)
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
            for w in r.clients.values() {
                w_str.push_str(
                    format!("\n\t\t-capacity = {};priority= {}", w.quantity, w.priority).as_str(),
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
                    r.in_service.len(),
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
                LValue::from(resource.max_capacity).try_into().unwrap(),
            );
            r#dynamic.insert(
                list![QUANTITY.into(), resource.label.clone().into()]
                    .try_into()
                    .unwrap(),
                LValue::from(resource.capacity).try_into().unwrap(),
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
