use crate::model::acting_domain::event::{Event, TriggerActivation};
use crate::ompas::interface::job::Job;
use crate::ompas::interface::rae_command::OMPASJob;
use crate::ompas::interface::trigger_collection::Response;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::state::instance::InstanceCollection;
use crate::ompas::manager::state::state_update_manager::{StateUpdate, StateUpdateSubscriber};
use crate::ompas::manager::state::StateManager;
use aries::utils::enumerate;
use aries::utils::StreamingIterator;
use ompas_language::monitor::model::EVENT_NEW_INSTANCE;
use ompas_language::process::*;
use ompas_middleware::logger::LogClient;
use ompas_middleware::ProcessInterface;
use sompas_core::eval;
use sompas_structs::lenv::LEnv;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::{oneshot, Mutex, RwLock};
use tokio::task::JoinHandle;

#[derive(Clone)]
pub struct EventManager {
    fluent_collection: Arc<Mutex<FluentCollection>>,
    event_collection: Arc<Mutex<EventCollection>>,
    state_manager: StateManager,
    clock_manager: ClockManager,
    log: Arc<RwLock<LogClient>>,
    env: Arc<RwLock<LEnv>>,
}

impl EventManager {
    pub fn new(state_manager: StateManager, clock_manager: ClockManager) -> Self {
        Self {
            fluent_collection: Default::default(),
            event_collection: Default::default(),
            state_manager,
            clock_manager,
            log: Default::default(),
            env: Arc::new(Default::default()),
        }
    }
}
#[derive(Default)]
pub struct FluentCollection {
    map: HashMap<WaitForId, WaitFor>,
    id: WaitForId,
}

impl FluentCollection {
    pub fn new_id(&mut self) -> WaitForId {
        let temp = self.id;
        self.id += 1;
        temp
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct WatchedEvent {
    label: String,
    pre_condition: LValue,
    body: LValue,
    trigger_activation: TriggerActivation,
}

#[derive(Default)]
pub struct ManagedEvent {
    events: Vec<String>,
    instance_handled: HashSet<String>,
}

#[derive(Default)]
pub struct EventCollection {
    managed_events: HashMap<String, ManagedEvent>,
    events: HashMap<String, Event>,
    tx_ompas: Option<UnboundedSender<OMPASJob>>,
    watched: Vec<WatchedEvent>,
    running: Vec<JoinHandle<()>>,
}

impl EventCollection {
    pub fn init_events(&mut self, events: HashMap<String, Event>, instances: InstanceCollection) {
        for t in instances.inner.keys() {
            self.managed_events
                .insert(t.to_string(), ManagedEvent::default());
        }

        for event in events.values() {
            for (_, param) in event.parameters.inner() {
                self.managed_events
                    .get_mut(&param.get_debug().to_string())
                    .unwrap_or_else(|| {
                        panic!("Event manager only supports events with enumerable parameter types")
                    })
                    .events
                    .push(event.label.to_string())
            }
        }
        self.events = events;
        self.init_watched_events(instances)
    }
    fn init_watched_events(&mut self, instances: InstanceCollection) {
        //Create
        for (label, event) in &self.events {
            let mut params_enum: Vec<_> = vec![];
            for (_, param) in event.parameters.inner() {
                let t_label = param.get_debug().to_string();
                params_enum.push(instances.get_instances(&t_label))
            }

            let params_enum_iter: Vec<_> = params_enum.iter().map(|l| l.iter()).collect();
            let mut enumerated = enumerate(params_enum_iter);
            while let Some(e) = enumerated.next() {
                let mut parameters: Vec<LValue> = vec![];
                for param in e {
                    parameters.push((*param).into())
                }
                let mut pre_condition = vec![event.trigger.pre_conditions.clone()];
                pre_condition.append(&mut parameters.clone());
                let pre_condition = pre_condition.into();
                let mut body = vec![event.lambda_body.clone()];
                body.append(&mut parameters);
                let body = body.into();
                let event = WatchedEvent {
                    label: label.to_string(),
                    pre_condition,
                    trigger_activation: event.trigger.trigger_activation,
                    body,
                };
                self.watched.push(event)
            }
        }

        //Add all current instances as handled
        for (label, instance) in &instances.inner {
            self.managed_events.get_mut(label).unwrap().instance_handled = instance.elements.clone()
        }
    }

    fn populate_events_for_new_instance(
        &mut self,
        r#type: String,
        instance: String,
        instances: &InstanceCollection,
    ) {
        let managed_event = self.managed_events.get(&r#type).unwrap();

        for label_event in &managed_event.events {
            let event = self.events.get(label_event).unwrap();
            let mut params_enum: Vec<_> = vec![];

            for (_, param) in event.parameters.inner() {
                let type_label = param.get_debug().to_string();
                if type_label == r#type {
                    params_enum.push(vec![instance.clone()])
                } else {
                    params_enum.push(instances.get_instances(&type_label))
                }
            }

            let params_enum_iter: Vec<_> = params_enum.iter().map(|vec| vec.iter()).collect();
            let mut enumerated = enumerate(params_enum_iter);
            while let Some(e) = enumerated.next() {
                let mut parameters: Vec<LValue> = vec![];
                for param in e {
                    parameters.push((*param).into())
                }
                let mut pre_condition = vec![event.trigger.pre_conditions.clone()];
                pre_condition.append(&mut parameters.clone());
                let pre_condition = pre_condition.into();
                let mut body = vec![event.lambda_body.clone()];
                body.append(&mut parameters);
                let body = body.into();
                let event = WatchedEvent {
                    label: event.label.to_string(),
                    pre_condition,
                    trigger_activation: event.trigger.trigger_activation,
                    body,
                };
                self.watched.push(event)
            }
        }
    }
}

impl EventManager {
    pub async fn add_waiter(&self, lambda: LValue) -> WaitForReceiver {
        let (tx, rx) = oneshot::channel();
        let mut inner = self.fluent_collection.lock().await;
        //println!("new waiter: {}", lambda);
        let w = WaitFor {
            lambda,
            date: self.clock_manager.now(),
            channel: tx,
        };
        let id = inner.new_id();
        inner.map.insert(id, w);
        WaitForReceiver { receiver: rx, id }
    }

    pub async fn remove_waiter(&self, id: WaitForId) {
        self.fluent_collection.lock().await.map.remove(&id);
    }

    pub async fn check_wait_for(&self) {
        let log = self.log.read().await.clone();
        let mut env = self.env.read().await.clone();
        //let mut item_to_remove: Vec<WaitForId> = vec![];
        let mut waiters = self.fluent_collection.lock().await;
        let keys: Vec<_> = waiters.map.keys().copied().collect();
        for id in &keys {
            let lambda: &LValue = &waiters.map.get(id).unwrap().lambda;
            let result = eval(lambda, &mut env, None).await;
            match result {
                Ok(lv) => {
                    if let LValue::True = lv {
                        let waiter = waiters.map.remove(id).unwrap();
                        waiter.channel.send(true).expect("");
                        log.debug(format!("{} became true", waiter.lambda));
                    }
                }
                Err(e) => {
                    log.warn(format!("error checking wait on: {e}"));
                }
            }
        }
    }
}
impl EventManager {
    pub async fn init_events(
        &self,
        events: HashMap<String, Event>,
        tx_ompas: UnboundedSender<OMPASJob>,
    ) {
        let mut event_collection = self.event_collection.lock().await;
        event_collection.tx_ompas = Some(tx_ompas);
        let instances = self.state_manager.get_instance_collection().await;
        event_collection.init_events(events, instances);
    }

    pub async fn check_events(&self, updated: StateUpdate) {
        let mut env = self.env.read().await.clone();
        let instances = self.state_manager.get_instance_collection().await;
        let log = self.log.read().await.clone();
        let mut event_collection = self.event_collection.lock().await;
        let new_instances: Vec<_> = updated
            .iter()
            .filter_map(|lvs| {
                if let LValueS::List(list) = lvs {
                    if list[0].to_string() == EVENT_NEW_INSTANCE {
                        return Some((list[1].to_string(), list[2].to_string()));
                    }
                };
                None
            })
            .collect();

        for (t, instance) in new_instances {
            event_collection.populate_events_for_new_instance(t, instance, &instances);
        }

        let mut to_run: Vec<usize> = vec![];
        for (i, event) in event_collection.watched.iter().enumerate() {
            if let Ok(r) = eval(&event.pre_condition, &mut env, None).await {
                if !matches!(r, LValue::Err(_)) {
                    log.info(format!("event triggered: {} ", event.body));
                    to_run.push(i)
                }
            }
        }

        to_run.reverse();

        let arc_event_collection = self.event_collection.clone();

        for i in to_run {
            let watched = event_collection.watched.remove(i);
            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel();
            event_collection
                .tx_ompas
                .as_ref()
                .unwrap()
                .send(Job::new_event(tx, watched.body.clone()).into())
                .unwrap();
            let arc_2 = arc_event_collection.clone();
            let log_2 = log.clone();
            let handle = tokio::spawn(async move {
                if let Some(Ok(Response::Handle(h))) = rx.recv().await {
                    if let Ok(_) = h.get_future().await {
                        if watched.trigger_activation != TriggerActivation::Once {
                            log_2.info(format!(
                                "event {} terminated, added again to watched events",
                                watched.body
                            ));
                            arc_2.lock().await.watched.push(watched);
                        }
                    }
                };
                ()
            });
            event_collection.running.push(handle)
        }
    }

    pub async fn get_debug(&self) -> String {
        let mut str = "'monitored dynamic expression(s): \n".to_string();
        for wf in self.fluent_collection.lock().await.map.values() {
            str.push_str(format!("- [{:.^3}]", wf.date).as_str());
            str.push_str(wf.lambda.to_string().as_str());
            str.push('\n');
        }
        str
    }

    pub async fn clear(&self) {
        *self.fluent_collection.lock().await = Default::default()
    }
}

pub struct WaitFor {
    lambda: LValue,
    date: Timepoint,
    channel: oneshot::Sender<bool>,
}

pub type WaitForId = usize;

pub struct WaitForReceiver {
    receiver: oneshot::Receiver<bool>,
    id: WaitForId,
}

impl WaitForReceiver {
    pub async fn recv(self) -> Result<bool, oneshot::error::RecvError> {
        self.receiver.await
    }

    pub fn id(&self) -> &WaitForId {
        &self.id
    }
}

pub async fn run_event_checker(
    mut update: StateUpdateSubscriber,
    event_manager: EventManager,
    tx_ompas: UnboundedSender<OMPASJob>,
    events: HashMap<String, Event>,
    env: LEnv,
) {
    let mut process: ProcessInterface =
        ProcessInterface::new(PROCESS_CHECK_FLUENT, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;
    process.log_info("check wait for launched");
    *event_manager.log.write().await = process.get_log_client();
    *event_manager.env.write().await = env;
    event_manager.init_events(events, tx_ompas).await;
    loop {
        tokio::select! {
            Some(updated) = update.channel.recv() => {
                event_manager.check_events(updated).await
            }
            _ = process.recv() => {
                break;
            }
        }
    }
}

pub async fn run_fluent_checker(mut update: StateUpdateSubscriber, event_manager: EventManager) {
    let mut process: ProcessInterface =
        ProcessInterface::new(PROCESS_CHECK_FLUENT, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;
    process.log_info("fluent checker launched");
    loop {
        tokio::select! {
            Some(_) = update.channel.recv() => {
                let n_wait_on = event_manager.fluent_collection.lock().await.map.len();
                if n_wait_on != 0 {
                    event_manager.check_wait_for().await;
                }
            }
            _ = process.recv() => {
                break;
            }
        }
    }
}
