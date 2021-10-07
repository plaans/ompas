/*use crate::blocking_async;
use crate::other::get_and_update_id_counter;
use crate::task_handler::subscribe_new_task;
use im::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, MutexGuard};
use std::thread;
use tokio::sync::{broadcast, mpsc, Mutex};

const TOKIO_EVENT_ID_CHANNEL_SIZE: usize = 64;
const TOKIO_EVENT_SIGNAL_CHANNEL_SIZE: usize = 64;

lazy_static! {
    static ref EVENT_HANDLER: ArcEventHandler = ArcEventHandler::init();
}

#[derive(Clone)]
pub struct EventSignal {}

#[derive(Debug)]
pub struct EventHandler {
    sender: mpsc::Sender<EventId>,
    hashmap: HashMap<EventId, EventDispatcher>,
    next_id: Arc<AtomicUsize>,
}

#[derive(Debug, Clone)]
pub struct ArcEventHandler(Arc<Mutex<EventHandler>>);

impl ArcEventHandler {
    fn init() -> Self {
        let (tx, rx) = mpsc::channel(TOKIO_EVENT_ID_CHANNEL_SIZE);
        let handler = ArcEventHandler(Arc::new(Mutex::new(EventHandler {
            sender: tx,
            hashmap: Default::default(),
            next_id: Arc::new(Default::default()),
        })));

        let copy_handler = handler.clone();

        tokio::spawn(async move { task_handle_event_and_dispatch(copy_handler, rx).await });

        handler
    }

    async fn get_dispatcher(&self, id: EventId) -> Option<EventDispatcher> {
        self.0.lock().await.hashmap.get(&id).cloned()
    }

    async fn async_new_event(&mut self) -> EventRaiser {
        let mut event_handler = self.0.lock().await;
        let id = get_and_update_id_counter(event_handler.next_id.clone());
        let (tx, rx) = broadcast::channel(TOKIO_EVENT_SIGNAL_CHANNEL_SIZE);
        let event_dispatcher = EventDispatcher::new(id, tx);
        let event_raiser = EventRaiser::new(id, event_handler.sender.clone());
        event_handler.hashmap.insert(id, event_dispatcher);
        event_raiser
    }
}

pub type EventId = usize;

pub struct EventRaiser {
    id: EventId,
    sender: mpsc::Sender<EventId>,
}

impl EventRaiser {
    fn new(id: EventId, sender: mpsc::Sender<EventId>) -> Self {
        Self { id, sender }
    }

    fn get_id(&self) -> EventId {
        self.id
    }

    fn trigger_event(&self) {
        let sender = self.sender.clone();
        let id = self.id.clone();
        blocking_async!(sender.send(id).await);
    }

    fn subscribe(&self) -> broadcast::Receiver<EventSignal> {
        let id = self.id;
        let dispatcher = blocking_async!(EVENT_HANDLER.get_dispatcher(id).await)
            .expect()
            .unwrap();
        dispatcher.broadcast.subscribe()
    }
}

#[derive(Clone, Debug)]
pub struct EventDispatcher {
    id: EventId,
    broadcast: broadcast::Sender<EventSignal>,
}

impl EventDispatcher {
    fn new(id: EventId, broadcast: broadcast::Sender<EventSignal>) -> Self {
        Self { id, broadcast }
    }
}
async fn task_handle_event_and_dispatch(
    event_handler: ArcEventHandler,
    mut rx: mpsc::Receiver<EventId>,
) {
    let mut end_receiver = subscribe_new_task();

    loop {
        tokio::select! {
            event_id = rx.recv() => {
                match event_id {
                    Some(id) => {
                        let event_dispatcher = event_handler.0.lock().await.hashmap.get(&id).cloned().expect("no such event");
                        event_dispatcher.broadcast.send(EventSignal{});
                    }
                    None => {}
                }
            }
            _ = end_receiver.recv() => {
                println!("Task \"task_check_wait_on\" killed.");
                break;
            }
        }
    }
}*/
