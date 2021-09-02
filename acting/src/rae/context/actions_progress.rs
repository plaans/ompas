use crate::rae::context::rae_state::{ActionStatus, ActionStatusSet};
use crate::rae::TOKIO_CHANNEL_SIZE;
use im::HashMap;
use log::{error, info, warn};
use ompas_utils::blocking_async;
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::{mpsc, RwLock};

pub type ActionLabel = String;
pub type ActionId = usize;
pub type MethodLabel = String;
pub type MethodId = usize;
pub type TaskLabel = String;
pub type TaskId = usize;

//struct to handle actions trigger, status and updates by other modules as godot
#[derive(Default, Debug, Clone)]
pub struct ActionsProgress {
    pub status: Arc<RwLock<im::HashMap<ActionId, Status>>>,
    pub sync: ActionStatusSync,
    next_id: Arc<AtomicUsize>,
}

impl ActionsProgress {
    pub fn declare_new_watcher(&self, action_id: &usize) -> Receiver<bool> {
        let (sender, receiver) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let c_sync = self.sync.clone();
        let c_action_id = *action_id;
        blocking_async!(c_sync.add_action_to_watch(c_action_id, sender).await)
            .expect("fail adding action to watch");
        receiver
    }
}

#[derive(Clone, Debug)]
pub enum Status {
    Pending,
    Running,
    Failure,
    Done,
}

impl Display for Status {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Status::Pending => write!(f, "pending"),
            Status::Running => write!(f, "running"),
            Status::Failure => write!(f, "failure"),
            Status::Done => write!(f, "done"),
        }
    }
}

impl ActionsProgress {
    pub async fn get_status(&self, id: &ActionId) -> Option<Status> {
        self.status.read().await.get(id).cloned()
    }

    pub async fn add_action(&self) -> usize {
        let id = self.get_new_id();
        self.status.write().await.insert(id, Status::Pending);
        id
    }

    pub async fn update_status_action(&self, action_id: &ActionId, status: Status) {
        self.status.write().await.insert(*action_id, status);
    }

    pub async fn get_action_status(&self, action_id: &ActionId) -> Status {
        self.status.read().await.get(action_id).unwrap().clone()
    }

    pub fn get_new_id(&self) -> usize {
        loop {
            let id = self.next_id.load(Ordering::Relaxed);
            if self
                .next_id
                .compare_exchange(id, id + 1, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return id;
            }
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct ActionStatusSync {
    pub map: Arc<RwLock<HashMap<usize, Sender<bool>>>>,
    pub sender: Option<Sender<usize>>,
}

impl ActionStatusSync {
    pub async fn trigger_event_update(&self, id: &usize) {
        //TODO: resolve bug that we have sometimes.
        let sender = self.map.read().await.get(id).unwrap().clone();
        if let Err(e) = sender.send(true).await {
            warn!(
                "trigger_event_update failed to send signal to receiver for update on action status: {}",
                e
            )
        }
    }

    pub async fn add_action_to_watch(&self, action_id: usize, watcher: Sender<bool>) {
        match self.map.write().await.insert(action_id, watcher) {
            None => {}
            Some(_) => panic!("action_id was already defined"),
        }
    }
}

pub async fn async_status_watcher_run(asw: ActionStatusSync, mut receiver: Receiver<usize>) {
    loop {
        match receiver.recv().await {
            None => {
                panic!("error from receiver")
            }
            Some(id) => {
                info!("action status of {} is updated", id);
                asw.trigger_event_update(&id).await;
            }
        }
    }
}
