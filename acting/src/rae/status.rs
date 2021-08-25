use crate::rae::context::Status;
use crate::rae::state::{ActionStatus, ActionStatusSet};
use im::HashMap;
use log::{error, info, warn};
use std::sync::Arc;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::RwLock;

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
