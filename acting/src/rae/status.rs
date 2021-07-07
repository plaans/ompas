use crate::rae::context::Status;
use crate::rae::state::{ActionStatus, ActionStatusSet};
use im::HashMap;
use std::sync::{Arc, RwLock};
use tokio::sync::mpsc::{Receiver, Sender};

#[derive(Default, Clone, Debug)]
pub struct ActionStatusSync {
    pub map: Arc<RwLock<HashMap<usize, Sender<bool>>>>,
    pub sender: Option<Sender<usize>>,
}

impl ActionStatusSync {
    pub async fn trigger_event_update(&self, id: &usize) {
        //TODO: resolve bug that we have sometimes.
        let sender = self.map.read().unwrap().get(id).unwrap().clone();
        sender
            .send(true)
            .await
            .expect("error sending signal to watcher");
    }

    pub fn add_action_to_watch(&self, action_id: usize, watcher: Sender<bool>) {
        match self.map.write().unwrap().insert(action_id, watcher) {
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
                //println!("action status of {} is updated", id);
                asw.trigger_event_update(&id).await;
            }
        }
    }
}
