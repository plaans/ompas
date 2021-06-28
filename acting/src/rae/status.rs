use tokio::sync::mpsc::{Receiver, Sender};
use crate::rae::context::Status;
use im::HashMap;
use std::sync::{RwLock, Arc};
use crate::rae::state::{ActionStatus, ActionStatusSet};
use tokio::sync::Mutex;

pub struct ActionStatusWatcher {
    pub map: Arc<Mutex<HashMap<usize, Option<Receiver<bool>>>>>,
}

impl ActionStatusWatcher {
    /*pub async fn get_watcher(&self, action_id: usize) -> Option<Receiver<bool>> {
        let value = self.map.lock().await.insert(action_id, None).unwrap();
        value
    }*/
}

#[derive(Default, Clone, Debug)]
pub struct ActionStatusSync {
    pub map: Arc<RwLock<HashMap<usize, Sender<bool>>>>,
    pub sender: Option<Sender<usize>>
}

impl ActionStatusSync {
    pub async fn trigger_event_update(&self, id: &usize) {
        let sender = self.map.read().unwrap().get(id).unwrap().clone();
        sender.send(true).await;
    }

    pub fn add_action_to_watch(&self, action_id: usize, watcher : Sender<bool>) {
        match self.map.write().unwrap().insert(action_id, watcher) {
            None => {}
            Some(_) => panic!("action_id was already defined"),
        }
    }
}

pub async fn async_status_watcher_run(mut asw: ActionStatusSync, mut receiver: Receiver<usize>) {
    loop {
        match receiver.recv().await {
            None => {
                panic!("error from receiver")
            },
            Some(id) => {
                //println!("action status of {} is updated", id);
                asw.trigger_event_update(&id).await;
            }
        }
    }
}