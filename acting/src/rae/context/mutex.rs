use log::{info, warn};
use ompas_lisp::structs::LValue;
use std::collections::VecDeque;
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot, Mutex, RwLock};

const TOKIO_CHANNEL_SIZE: usize = 16;

lazy_static! {
    pub static ref MUTEXES: MutexMap = Default::default();
}

#[derive(Default, Clone)]
pub struct RaeMutex {
    fifo: VecDeque<mpsc::Sender<bool>>,
}

impl RaeMutex {
    pub fn new_waiter(&mut self) -> mpsc::Receiver<bool> {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        self.fifo.push_back(tx);
        rx
    }

    pub async fn release(&mut self) -> bool {
        if let Some(waiter) = self.fifo.pop_front() {
            waiter.send(true).await.expect("error releasing the mutex");
            false
        } else {
            true
        }
    }
}

pub async fn lock(lv: LValue) -> MutexResponse {
    MUTEXES.lock(lv).await
}

pub async fn release(lv: LValue) {
    MUTEXES.release(lv).await
}

pub async fn is_locked(lv: LValue) -> bool {
    MUTEXES.is_locked(lv).await
}

pub enum MutexResponse {
    Ok,
    Wait(mpsc::Receiver<bool>),
}

#[derive(Default)]
pub struct MutexMap {
    map: Arc<Mutex<im::HashMap<String, RaeMutex>>>,
}

impl MutexMap {
    pub async fn lock(&self, lv: LValue) -> MutexResponse {
        let key: String = lv.to_string();
        info!("locking {}", key);
        let mut locked = self.map.lock().await;
        if locked.contains_key(&key) {
            info!("already locked {}", key);
            let waiter = locked.get_mut(&key).unwrap().new_waiter();
            MutexResponse::Wait(waiter)
        } else {
            info!("not locked yet {}!", key);
            locked.insert(key, Default::default());
            MutexResponse::Ok
        }
    }

    pub async fn release(&self, lv: LValue) {
        let key: String = lv.to_string();
        let mut locked = self.map.lock().await;
        info!("releasing {}", key);
        if locked.get_mut(&key).unwrap().release().await {
            info!("no one waiting for the lock on {}", key);
            locked.remove(&key);
        }
    }

    pub async fn is_locked(&self, lv: LValue) -> bool {
        self.map.lock().await.contains_key(&lv.to_string())
    }
}
