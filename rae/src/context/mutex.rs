use log::info;
use ompas_lisp::core::structs::lvalue::LValue;
use std::collections::VecDeque;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

const TOKIO_CHANNEL_SIZE: usize = 16;

lazy_static! {
    pub static ref MUTEXES: MutexMap = Default::default();
}

#[derive(Clone)]
pub struct Waiter {
    pub sender: mpsc::Sender<bool>,
    pub priority: usize,
}

#[derive(Default, Clone)]
pub struct RaeMutex {
    fifo: VecDeque<Waiter>,
}

impl RaeMutex {
    pub fn new_waiter(&mut self, priority: usize) -> mpsc::Receiver<bool> {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let waiter = Waiter {
            sender: tx,
            priority,
        };
        self.fifo.push_back(waiter);
        self.fifo
            .make_contiguous()
            .sort_by(|w1, w2| w1.priority.cmp(&w2.priority));
        rx
    }

    pub async fn release(&mut self) -> bool {
        if let Some(waiter) = self.fifo.pop_front() {
            waiter
                .sender
                .send(true)
                .await
                .expect("error releasing the mutex");
            false
        } else {
            true
        }
    }
}

pub async fn lock(r: String, p: usize) -> MutexResponse {
    MUTEXES.lock(r, p).await
}

pub async fn release(lv: LValue) {
    MUTEXES.release(lv).await
}

pub async fn is_locked(lv: LValue) -> bool {
    MUTEXES.is_locked(lv).await
}

pub async fn get_list_locked() -> Vec<String> {
    MUTEXES.get_list_locked().await
}

pub async fn get_debug() -> String {
    MUTEXES.get_debug().await
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
    pub async fn lock(&self, r: String, p: usize) -> MutexResponse {
        info!("locking {} with priority {}", r, p);
        let mut locked = self.map.lock().await;
        if locked.contains_key(&r) {
            info!("already locked {}", r);
            let waiter = locked.get_mut(&r).unwrap().new_waiter(p);
            MutexResponse::Wait(waiter)
        } else {
            info!("not locked yet {}!", r);
            locked.insert(r, Default::default());
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

    pub async fn get_list_locked(&self) -> Vec<String> {
        self.map
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<String>>()
    }

    pub async fn get_debug(&self) -> String {
        let locked: Vec<(String, usize)> = self
            .map
            .lock()
            .await
            .iter()
            .map(|k_v| (k_v.0.clone(), k_v.1.fifo.len()))
            .collect();
        let mut str = "mutexes:\n".to_string();

        for e in locked {
            str.push_str(format!("- {}: {} waiting\n", e.0, e.1).as_str());
        }
        str
    }
}
