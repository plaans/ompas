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
    fifo: VecDeque<(usize, Waiter)>,
    n: usize,
}

pub struct Wait {
    pub rx: mpsc::Receiver<bool>,
    pub index: usize,
}

impl RaeMutex {
    pub fn remove_waiter(&mut self, index: usize) {
        for (i, (n, _)) in self.fifo.iter().enumerate() {
            if *n == index {
                self.fifo.remove(i);
                return;
            }
        }
        unreachable!("remove waiter did not work")
    }

    fn new_index(&mut self) -> usize {
        self.n += 1;
        self.n - 1
    }

    pub fn new_waiter(&mut self, priority: usize) -> Wait {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let waiter = Waiter {
            sender: tx,
            priority,
        };
        let index = self.new_index();
        let previous_len = self.fifo.len();
        let value = (index, waiter);
        if self.fifo.len() == 0 {
            self.fifo.push_back(value);
        } else {
            for (i, (_, w)) in self.fifo.iter().enumerate() {
                if w.priority < value.1.priority {
                    self.fifo.insert(i, value);
                    break;
                } else if i == self.fifo.len() - 1 {
                    self.fifo.push_back(value);
                    break;
                }
            }
        }

        assert_eq!(previous_len + 1, self.fifo.len());

        Wait { rx, index }
    }

    pub async fn release(&mut self) -> bool {
        while let Some((_, waiter)) = self.fifo.pop_front() {
            if let Ok(_) = waiter.sender.try_send(true) {
                return false;
            }
        }
        return true;
    }
}

pub async fn lock(r: String, p: usize) -> MutexResponse {
    MUTEXES.lock(r, p).await
}

pub async fn release(r: String) {
    MUTEXES.release(r).await
}

pub async fn remove_waiter(resource: String, index: usize) {
    MUTEXES.remove_waiter(resource, index).await
}

pub async fn is_locked(r: String) -> bool {
    MUTEXES.is_locked(r).await
}

pub async fn get_list_locked() -> Vec<String> {
    MUTEXES.get_list_locked().await
}

pub async fn get_debug() -> String {
    MUTEXES.format().await
}

pub enum MutexResponse {
    Ok,
    Wait(Wait),
}

#[derive(Default)]
pub struct MutexMap {
    map: Arc<Mutex<im::HashMap<String, RaeMutex>>>,
}

impl MutexMap {
    pub async fn lock(&self, r: String, p: usize) -> MutexResponse {
        //info!("locking {} with priority {}", r, p);
        let mut locked = self.map.lock().await;
        if locked.contains_key(&r) {
            //info!("already locked {}", r);
            let waiter = locked.get_mut(&r).unwrap().new_waiter(p);
            MutexResponse::Wait(waiter)
        } else {
            //info!("not locked yet {}!", r);
            locked.insert(r, Default::default());
            MutexResponse::Ok
        }
    }

    pub async fn release(&self, key: String) {
        let mut locked = self.map.lock().await;
        //info!("releasing {}", key);
        if locked.get_mut(&key).unwrap().release().await {
            //info!("no one waiting for the lock on {}", key);
            locked.remove(&key);
        }
    }

    pub async fn remove_waiter(&self, resource: String, index: usize) {
        let mut locked = self.map.lock().await;
        locked.get_mut(&resource).unwrap().remove_waiter(index);
    }

    pub async fn is_locked(&self, r: String) -> bool {
        self.map.lock().await.contains_key(&r)
    }

    pub async fn get_list_locked(&self) -> Vec<String> {
        self.map
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<String>>()
    }

    pub async fn format(&self) -> String {
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
