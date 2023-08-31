use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::state::state_update_manager::StateUpdateSubscriber;
use ompas_language::process::*;
use ompas_middleware::logger::LogClient;
use ompas_middleware::{LogLevel, ProcessInterface};
use sompas_core::eval;
use sompas_structs::lenv::LEnv;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{oneshot, Mutex};

#[derive(Clone)]
pub struct MonitorManager {
    inner: Arc<Mutex<MonitorCollectionInner>>,
    clock_manager: ClockManager,
}

impl From<ClockManager> for MonitorManager {
    fn from(value: ClockManager) -> Self {
        Self {
            inner: Default::default(),
            clock_manager: value,
        }
    }
}

#[derive(Default)]
pub struct MonitorCollectionInner {
    map: HashMap<WaitForId, WaitFor>,
    id: WaitForId,
}

impl MonitorCollectionInner {
    pub fn new_id(&mut self) -> WaitForId {
        let temp = self.id;
        self.id += 1;
        temp
    }
}

impl MonitorManager {
    pub async fn add_waiter(&self, lambda: LValue) -> WaitForReceiver {
        let (tx, rx) = oneshot::channel();
        let mut inner = self.inner.lock().await;
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
        self.inner.lock().await.map.remove(&id);
    }

    pub async fn check_wait_for(&self, mut env: LEnv, log: LogClient) {
        //let mut item_to_remove: Vec<WaitForId> = vec![];
        let mut waiters = self.inner.lock().await;
        let keys: Vec<_> = waiters.map.keys().copied().collect();
        for id in &keys {
            let lambda: &LValue = &waiters.map.get(id).unwrap().lambda;
            let result = eval(lambda, &mut env, None).await;
            match result {
                Ok(lv) => {
                    if let LValue::True = lv {
                        let waiter = waiters.map.remove(id).unwrap();
                        waiter.channel.send(true).expect("");
                        log.debug(format!("{} became true", waiter.lambda.to_string()));
                    }
                }
                Err(e) => {
                    log.warn(format!("error checking wait on: {e}"));
                }
            }
        }
    }

    pub async fn get_debug(&self) -> String {
        /*let lambdas: Vec<LValue> = self
        .inner
        .lock()
        .await
        .map
        .values()
        .map(|v| v.lambda.clone())
        .collect();*/
        let mut str = "'monitored dynamic expression(s): \n".to_string();
        for wf in self.inner.lock().await.map.values() {
            str.push_str(format!("- [{:.^3}]", wf.date).as_str());
            str.push_str(wf.lambda.to_string().as_str());
            str.push('\n');
        }
        str
    }

    pub async fn clear(&self) {
        *self.inner.lock().await = Default::default()
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

pub async fn task_check_wait_for(
    mut update: StateUpdateSubscriber,
    monitors: MonitorManager,
    env: LEnv,
) {
    let mut process: ProcessInterface =
        ProcessInterface::new(PROCESS_CHECK_WAIT_FOR, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;
    process.log("check wait for launched", LogLevel::Info);
    //println!("wait for working");
    loop {
        tokio::select! {
            _ = update.channel.recv() => {
                let n_wait_on = monitors.inner.lock().await.map.len();
                if n_wait_on != 0 {
                    monitors.check_wait_for(env.clone(), process.get_log_client()).await;
                }
            }
            _ = process.recv() => {
                break;
            }
        }
    }
}
