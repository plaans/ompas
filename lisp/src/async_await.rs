use crate::structs::{LValue, LError};
use std::sync::atomic::{AtomicUsize, Ordering};
use tokio::sync::{oneshot, mpsc, Mutex};
use std::sync::Arc;
use tokio::sync::oneshot::Receiver;
use crate::structs::LCoreOperator::Await;

pub const TOKIO_CHANNEL_SIZE: usize  = 16_384;

pub type TaskResult = (usize, Result<LValue, LError>);

#[derive(Default, Debug, Clone)]
pub struct TaskHandler {
    map_result: Arc<Mutex<im::HashMap<usize, Option<Result<LValue, LError>>>>>,
    map_waiter: Arc<Mutex<im::HashMap<usize, Option<mpsc::Sender<Result<LValue,LError>>>>>>,
    sender: Option<mpsc::Sender<TaskResult>>,
    next_id: Arc<AtomicUsize>
}

pub enum AwaitResponse {
    Result(Result<LValue, LError>),
    Receiver(mpsc::Receiver<Result<LValue, LError>>),
}

impl TaskHandler {
    pub async fn declare_new_task(&self) -> (usize, mpsc::Sender<TaskResult>) {
        let id = self.next_id.load(Ordering::Relaxed);
        self.next_id.store(id+1,Ordering::Relaxed);
        self.map_result.lock().await.insert(id, None);
        self.map_waiter.lock().await.insert(id, None);
        (id, self.sender.clone().unwrap())
    }

    pub async fn get_response_await(&self, id: &usize) -> AwaitResponse {
        let clean: bool;
        let result = match self.map_result.lock().await.get(id).unwrap() {
            None => {
                clean = false;
                let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
                self.map_waiter.lock().await.insert(*id, Some(tx));
                AwaitResponse::Receiver(rx)
            }
            Some(result) => {
                clean = true;
                AwaitResponse::Result(result.clone())
            }
        };
        if clean {
            self.map_result.lock().await.remove(id);
            self.map_waiter.lock().await.remove(id);
        }

        result
    }
}

pub async fn task_watcher(task_handler: TaskHandler, mut receiver: mpsc::Receiver<TaskResult>) {
    loop {

        let clean:bool;
        let (id, result) = match receiver.recv().await {
            None => panic!("task result receiver is not working"),
            Some(result) => result
        };

        match task_handler.map_waiter.lock().await.get(&id).unwrap(){
            None => {
                task_handler.map_result.lock().await.insert(id, Some(result));
                clean = false;
            }
            Some(sender) => {
                clean = true;
                sender.send(result);

            }
        }

        if clean {
            task_handler.map_result.lock().await.remove(&id);
            task_handler.map_waiter.lock().await.remove(&id);
        }
    }
}
