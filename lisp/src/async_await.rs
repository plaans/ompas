#![allow(deprecated)]
use crate::structs::{LError, LValue};
//use log::info;
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

pub const TASK_HANDLER_MISSING: &str = "task handler is missing";

lazy_static! {
    static ref TASK_HANDLER: TaskHandler = launch_task_handler();
}

/// Returns a copy of the TaskHandler shared between all the threads.
pub fn current() -> TaskHandler {
    TASK_HANDLER.borrow().deref().clone()
}

pub const TOKIO_CHANNEL_SIZE: usize = 16_384;

pub type TaskResult = (usize, Result<LValue, LError>);

pub type MapWaiter = im::HashMap<usize, Option<mpsc::Sender<Result<LValue, LError>>>>;

pub type MapResult = im::HashMap<usize, Option<Result<LValue, LError>>>;

#[derive(Default, Debug, Clone)]
pub struct TaskHandler {
    pub(crate) map_result: Arc<Mutex<MapResult>>,
    pub(crate) map_waiter: Arc<Mutex<MapWaiter>>,
    pub(crate) sender: Option<mpsc::Sender<TaskResult>>,
    pub(crate) next_id: Arc<AtomicUsize>,
}

/// Type of response an await can get:
/// - If the result has already been computed by the asynchronous evaluation, then it returns the a Result.
/// - Otherwise it returns a channel on which it can wait to get the result.
pub enum AwaitResponse {
    Result(Result<LValue, LError>),
    Receiver(mpsc::Receiver<Result<LValue, LError>>),
}

impl TaskHandler {
    /// Declare a new task
    /// Returns the id of the task and the sender to the TaskHandler that will process the result.
    pub async fn declare_new_task(&self) -> (usize, mpsc::Sender<TaskResult>) {
        println!("new task declared");
        //println!("new task declared!");
        let id;
        loop {
            let temp_id = self.next_id.load(Ordering::Relaxed);
            if self
                .next_id
                .compare_exchange(temp_id, temp_id + 1, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                id = temp_id;
                break;
            }
        }
        self.map_result.lock().await.insert(id, None);
        self.map_waiter.lock().await.insert(id, None);
        (id, self.sender.clone().unwrap())
    }

    /// Returns an AwaitResponse.
    /// The Kind of AwaitResponse depends on the progress of the asynchronous evaluation.
    pub async fn get_response_await(&self, id: &usize) -> AwaitResponse {
        println!("get response!");
        let clean: bool;
        let result = match self.map_result.lock().await.get(id).unwrap() {
            None => {
                println!("result not yet available");
                clean = false;
                let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
                self.map_waiter.lock().await.insert(*id, Some(tx));
                AwaitResponse::Receiver(rx)
            }
            Some(result) => {
                println!("the result is already available");
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

fn launch_task_handler() -> TaskHandler {
    let mut task_handler = TaskHandler::default();
    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    task_handler.sender = Some(tx);
    let copy_task_handler = task_handler.clone();
    tokio::spawn(async move {
        task_watcher(copy_task_handler, rx).await;
    });
    task_handler
}

async fn task_watcher(task_handler: TaskHandler, mut receiver: mpsc::Receiver<TaskResult>) {
    println!("Task watcher launched");
    loop {
        let clean: bool;
        let (id, result) = match receiver.recv().await {
            None => panic!("task result receiver is not working"),
            Some(result) => result,
        };

        match task_handler.map_waiter.lock().await.get(&id).unwrap() {
            None => {
                println!("new result received and no waiter");
                task_handler
                    .map_result
                    .lock()
                    .await
                    .insert(id, Some(result));
                clean = false;
            }
            Some(sender) => {
                println!("new result received and a waiter is waiting on the result");
                clean = true;
                sender
                    .send(result)
                    .await
                    .expect("Could not send task result to waiter");
            }
        }

        if clean {
            task_handler.map_result.lock().await.remove(&id);
            task_handler.map_waiter.lock().await.remove(&id);
        }
    }
}
