#![allow(deprecated)]
use crate::structs::{LError, LValue};
//use log::info;
use crate::core::get_debug;
use crate::TOKIO_CHANNEL_SIZE;
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

pub const TASK_HANDLER_MISSING: &str = "task handler is missing";

lazy_static! {
    static ref TASK_HANDLER: ArcTaskHandler = launch_task_handler();
}

/// Returns a copy of the TaskHandler shared between all the threads.
pub fn current() -> ArcTaskHandler {
    TASK_HANDLER.borrow().deref().clone()
}

pub type TaskResult = (usize, Result<LValue, LError>);

pub type MapWaiter = im::HashMap<usize, Option<mpsc::Sender<Result<LValue, LError>>>>;

pub type MapResult = im::HashMap<usize, Option<Result<LValue, LError>>>;

#[derive(Default, Debug, Clone)]
pub struct TaskHandler {
    pub(crate) map_result: MapResult,
    pub(crate) map_waiter: MapWaiter,
    pub(crate) sender: Option<mpsc::Sender<TaskResult>>,
    pub(crate) next_id: Arc<AtomicUsize>,
}

#[derive(Default, Debug, Clone)]
pub struct ArcTaskHandler(Arc<Mutex<TaskHandler>>);

/// Type of response an await can get:
/// - If the result has already been computed by the asynchronous evaluation, then it returns the a Result.
/// - Otherwise it returns a channel on which it can wait to get the result.
#[derive(Debug)]
pub enum AwaitResponse {
    Result(Result<LValue, LError>),
    Receiver(mpsc::Receiver<Result<LValue, LError>>),
}

impl ArcTaskHandler {
    /// Declare a new task
    /// Returns the id of the task and the sender to the TaskHandler that will process the result.
    pub async fn declare_new_task(&self) -> (usize, mpsc::Sender<TaskResult>) {
        if get_debug() {
            println!("new task declared");
        }

        let mut locked_task_handler = self.0.lock().await;

        let id;
        loop {
            let temp_id = locked_task_handler.next_id.load(Ordering::Relaxed);
            if locked_task_handler
                .next_id
                .compare_exchange(temp_id, temp_id + 1, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                id = temp_id;
                break;
            }
        }
        locked_task_handler.map_result.insert(id, None);
        locked_task_handler.map_waiter.insert(id, None);
        (id, locked_task_handler.sender.clone().unwrap())
    }

    /// Returns an AwaitResponse.
    /// The Kind of AwaitResponse depends on the progress of the asynchronous evaluation.
    pub async fn get_response_await(&self, id: &usize) -> AwaitResponse {
        if get_debug() {
            println!("get_response_await>>fetching result");
        }
        let mut locked_task_handler = self.0.lock().await;
        let clean: bool;
        let result = match locked_task_handler.map_result.get(id).unwrap() {
            None => {
                if get_debug() {
                    println!("get_response_await>>result not yet available");
                }
                clean = false;
                let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
                locked_task_handler.map_waiter.insert(*id, Some(tx));
                AwaitResponse::Receiver(rx)
            }
            Some(result) => {
                if get_debug() {
                    println!("get_response_await>>the result is already available");
                }
                clean = true;
                AwaitResponse::Result(result.clone())
            }
        };
        if clean {
            locked_task_handler.map_result.remove(id);
            locked_task_handler.map_waiter.remove(id);
        }
        if get_debug() {
            println!("get_response_await>>return result {:?}", result);
        }

        result
    }
}

fn launch_task_handler() -> ArcTaskHandler {
    let mut task_handler = TaskHandler::default();
    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    task_handler.sender = Some(tx);
    let arc_task_handler = ArcTaskHandler(Arc::new(Mutex::new(task_handler)));
    let copy_task_handler = arc_task_handler.clone();
    tokio::spawn(async move {
        task_watcher(copy_task_handler, rx).await;
    });
    arc_task_handler
}

async fn task_watcher(task_handler: ArcTaskHandler, mut receiver: mpsc::Receiver<TaskResult>) {
    if get_debug() {
        println!("Task watcher launched");
    }
    loop {
        let clean: bool;
        let (id, result) = match receiver.recv().await {
            None => panic!("task_watcher>>task result receiver is not working"),
            Some(result) => result,
        };

        let mut locked_task_handler = task_handler.0.lock().await;

        match locked_task_handler.map_waiter.get(&id).unwrap() {
            None => {
                if get_debug() {
                    println!("task_watcher>>new result received and no waiter");
                }
                locked_task_handler.map_result.insert(id, Some(result));
                clean = false;
            }
            Some(sender) => {
                if get_debug() {
                    println!(
                        "task_watcher>>new result received and a waiter is waiting on the result"
                    );
                }
                clean = true;
                sender
                    .send(result)
                    .await
                    .expect("Could not send task result to waiter");
            }
        }

        if clean {
            locked_task_handler.map_waiter.remove(&id);
            locked_task_handler.map_result.remove(&id);
        }
    }
}
