//! TaskEndHandler is a special structs that can be called to shutdown each task that has been launched.
//! To use TaskEndHandler to shutdown a task, it needs to subscribe.
//! It will receive a channel on which it can await the signal *true*.
//! # Example
//! ```ignore
//! #[tokio::main]
//! async fn main(){
//!     use std::time::Duration;
//!     use ompas_utils::task_handler::end_all;
//!     task();
//!     tokio::time::sleep(Duration::from_secs(1)).await;
//!     end_all();
//! }
//!
//! async fn task() {
//!     use ompas_utils::task_handler::subscribe_new_task;
//!     let end_receiver = subscribe_new_task();
//!     loop {
//!         tokio::select! {
//!             end = end_receiver => {
//!                 println!("received end signal");
//!                 break;            
//!             }
//!             other = tokio::time::sleep(tokio::time::Duration::from_secs(1)) => {
//!                 println!("has await an other second");            
//!             }
//!         }
//!     }
//! }
//!
//! ```
#![allow(deprecated)]
use std::thread;
use std::time::Duration;
use tokio::sync::broadcast;
use tokio::sync::broadcast::{Receiver, Sender};
lazy_static! {
    static ref TASK_END_HANDLER: TaskEndHandler = launch_task_end_handler();
}

const TOKIO_CHANNEL_TOKIO_TASK_END_SIZE: usize = 64;

pub type EndSignal = bool;

struct TaskEndHandler {
    broadcast: Sender<EndSignal>,
}

/// Returns the channel on which the task has to await the end signal
pub fn subscribe_new_task() -> Receiver<EndSignal> {
    TASK_END_HANDLER.broadcast.subscribe()
}

/// Sends to all subscribers the end signal.
pub fn end_all() {
    println!(
        "number of subscriber(s) to end task: {}",
        TASK_END_HANDLER.broadcast.receiver_count()
    );

    if TASK_END_HANDLER.broadcast.receiver_count() != 0 {
        TASK_END_HANDLER
            .broadcast
            .send(true)
            .expect("could not kill tasks");
        thread::sleep(Duration::from_millis(1000));
    } else {
        println!("no tasks to kill");
    }
}

fn launch_task_end_handler() -> TaskEndHandler {
    let (tx, _) = broadcast::channel(TOKIO_CHANNEL_TOKIO_TASK_END_SIZE);
    TaskEndHandler { broadcast: tx }
}
