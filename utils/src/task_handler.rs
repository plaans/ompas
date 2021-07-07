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

pub fn subscribe_new_task() -> Receiver<EndSignal> {
    TASK_END_HANDLER.broadcast.subscribe()
}

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
