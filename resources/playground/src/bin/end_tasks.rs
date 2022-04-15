use sompas_utils::task_handler;
use std::thread;
use std::time::Duration;

#[tokio::main]
async fn main() {
    println!("Hello, world!");

    tokio::spawn(async {
        let mut receiver = task_handler::subscribe_new_task();
        loop {
            tokio::select! {
                string = wait() => {
                    println!("1: {}", string);
                }
                fut = receiver.recv() => {
                    let bool = fut.expect("could not receive");
                    println!("1: end signal: {}", bool);
                }
            }
        }
    });

    tokio::spawn(async {
        let mut receiver = task_handler::subscribe_new_task();
        loop {
            tokio::select! {
                string = wait() => {
                    println!("2: {}", string);
                }
                fut = receiver.recv() => {
                    let bool = fut.expect("could not receive");
                    println!("2: end signal: {}", bool)
                }
            }
        }
    });

    thread::sleep(Duration::from_millis(5000));

    task_handler::end_all();
    thread::sleep(Duration::from_millis(100));
}

async fn wait() -> String {
    tokio::time::sleep(Duration::from_millis(1000)).await;
    "OK".to_string()
}
