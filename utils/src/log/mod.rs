#![allow(deprecated)]
use crate::task_handler::subscribe_new_task;
use std::process::Command;
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use tokio::sync::mpsc;

const RAE_LOG_IP_ADDR: &str = "127.0.0.1:10001";
const TOKIO_CHANNEL_SIZE: usize = 16_384;
pub const END_MSG: &str = "END";

lazy_static! {
    static ref LOGGER: Logger = init();
}

pub struct Logger {
    tx: mpsc::Sender<String>,
}

impl Logger {}

///Send a msg to the Logger
pub fn send(string: String) {
    //println!("sending: {}", string);
    tokio::spawn(async move {
        LOGGER.tx.send(string).await.expect("no sender to logger");
    });
}

///Initiate new terminal and logger
fn init() -> Logger {
    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    tokio::spawn(async move { run_logger(rx).await });

    Logger { tx }
}

async fn run_logger(mut rx: mpsc::Receiver<String>) {
    Command::new("gnome-terminal")
        .args(&["--", "python3", "utils/src/log/logger.py", "&"])
        .spawn()
        .expect("could not spawn terminal");

    tokio::time::sleep(Duration::from_millis(500)).await;

    let mut end_receiver = subscribe_new_task();
    let mut stream = TcpStream::connect(RAE_LOG_IP_ADDR)
        .await
        .expect("could not open stream");
    //println!("logger running");
    loop {
        tokio::select! {
            str_log = rx.recv() => {
                match str_log {
                    None => {
                        eprintln!("error on receiver");
                        break;
                    }
                    Some(str) => {
                        stream.write_all(str.as_bytes()).await.expect("could not send to RAE Logger");
                    }
                }
            }
            _ = end_receiver.recv() => {
                println!("logger ended");
                stream.write_all(END_MSG.as_bytes()).await.expect("could not send to RAE Logger");
                break;
            }
        }
    }
}
