//! This module add some features to monitor logs in a different terminal from the one in which the program is running.
//! It proposes two implementations:
//!     - One using logger.py, a python script received logs via TCP
//!     - The other using *tail -f* to monitor logs written to a file
//! # Examples
//! The logger will be automatically built when methods using it are called
//! ```ignore
//! info!("new log string");
//! /*This will send via a channel the string to be passed
//! then to the task handling writing to the socket or the file.*/
//! ```

#![allow(deprecated)]

use std::fs::OpenOptions;
use std::io::Write;
use std::process::{Command, Stdio};
use std::time::Duration;

use log::{Level, LevelFilter, Log, Metadata, Record, SetLoggerError};
use tokio::io::AsyncWriteExt;
use tokio::net::TcpStream;
use tokio::sync::{broadcast, mpsc};

use ompas_utils::task_handler::{subscribe_new_task, EndSignal};
use std::path::PathBuf;

const RAE_LOG_IP_ADDR: &str = "127.0.0.1:10001";
const TOKIO_CHANNEL_SIZE: usize = 16_384;
pub const END_MSG: &str = "END";

/// Struct to wrap a tokio::sync::mpsc tx channel
pub struct Logger {
    tx: mpsc::Sender<String>,
}

impl Logger {
    fn new(log_path: PathBuf, killer: broadcast::Receiver<EndSignal>) -> Logger {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        tokio::spawn(async move { run_logger_file(rx, log_path, killer).await });
        //tokio::spawn(async move { run_logger(rx).await });

        Logger { tx }
    }
}

impl Log for Logger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            let string = format!("{} - {}", record.level(), record.args());
            let tx = self.tx.clone();
            tokio::spawn(async move {
                tx.send(string).await.expect("no sender to logger");
            });
        }
    }

    fn flush(&self) {
        todo!()
    }
}

/// Initiate new terminal and logger
/// Build the global object
pub fn init(log_path: PathBuf) -> Result<mpsc::Sender<String>, SetLoggerError> {
    let killer = subscribe_new_task();
    let logger = Logger::new(log_path, killer);
    let sender = logger.tx.clone();
    match log::set_boxed_logger(Box::new(logger)).map(|()| log::set_max_level(LevelFilter::Info)) {
        Err(e) => Err(e),
        Ok(_) => Ok(sender),
    }
}

pub fn display_logger(mut killer: broadcast::Receiver<EndSignal>, log_path: PathBuf) {
    tokio::spawn(async move {
        let child = Command::new("gnome-terminal")
            .args(["--title", "RAE LOG", "--disable-factory"])
            .args(["--", "tail", "-f", log_path.to_str().unwrap()])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("could not spawn terminal");
        killer.recv().await.expect("error on receiver");
        //println!("killing rae log process : {}", child.id());
        Command::new("pkill")
            .args(["-P", child.id().to_string().as_str()])
            .spawn()
            .expect("error on killing process");
    });
}

/// Task that is running asynchronously
/// It receives string to log via a channel and write it to the log file of the session
///
/// Log files are stored in the <current>/rae_logs.
/// Files names are formatted in function of the date and time at which the script is launched.
async fn run_logger_file(
    mut rx: mpsc::Receiver<String>,
    log_path: PathBuf,
    mut killer: broadcast::Receiver<EndSignal>,
) {
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(&log_path)
        .expect("error creating log file");

    file.write_all("RAE LOG\n\n".as_bytes())
        .expect("could not write to RAE log file.");

    //let mut logger_pid = None;
    //let mut first = true;
    //println!("child pid: {}", child.as_ref().unwrap().id());

    loop {
        tokio::select! {
            str_log = rx.recv() => {
                match str_log {
                    None => {
                        eprintln!("error on receiver");
                        break;
                    }
                    Some(str) => {
                        file.write_all(format!("{}\n", str).as_bytes()).expect("could not write to RAE log file");
                    }
                }
            }
            _ = killer.recv() => {
                println!("Process RAE LOG killed.");
                break;
            }
        }
    }
    //println!("Draining RAE log queue...");
    while let Some(msg) = rx.recv().await {
        file.write_all(format!("{}\n", msg).as_bytes())
            .expect("could not write to RAE log file");
    }
    file.write_all(END_MSG.as_bytes())
        .expect("could not write to RAE log file");
    //println!("RAE LOG task ended.");
}

/// Sends via tcp to logger.py strings that need to be logged.
/// run_logger is an asynchronous task that await a message to send via tcp.
#[allow(unused)]
async fn run_logger(mut rx: mpsc::Receiver<String>) {
    Command::new("gnome-terminal")
        .args(["--", "python3", "utils/src/log/logger.py", "&"])
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