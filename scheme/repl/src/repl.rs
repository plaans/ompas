//! Contains the function to handle the REPL of the project.
//! The repl is based on the project rustyline.
//!
//! It contains only one function (for the moment): run that takes two arguments.
use ompas_middleware::{Master, OMPAS_WORKING_DIR};
use ompas_utils::task_handler::{subscribe_new_task, EndSignal};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use sompas_language::kind::NIL;
use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use tokio::sync::broadcast;
use tokio::sync::mpsc::{self, UnboundedReceiver, UnboundedSender};
use tokio::task::JoinHandle;

///Spawn repl task
pub async fn spawn_repl(sender: UnboundedSender<String>) -> Option<UnboundedSender<String>> {
    let (sender_repl, receiver_repl) = mpsc::unbounded_channel();
    let mut end_receiver = subscribe_new_task();
    tokio::spawn(async move {
        tokio::select! {
            _ = repl(sender, receiver_repl) => {
            }
            _ = end_receiver.recv() => {
                println!("task_handler killed repl.")
            }
        }
    });

    Some(sender_repl)
}

/// Spawn the log task
pub async fn spawn_log(
    log_path: Option<PathBuf>,
) -> Option<(UnboundedSender<String>, JoinHandle<()>)> {
    let (sender_log, receiver_log) = mpsc::unbounded_channel();

    let end_receiver = subscribe_new_task();
    let handle = tokio::spawn(async move {
        log(receiver_log, log_path, end_receiver).await;
    });

    Some((sender_log, handle))
}

async fn log(
    mut receiver: UnboundedReceiver<String>,
    working_dir: Option<PathBuf>,
    mut end_receiver: broadcast::Receiver<EndSignal>,
) {
    let dir_path: PathBuf = match working_dir {
        Some(wd) => {
            let mut dir_path = wd;
            dir_path.push("sompas_logs");
            dir_path
        }
        None => format!("{}/lisp", OMPAS_WORKING_DIR.get_ref()).into(),
    };

    fs::create_dir_all(&dir_path).expect("could not create logs directory");
    let mut file_path = dir_path.clone();
    file_path.push(format!("log_{}", Master::get_string_date()));
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(&file_path)
        .expect("error creating log file");

    loop {
        tokio::select! {
            buffer = receiver.recv() => {
                let buffer = match buffer {
                    None => {
                        eprintln!("log task stopped working");
                        break;
                    }
                    Some(b) => b,
                };
            file.write_all(format!("{}\n", buffer).as_bytes())
                .expect("could not write to log file");
            }
            _ = end_receiver.recv() => {
                receiver.close();
                break;
            }
        }
    }
    println!("Draining log queue...");
    while let Some(msg) = receiver.recv().await {
        file.write_all(format!("{}\n", msg).as_bytes())
            .expect("could not write to log file");
    }
    println!("log task ended");
}

/// Function to handle the repl.
/// ### functioning:
/// loop waiting for an object on *stdin*
/// ### args
/// - sender: channel object to send string to lisp interpreter.
/// - receiver: channel object to receive ack from lisp interpreter after evaluation.
/// Used for synchronization.
#[warn(deprecated)]
async fn repl(sender: UnboundedSender<String>, mut receiver: UnboundedReceiver<String>) {
    let mut rl = DefaultEditor::new().unwrap();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(string) => {
                rl.add_history_entry(string.clone()).unwrap();
                sender
                    .send(format!("repl:{}", string))
                    .expect("couldn't send lisp command");
                let buffer = match receiver.recv().await {
                    None => {
                        eprintln!("repl task stopped working");
                        break;
                    }
                    Some(b) => b,
                };
                if buffer != NIL {
                    println!("LI>> {}", buffer);
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    sender
        .send("exit".to_string())
        .expect("couldn't send exit msg");
    rl.save_history("history.txt").unwrap();
}

pub const EXIT_CODE_STDOUT: &str = "EXIT";
