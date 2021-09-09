//! Contains the function to handle the REPL of the project.
//! The repl is based on the project rustyline.
//!
//! It contains only one function (for the moment): run that takes two arguments.
use crate::language::scheme_primitives::NIL;
use crate::TOKIO_CHANNEL_SIZE;
use chrono::{DateTime, Utc};
use ompas_utils::task_handler::{subscribe_new_task, EndSignal};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::{env, fs};
use tokio::sync::broadcast;
use tokio::sync::mpsc::{self, Receiver, Sender};
use tokio::task::JoinHandle;

///Spawn repl task
pub async fn spawn_repl(sender: Sender<String>) -> Option<Sender<String>> {
    let (sender_repl, receiver_repl) = mpsc::channel(TOKIO_CHANNEL_SIZE);
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

/// Spawn stdin task.
/// Not used anymore.
/*pub async fn spawn_stdin(sender: Sender<String>) -> Option<Sender<String>> {
    let (sender_stdin, receiver_stdin) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    tokio::spawn(async move {
        stdin(sender, receiver_stdin).await;
    });

    Some(sender_stdin)
}*/

/// Spawn stdout task
/// Not used anymore.
/*pub async fn spawn_stdout() -> Option<Sender<String>> {
    let (sender_stdout, receiver_stdout): (Sender<String>, Receiver<String>) =
        mpsc::channel(TOKIO_CHANNEL_SIZE);

    tokio::spawn(async move {
        output(receiver_stdout).await;
    });

    Some(sender_stdout)
}*/

/// Spawn the log task
pub async fn spawn_log(log_path: Option<PathBuf>) -> Option<(Sender<String>, JoinHandle<()>)> {
    let (sender_log, receiver_log) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let end_receiver = subscribe_new_task();
    let handle = tokio::spawn(async move {
        log(receiver_log, log_path, end_receiver).await;
    });

    Some((sender_log, handle))
}

/// Function to handle the repl.
/// ### functioning:
/// loop waiting for an object on *stdin*
/// ### args
/// - sender: channel object to send string to lisp interpreter.
/// - receiver: channel object to receive ack from lisp interpreter after evaluation.
/// Used for synchronization.
/*async fn stdin(sender: Sender<String>, mut receiver: Receiver<String>) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(string) => {
                rl.add_history_entry(string.clone());
                sender
                    .send(format!("repl:{}", string))
                    .await
                    .expect("couldn't send lisp command");
                let buffer = receiver.recv().await.expect("error receiving");
                assert_eq!(
                    buffer, "ACK",
                    "should receive an ack from Lisp Intrepretor and nothing else"
                );
                //println!("repl ack: {}", buffer);
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
        .await
        .expect("couldn't send exit msg");
    rl.save_history("history.txt").unwrap();
}*/

async fn log(
    mut receiver: Receiver<String>,
    working_dir: Option<PathBuf>,
    mut end_receiver: broadcast::Receiver<EndSignal>,
) {
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();

    let dir_path: PathBuf = match working_dir {
        Some(wd) => {
            let mut dir_path = wd;
            dir_path.push("lisp_logs");
            dir_path
        }
        None => format!(
            "{}/ompas/lisp_logs",
            match env::var("HOME") {
                Ok(val) => val,
                Err(_) => ".".to_string(),
            }
        )
        .into(),
    };

    fs::create_dir_all(&dir_path).expect("could not create logs directory");
    let mut file_path = dir_path.clone();
    file_path.push(format!("log_{}", string_date));
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
async fn repl(sender: Sender<String>, mut receiver: Receiver<String>) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        //TODO: to handle side effects when other user wants to connect to lisp.
        //TODO: add multiline support
        let readline = rl.readline(">> ");

        match readline {
            Ok(string) => {
                rl.add_history_entry(string.clone());
                sender
                    .send(format!("repl:{}", string))
                    .await
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

                /*assert_eq!(
                    buffer, "ACK",
                    "should receive an ack from Lisp Intrepretor and nothing else"
                );*/
                //println!("repl ack: {}", buffer);
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
        .await
        .expect("couldn't send exit msg");
    rl.save_history("history.txt").unwrap();
}

pub const EXIT_CODE_STDOUT: &str = "EXIT";

/*async fn output(mut receiver: Receiver<String>) {
    let mut end_receiver = ompas_utils::task_handler::subscribe_new_task();
    println!("output launched");
    loop {
        tokio::select! {
            str = receiver.recv() => {
                let str = str.expect("could not receive");

                let mut stdout = std::io::stdout();
                stdout.lock();
                stdout
                    .write_all(format!("{}\n", str).as_bytes())
                    .expect("could not print to stdout");
                drop(stdout);
            }
            _ = end_receiver.recv() => {
                println!("output task ended");
                break;
            }
        }

        //TODO: check if it always works.
        //print!("{}\n", str);
    }
}*/
