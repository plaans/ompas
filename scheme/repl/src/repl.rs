//! Contains the function to handle the REPL of the project.
//! The repl is based on the project rustyline.
//!
//! It contains only one function (for the moment): run that takes two arguments.
use ompas_utils::task_handler::subscribe_new_task;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use sompas_language::kind::NIL;
use tokio::sync::mpsc::{self, UnboundedReceiver, UnboundedSender};

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
