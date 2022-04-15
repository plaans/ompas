use crate::TOKIO_CHANNEL_SIZE;
use chrono::{DateTime, Utc};
use im::HashMap;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use sompas_core::{eval, eval_init, get_root_env, parse};
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::module::IntoModule;
use sompas_utils::task_handler;
use sompas_utils::task_handler::{subscribe_new_task, EndSignal};
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::{env, fs};
use tokio::sync::broadcast;
use tokio::sync::mpsc::error::SendError;
use tokio::sync::mpsc::{channel, Receiver, Sender};
use tokio::task::JoinHandle;

#[derive(Debug)]
pub struct LispInterpreterChannel {
    sender: Sender<String>,
    receiver: Receiver<String>,
    subscribers: HashMap<usize, Sender<String>>,
    next_id: usize,
}

impl Default for LispInterpreterChannel {
    fn default() -> Self {
        let (sender, receiver) = channel(TOKIO_CHANNEL_SIZE);

        Self {
            sender,
            receiver,
            subscribers: Default::default(),
            next_id: 0,
        }
    }
}

impl LispInterpreterChannel {
    pub fn get_new_subscriber(&mut self) -> ChannelToLispInterpreter {
        let id = self.next_id;
        self.next_id += 1;

        let (tx, rx) = tokio::sync::mpsc::channel(TOKIO_CHANNEL_SIZE);
        self.subscribers.insert(id, tx);

        ChannelToLispInterpreter {
            sender: self.sender.clone(),
            receiver: rx,
            id,
        }
    }

    pub async fn recv(&mut self) -> Option<String> {
        self.receiver.recv().await
    }

    pub async fn send(&mut self, id: &usize, msg: String) -> Result<(), SendError<String>> {
        self.subscribers
            .get(id)
            .expect("error getting subscriber's sender")
            .send(msg)
            .await
    }
}
#[derive(Debug)]
pub struct ChannelToLispInterpreter {
    sender: Sender<String>,
    receiver: Receiver<String>,
    id: usize,
}

impl ChannelToLispInterpreter {
    pub async fn send(&self, msg: String) -> Result<(), SendError<String>> {
        self.sender.send(self.id.to_string()).await?;
        self.sender.send(msg).await?;
        Ok(())
    }

    pub async fn recv(&mut self) -> Option<String> {
        self.receiver.recv().await
    }

    pub fn close(&mut self) {
        self.receiver.close()
    }
}

#[derive(Default, Debug)]
pub struct LispInterpreterConfig {
    repl: bool,
}

impl LispInterpreterConfig {
    pub fn new(repl: bool) -> Self {
        Self { repl }
    }
}

#[derive(Debug)]
pub struct LispInterpreter {
    env: LEnv,
    li_channel: LispInterpreterChannel,
    config: LispInterpreterConfig,
}

impl LispInterpreter {
    pub fn set_config(&mut self, config: LispInterpreterConfig) {
        self.config = config;
    }
}

impl LispInterpreter {
    pub fn import_namespace(&mut self, ctx: impl IntoModule) {
        self.env.import(ctx, ImportType::WithoutPrefix)
    }

    pub fn import(&mut self, ctx: impl IntoModule) {
        self.env.import(ctx, ImportType::WithPrefix)
    }

    async fn recv(&mut self) -> Option<String> {
        self.li_channel.recv().await
    }

    pub async fn run(mut self, log: Option<PathBuf>) {
        eval_init(&mut self.env).await;

        let channel_with_log = self.subscribe();

        let handle_log = spawn_log(channel_with_log, log).await;
        let handle_repl = if self.config.repl {
            Some(spawn_repl(self.subscribe()).await)
        } else {
            None
        };

        loop {
            let id_subscriber: usize = self
                .recv()
                .await
                .expect("bug in lisp interpretor")
                .parse()
                .unwrap();
            let str_lvalue = self.recv().await.expect("bug in LI");
            //println!("expr: {}", str_lvalue);

            if str_lvalue == *"exit" {
                task_handler::end_all();
                break;
            }

            match parse(str_lvalue.as_str(), &mut self.env).await {
                Ok(lv) => match eval(&lv, &mut self.env).await {
                    Ok(lv) => {
                        self.li_channel
                            .send(&id_subscriber, lv.format(0))
                            .await
                            .expect("error on channel to stdout");
                    }
                    Err(e) => {
                        self.li_channel
                            .send(&id_subscriber, format!("error: {}", e))
                            .await
                            .expect("error on channel to stdout");
                    }
                },
                Err(e) => {
                    self.li_channel
                        .send(&id_subscriber, format!("error: {}", e))
                        .await
                        .expect("error on channel to stdout");
                }
            };
        }

        handle_log.await.expect("Error on task log");
        if let Some(handle) = handle_repl {
            handle.await.expect("Error on task repl");
        }
    }

    pub fn subscribe(&mut self) -> ChannelToLispInterpreter {
        self.li_channel.get_new_subscriber()
    }
}

impl LispInterpreter {
    pub async fn new() -> Self {
        let env = get_root_env().await;

        Self {
            env,
            li_channel: Default::default(),
            config: Default::default(),
        }
    }
}

///Spawn repl task
pub async fn spawn_repl(communication: ChannelToLispInterpreter) -> JoinHandle<()> {
    let mut end_receiver = subscribe_new_task();
    tokio::spawn(async move {
        tokio::select! {
            _ = repl(communication) => {
            }
            _ = end_receiver.recv() => {
                println!("task_handler killed repl.")
            }
        }
    })
}

/// Spawn the log task
pub async fn spawn_log(com: ChannelToLispInterpreter, log_path: Option<PathBuf>) -> JoinHandle<()> {
    let end_receiver = subscribe_new_task();
    tokio::spawn(async move {
        log(com, log_path, end_receiver).await;
    })
}

async fn log(
    mut com: ChannelToLispInterpreter,
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
            buffer = com.recv() => {
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
                com.close();
                break;
            }
        }
    }
    println!("Draining log queue...");
    while let Some(msg) = com.recv().await {
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
async fn repl(mut com: ChannelToLispInterpreter) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(string) => {
                rl.add_history_entry(string.clone());
                com.send(string).await.expect("couldn't send lisp command");
                let buffer = match com.recv().await {
                    None => {
                        eprintln!("repl task stopped working");
                        break;
                    }
                    Some(b) => b,
                };
                //if buffer != NIL {
                println!("LI>> {}", buffer);
                //}

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
    com.send("exit".to_string())
        .await
        .expect("couldn't send exit msg");
    rl.save_history("history.txt").unwrap();
}

pub const EXIT_CODE_STDOUT: &str = "EXIT";
