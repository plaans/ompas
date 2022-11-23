use crate::ompas_log::{EndSignal, LogMessage, TopicId, END_SIGNAL, LOGGER};
use lazy_static::lazy_static;
use log::Level;
use map_macro::{map, set};
use ompas_utils::other::get_and_update_id_counter;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex, RwLock};

pub mod ompas_log;
const TOKIO_CHANNEL_SIZE: usize = 100;

lazy_static! {
    pub static ref MASTER: Master = Master::new();
}

pub const PROCESS_TOPIC_ALL: &str = "__PROCESS_TOPIC_ALL__";
const TOPIC_ALL_ID: usize = 0;
const ROOT: usize = 0;
const LOG_TOPIC_ROOT: &str = "__LOG_TOPIC_ROOT__";
pub type ProcessId = usize;

#[derive(Clone)]
pub struct Master {
    processes: Arc<Mutex<HashMap<ProcessId, ProcessDescriptor>>>,
    topic_id: Arc<RwLock<HashMap<ProcessTopic, TopicId>>>,
    topic_process: Arc<Mutex<HashMap<TopicId, Vec<ProcessId>>>>,
    sender: Arc<RwLock<tokio::sync::mpsc::Sender<KillRequest>>>,
    next_topic_id: Arc<AtomicUsize>,
    next_process_id: Arc<AtomicUsize>,
}

pub struct ProcessDescriptor {
    label: String,
    sender: tokio::sync::mpsc::Sender<EndSignal>,
}

impl Master {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        let master = Self {
            processes: Arc::new(Default::default()),
            topic_id: Arc::new(RwLock::new(map! {
                PROCESS_TOPIC_ALL.to_string() => TOPIC_ALL_ID,
            })),
            topic_process: Arc::new(Mutex::new(map! {
                TOPIC_ALL_ID => vec![],
            })),
            sender: Arc::new(RwLock::new(tx)),
            next_topic_id: Arc::new(AtomicUsize::new(1)),
            next_process_id: Arc::new(Default::default()),
        };

        let master2 = master.clone();

        tokio::spawn(async move { Master::run_middleware(master2, rx) });
        master
    }

    async fn run_middleware(
        master: Master,
        mut receiver: tokio::sync::mpsc::Receiver<KillRequest>,
    ) {
        let topic_id = LOGGER.subscribe_to_topic(LOG_TOPIC_ROOT).await;

        loop {
            if let Some(kill_request) = receiver.recv().await {
                let id = master.topic_id.write().await.remove(&kill_request.topic);
                if let Some(id) = id {
                    LOGGER
                        .log(LogMessage {
                            level: Level::Warn,
                            source: ROOT.to_string(),
                            topics: set! {topic_id},
                            message: format!(
                                "{} requested termination of process topic {}.",
                                kill_request.requestor, kill_request.topic,
                            ),
                        })
                        .await;
                    let processes = master.topic_process.lock().await.remove(&id);
                    if let Some(processes) = processes {
                        for p in processes {
                            let process = master.processes.lock().await.remove(&p);
                            if let Some(process) = process {
                                match process.sender.send(END_SIGNAL).await {
                                    Ok(_) => {
                                        LOGGER
                                            .log(LogMessage {
                                                level: Level::Warn,
                                                source: ROOT.to_string(),
                                                topics: set! {topic_id},
                                                message: format!(
                                                    "Request termination of process {}{{Requested by: {}; Part of Topic: {}}}.",
                                                    process.label,
                                                    kill_request.requestor,
                                                    kill_request.topic,
                                                ),
                                            })
                                            .await;
                                    }
                                    Err(err) => {
                                        LOGGER
                                            .log(LogMessage {
                                                level: Level::Warn,
                                                source: ROOT.to_string(),
                                                topics: set! {topic_id},
                                                message: format!(
                                                    "Error request termination of process {}: {}",
                                                    process.label, err
                                                ),
                                            })
                                            .await;
                                    }
                                }
                            }
                        }
                    }
                    if id == TOPIC_ALL_ID {
                        break;
                    }
                }
            }
        }
    }

    async fn subscribe_to_topic(&self, process_id: ProcessId, topic: ProcessTopic) {
        let id = self.topic_id.read().await.get(&topic).cloned();
        match id {
            Some(id) => {
                self.topic_process
                    .lock()
                    .await
                    .get_mut(&id)
                    .unwrap()
                    .push(process_id);
            }
            None => {
                let id = get_and_update_id_counter(self.next_topic_id.clone());
                self.topic_process.lock().await.insert(id, vec![process_id]);
                self.topic_id.write().await.insert(topic, id);
            }
        }
    }

    #[allow(dead_code)]
    async fn new_topic(&self, name: ProcessTopic) -> TopicId {
        let id = get_and_update_id_counter(self.next_topic_id.clone());
        self.topic_process.lock().await.insert(id, vec![]);
        self.topic_id.write().await.insert(name, id);
        id
    }

    pub async fn subscribe_new_process(
        &self,
        label: String,
        mut topics: HashSet<ProcessTopic>,
    ) -> ProcessInterface {
        let id = ompas_utils::other::get_and_update_id_counter(self.next_process_id.clone());
        let (tx, rx) = mpsc::channel(32);
        self.processes.lock().await.insert(
            id,
            ProcessDescriptor {
                label: label.to_string(),
                sender: tx,
            },
        );

        topics.insert(PROCESS_TOPIC_ALL.to_string());
        for topic in topics {
            self.subscribe_to_topic(id, topic).await;
        }

        ProcessInterface {
            label,
            id,
            sender: self.sender.read().await.clone(),
            receiver: rx,
            log_topic: Default::default(),
        }
    }
}

pub type ProcessTopic = String;

pub struct ProcessInterface {
    label: String,
    id: ProcessId,
    sender: mpsc::Sender<KillRequest>,
    receiver: mpsc::Receiver<EndSignal>,
    log_topic: HashSet<TopicId>,
}

#[derive(Clone, Default)]
pub struct KillRequest {
    requestor: String,
    topic: ProcessTopic,
}

impl KillRequest {
    pub fn new(requestor: String, topic: ProcessTopic) -> Self {
        Self { requestor, topic }
    }
}

impl ProcessInterface {
    pub async fn new(label: impl Display, mut topics: HashSet<impl Display>) -> Self {
        MASTER
            .subscribe_new_process(
                label.to_string(),
                topics.drain().map(|e| e.to_string()).collect(),
            )
            .await
    }

    pub async fn kill(&self, topic: impl Display) {
        self.sender
            .send(KillRequest::new(self.label.to_string(), topic.to_string()))
            .await
            .unwrap_or_else(|_| {
                panic!(
                    "Error sending kill signal for topic {} requested by {}",
                    topic.to_string(),
                    self.label
                )
            });
    }
    pub async fn recv(&mut self) -> Option<EndSignal> {
        self.receiver.recv().await
    }
    pub async fn subscribe_to_process_topic(&self, topic: ProcessTopic) {
        MASTER.subscribe_to_topic(self.id, topic).await;
    }

    pub async fn subscribe_to_log_topic(&mut self, topic: impl Display) {
        self.log_topic
            .insert(LOGGER.subscribe_to_topic(topic.to_string()).await);
    }

    pub async fn log(&self, message: impl Display, level: LogLevel) {
        LOGGER
            .log(LogMessage {
                level: level.into(),
                source: self.label.to_string(),
                topics: self.log_topic.clone(),
                message: message.to_string(),
            })
            .await;
    }

    pub async fn die(self) {
        let topic_root = LOGGER.subscribe_to_topic(LOG_TOPIC_ROOT).await;
        LOGGER
            .log(LogMessage {
                level: Level::Debug,
                source: self.label.to_string(),
                topics: set! {topic_root},
                message: format!("Process {} is dead.", self.label),
            })
            .await;
    }
}

pub enum LogLevel {
    Error = 1,
    Warn,
    Info,
    Debug,
    Trace,
}

impl From<LogLevel> for Level {
    fn from(l: LogLevel) -> Self {
        match l {
            LogLevel::Error => Self::Error,
            LogLevel::Warn => Self::Warn,
            LogLevel::Info => Self::Info,
            LogLevel::Debug => Self::Debug,
            LogLevel::Trace => Self::Trace,
        }
    }
}
