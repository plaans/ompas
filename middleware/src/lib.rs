use crate::ompas_log::{EndSignal, LogMessage, Logger, TopicId, END_SIGNAL};
use lazy_static::lazy_static;
use log::Level;
use map_macro::{map, set};
use ompas_utils::other::get_and_update_id_counter;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, Mutex, RwLock};

pub mod ompas_log;
const TOKIO_CHANNEL_SIZE: usize = 100;

lazy_static! {
    static ref MASTER: Master = Master::new();
}

pub const PROCESS_TOPIC_ALL: &str = "__PROCESS_TOPIC_ALL__";
const TOPIC_ALL_ID: usize = 0;
const ROOT: usize = 0;
const LOG_TOPIC_ROOT: &str = "__LOG_TOPIC_ROOT__";
const MASTER_LABEL: &str = "master";
pub type ProcessId = usize;

#[derive(Clone)]
pub struct Master {
    processes: Arc<Mutex<HashMap<ProcessId, ProcessDescriptor>>>,
    topic_id: Arc<RwLock<HashMap<ProcessTopic, TopicId>>>,
    topic_process: Arc<Mutex<HashMap<TopicId, Vec<ProcessId>>>>,
    sender: Arc<RwLock<tokio::sync::mpsc::Sender<KillRequest>>>,
    next_topic_id: Arc<AtomicUsize>,
    next_process_id: Arc<AtomicUsize>,
    end_receiver: Arc<broadcast::Receiver<EndSignal>>,
}

pub struct ProcessDescriptor {
    label: String,
    sender: tokio::sync::mpsc::Sender<EndSignal>,
}

impl Master {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let (tx_end, rx_end) = broadcast::channel(TOKIO_CHANNEL_SIZE);

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
            end_receiver: Arc::new(rx_end),
        };

        let master2 = master.clone();

        tokio::spawn(async move { Master::run_middleware(master2, rx, tx_end).await });
        master
    }

    pub async fn end() {
        let mut end = MASTER.end_receiver.resubscribe();
        let _ = end.recv().await;
    }

    async fn run_middleware(
        master: Master,
        mut receiver: tokio::sync::mpsc::Receiver<KillRequest>,
        end: broadcast::Sender<EndSignal>,
    ) {
        let topic_id = Logger::subscribe_to_topic(LOG_TOPIC_ROOT).await;
        Logger::log(LogMessage::new(
            LogLevel::Info,
            MASTER_LABEL,
            topic_id,
            "INITIATE MASTER",
        ))
        .await;

        loop {
            if let Some(kill_request) = receiver.recv().await {
                let id = master.topic_id.write().await.remove(&kill_request.topic);
                if let Some(id) = id {
                    Logger::log(LogMessage {
                        level: Level::Warn,
                        source: ROOT.to_string(),
                        topic: topic_id,
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
                                        Logger::log(LogMessage {
                                                level: Level::Warn,
                                                source: ROOT.to_string(),
                                                topic: topic_id,
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
                                        Logger::log(LogMessage {
                                            level: Level::Warn,
                                            source: ROOT.to_string(),
                                            topic: topic_id,
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
        Logger::log(LogMessage::new(
            LogLevel::Info,
            MASTER_LABEL,
            topic_id,
            "END MASTER",
        ))
        .await;
        let _ = end.send(END_SIGNAL);
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
                let topic_root = Logger::subscribe_to_topic(LOG_TOPIC_ROOT).await;
                Logger::log(LogMessage::new(
                    LogLevel::Debug,
                    MASTER_LABEL,
                    topic_root,
                    format!("Creating proces topic {}", topic),
                ))
                .await;
                let id = get_and_update_id_counter(self.next_topic_id.clone());
                self.topic_process.lock().await.insert(id, vec![process_id]);
                self.topic_id.write().await.insert(topic, id);
            }
        }
    }

    #[allow(dead_code)]
    async fn new_topic(&self, name: ProcessTopic) -> TopicId {
        let topic_root = Logger::subscribe_to_topic(LOG_TOPIC_ROOT).await;
        Logger::log(LogMessage::new(
            LogLevel::Debug,
            MASTER_LABEL,
            topic_root,
            format!("Creating proces topic {}", name),
        ))
        .await;
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
    log_topic: TopicId,
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
            .unwrap_or_else(|e| {
                panic!(
                    "Error sending kill signal for topic {} requested by {}: {}",
                    topic.to_string(),
                    self.label,
                    e
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
        self.log_topic = Logger::subscribe_to_topic(topic).await;
    }

    pub async fn log(&self, message: impl Display, level: LogLevel) {
        Logger::log(LogMessage {
            level: level.into(),
            source: self.label.to_string(),
            topic: self.log_topic.clone(),
            message: message.to_string(),
        })
        .await;
    }

    pub async fn die(self) {
        let topic_root = Logger::subscribe_to_topic(LOG_TOPIC_ROOT).await;
        Logger::log(LogMessage {
            level: Level::Debug,
            source: self.label.to_string(),
            topic: topic_root,
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
