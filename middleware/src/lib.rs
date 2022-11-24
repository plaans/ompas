extern crate core;

use crate::logger::{
    EndSignal, FileDescriptor, LogClient, LogMessage, LogTopicId, Logger, END_SIGNAL,
};
use lazy_static::lazy_static;
use log::Level;
use map_macro::{map, set};
use ompas_utils::other::get_and_update_id_counter;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::ops::Deref;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, Mutex, RwLock};

pub mod logger;
const TOKIO_CHANNEL_SIZE: usize = 100;

lazy_static! {
    static ref MASTER: Master = Master::new();
}

pub const PROCESS_TOPIC_ALL: &str = "__PROCESS_TOPIC_ALL__";
const TOPIC_ALL_ID: usize = 0;
pub const LOG_TOPIC_ROOT: &str = "__LOG_TOPIC_ROOT__";
const MASTER_LABEL: &str = "MASTER";
pub type ProcessId = usize;
pub type ProcessTopidId = usize;

#[derive(Clone)]
pub struct Master {
    processes: Arc<Mutex<HashMap<ProcessId, ProcessDescriptor>>>,
    topic_id: Arc<RwLock<HashMap<ProcessTopicLabel, ProcessTopidId>>>,
    topics: Arc<RwLock<HashMap<ProcessTopidId, ProcessTopic>>>,
    sender_kill: Arc<mpsc::Sender<KillRequest>>,
    sender_death: Arc<RwLock<Option<mpsc::Sender<DeathNotification>>>>,
    next_topic_id: Arc<AtomicUsize>,
    next_process_id: Arc<AtomicUsize>,
    end_receiver: Arc<broadcast::Receiver<EndSignal>>,
    logger: Logger,
}

pub struct ProcessTopic {
    label: ProcessTopicLabel,
    processes: HashSet<ProcessId>,
    childs: HashSet<ProcessTopidId>,
}

impl ProcessTopic {
    pub fn new(label: impl Display) -> Self {
        Self {
            label: label.to_string(),
            processes: Default::default(),
            childs: Default::default(),
        }
    }
}

pub struct ProcessDescriptor {
    label: String,
    sender: tokio::sync::mpsc::Sender<EndSignal>,
}

impl Master {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let (tx_death, rx_death) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        let (tx_end, rx_end) = broadcast::channel(TOKIO_CHANNEL_SIZE);

        let logger = Logger::default();

        let master = Self {
            processes: Arc::new(Default::default()),
            topic_id: Arc::new(RwLock::new(map! {
                PROCESS_TOPIC_ALL.to_string() => TOPIC_ALL_ID,
            })),
            topics: Arc::new(RwLock::new(map! {
                TOPIC_ALL_ID => ProcessTopic {
                    label: PROCESS_TOPIC_ALL.to_string(),
                    processes: set!{},
                    childs: set!{}
                }
            })),
            sender_kill: Arc::new(tx),
            sender_death: Arc::new(RwLock::new(Some(tx_death))),
            next_topic_id: Arc::new(AtomicUsize::new(1)),
            next_process_id: Arc::new(Default::default()),
            end_receiver: Arc::new(rx_end),
            logger,
        };

        let master2 = master.clone();

        tokio::spawn(async move { Master::run_middleware(master2, rx, rx_death, tx_end).await });

        master
    }

    pub async fn end() {
        let mut end = MASTER.end_receiver.resubscribe();
        let _ = end.recv().await;
    }

    async fn remove_process(&self, process_id: ProcessId) {
        let process: ProcessDescriptor = self.processes.lock().await.remove(&process_id).unwrap();
        self.logger
            .log(LogMessage {
                level: Level::Info,
                source: process.label.to_string(),
                topic: MASTER.logger.subscribe_to_topic(LOG_TOPIC_ROOT).await,
                message: format!("Process {} is dead.", process.label),
            })
            .await;
    }

    async fn run_middleware(
        master: Master,
        mut receiver_kill: mpsc::Receiver<KillRequest>,
        mut receiver_death: mpsc::Receiver<DeathNotification>,
        end: broadcast::Sender<EndSignal>,
    ) {
        let log: LogClient = LogClient::new(MASTER_LABEL, LOG_TOPIC_ROOT).await;
        log.info("INITIATE MASTER").await;

        let (tx_end_logger, rx) = mpsc::channel(1);

        master.logger.start(rx).await;

        'main: loop {
            tokio::select! {
                kill_request = receiver_kill.recv() => {
                    if let Some(kill_request) = kill_request {
                        let id = master.topic_id.write().await.remove(&kill_request.topic);
                        if let Some(id) = id {
                            log.info(format!(
                                "{} requested termination of process topic {}.",
                                kill_request.requestor, kill_request.topic,
                            ))
                            .await;

                            let mut topic_ids: Vec<ProcessTopidId> = vec![id];

                            while let Some(id) = topic_ids.pop() {
                                /*
                                KILLING PROCESSES DEPEND ON THE TOPIC
                                 */

                                let mut topics = master.topics.write().await;

                                let topic: &mut ProcessTopic = topics.get_mut(&id).unwrap();
                                for p in topic.processes.drain() {

                                    if let Some(process) =  master.processes.lock().await.get(&p) {

                                        match process.sender.send(END_SIGNAL).await {
                                            Ok(_) => {
                                                log.info(format!(
                                                    "Request termination of process {}{{Requested by: {}; Part of Topic: {}}}.",
                                                    process.label,
                                                    kill_request.requestor,
                                                    kill_request.topic,
                                                ))
                                                    .await;
                                            }
                                            Err(err) => {
                                                log.warn(format!(
                                                    "Error request termination of process {}: {}",
                                                    process.label, err
                                                ))
                                                .await;
                                            }
                                        }
                                    }

                                }

                                /*
                                KILLING PROCESSES OF CHILD TOPICS
                                 */

                                let topic_name = topic.label.to_string();
                                let childs: Vec<ProcessTopidId> = topic.childs.drain().collect();

                                for topic in childs {
                                    let child_name = topics.get(&topic).unwrap().label.clone();
                                    log.info(format!(
                                        "Request termination of process topic {child_name} because it is a child of the process topic {topic_name}",
                                    )).await;
                                    topic_ids.push(topic);
                                }
                            }

                            if id == TOPIC_ALL_ID {
                                break 'main;
                            }
                        }
                    }
                }
                death_notification = receiver_death.recv() => {
                    if let Some(death_notification) = death_notification {
                        master.remove_process(death_notification.process_id).await;
                    }
                }
            }
        }

        *master.sender_death.write().await = None;

        while let Some(death_notification) = receiver_death.recv().await {
            master.remove_process(death_notification.process_id).await;
        }

        log.info("END MASTER").await;
        let _ = tx_end_logger.send(END_SIGNAL).await;
        master.logger.end().await;
        let _ = end.send(END_SIGNAL);
    }

    async fn subscribe_to_topic(&self, process_id: ProcessId, topic: impl Display) {
        let id = self.get_topic_id(&topic).await;
        match id {
            Some(id) => {
                self.topics
                    .write()
                    .await
                    .get_mut(&id)
                    .unwrap()
                    .processes
                    .insert(process_id);
            }
            None => {
                let parent: Option<&str> = None;
                let id = self.new_topic(topic, parent).await;
                self.topics
                    .write()
                    .await
                    .get_mut(&id)
                    .unwrap()
                    .processes
                    .insert(process_id);
            }
        }
    }

    async fn new_topic(&self, name: impl Display, parent: Option<impl Display>) -> ProcessTopidId {
        let id = self.get_topic_id(&name).await;
        match id {
            Some(id) => id,
            None => {
                let log = LogClient::new(MASTER_LABEL, LOG_TOPIC_ROOT).await;
                log.debug(format!("Creating process topic {}", name)).await;
                let id = get_and_update_id_counter(self.next_topic_id.clone());

                let mut topics = self.topics.write().await;
                topics.insert(id, ProcessTopic::new(name.to_string()));
                self.topic_id.write().await.insert(name.to_string(), id);

                match parent {
                    Some(parent) => match self.get_topic_id(&parent).await {
                        Some(parent_id) => {
                            topics.get_mut(&parent_id).unwrap().childs.insert(id);
                        }
                        None => {
                            log.debug(format!("Creating process topic {}", name)).await;
                            let parent_id = get_and_update_id_counter(self.next_topic_id.clone());
                            self.topic_id.write().await.insert(parent.to_string(), id);
                            topics.insert(
                                parent_id,
                                ProcessTopic {
                                    label: parent.to_string(),
                                    processes: Default::default(),
                                    childs: set! {id},
                                },
                            );
                            topics
                                .get_mut(&TOPIC_ALL_ID)
                                .unwrap()
                                .childs
                                .insert(parent_id);
                        }
                    },
                    None => {
                        topics.get_mut(&TOPIC_ALL_ID).unwrap().childs.insert(id);
                    }
                }

                id
            }
        }
    }

    pub async fn get_topic_id(&self, topic: impl Display) -> Option<ProcessTopidId> {
        self.topic_id.read().await.get(&topic.to_string()).copied()
    }

    pub async fn subscribe_new_process(
        &self,
        label: impl Display,
        process_topic: impl Display,
        log_topic: impl Display,
    ) -> ProcessInterface {
        let id = get_and_update_id_counter(self.next_process_id.clone());
        let (tx, rx) = mpsc::channel(32);
        self.processes.lock().await.insert(
            id,
            ProcessDescriptor {
                label: label.to_string(),
                sender: tx,
            },
        );

        self.subscribe_to_topic(id, process_topic).await;

        ProcessInterface {
            label: label.to_string(),
            id,
            sender_kill: self.sender_kill.deref().clone(),
            receiver: rx,
            log: LogClient {
                topic_id: self.logger.subscribe_to_topic(log_topic).await,
                source: label.to_string(),
            },
            sender_death: self.sender_death.read().await.as_ref().unwrap().clone(),
        }
    }

    pub async fn new_log_topic(name: impl Display, file_descriptor: Option<FileDescriptor>) {
        MASTER.logger.new_topic(name, file_descriptor).await;
    }

    pub async fn subscribe_to_log_topic(topic: impl Display) -> LogTopicId {
        MASTER.logger.subscribe_to_topic(topic).await
    }

    pub async fn get_log_topic_id() {}

    pub async fn start_display_log_topic(topic: impl Display) {
        MASTER.logger.start_display_log_topic(topic).await;
    }

    pub async fn stop_display_log_topic(topic: impl Display) {
        MASTER.logger.stop_display_log_topic(topic).await;
    }

    pub async fn set_max_log_level(level: LogLevel) {
        MASTER.logger.set_max_log_level(level.into()).await;
    }
}

pub type ProcessTopicLabel = String;

pub struct ProcessInterface {
    label: String,
    #[allow(dead_code)]
    id: ProcessId,
    sender_kill: mpsc::Sender<KillRequest>,
    sender_death: mpsc::Sender<DeathNotification>,
    receiver: mpsc::Receiver<EndSignal>,
    log: LogClient,
}

#[derive(Clone, Default)]
pub struct KillRequest {
    requestor: String,
    topic: ProcessTopicLabel,
}

pub struct DeathNotification {
    process_id: ProcessId,
}

impl KillRequest {
    pub fn new(requestor: String, topic: ProcessTopicLabel) -> Self {
        Self { requestor, topic }
    }
}

impl ProcessInterface {
    pub async fn new(
        label: impl Display,
        process_topic: impl Display,
        log_topic: impl Display,
    ) -> Self {
        MASTER
            .subscribe_new_process(label, process_topic, log_topic)
            .await
    }

    pub async fn kill(&self, topic: impl Display) {
        self.sender_kill
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

    /*
    LOG FUNCTIONS
     */
    pub async fn log(&self, message: impl Display, level: LogLevel) {
        self.log.log(message, level).await;
    }

    pub async fn log_error(&self, message: impl Display) {
        self.log.error(message).await
    }

    pub async fn log_warn(&self, message: impl Display) {
        self.log.warn(message).await
    }

    pub async fn log_info(&self, message: impl Display) {
        self.log.info(message).await
    }

    pub async fn log_debug(&self, message: impl Display) {
        self.log.debug(message).await
    }

    pub async fn log_trace(&self, message: impl Display) {
        self.log.trace(message).await
    }

    pub fn get_log_client(&self) -> LogClient {
        self.log.clone()
    }

    fn die(&self) {
        self.sender_death
            .try_send(DeathNotification {
                process_id: self.id,
            })
            .unwrap_or_else(|e| panic!("Error sending death notification of {}: {}", self.label, e))
    }
}

impl Drop for ProcessInterface {
    fn drop(&mut self) {
        self.die()
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
