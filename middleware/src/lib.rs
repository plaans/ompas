extern crate core;

use crate::logger::{
    EndSignal, FileDescriptor, LogClient, LogMessage, LogTopicId, Logger, END_SIGNAL,
};
use chrono::{DateTime, Utc};
use env_param::EnvParam;
use lazy_static::lazy_static;
use log::Level;
use map_macro::{hash_map, hash_set};
use ompas_utils::other::get_and_update_id_counter;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use tokio::sync::{broadcast, mpsc, Mutex, RwLock};

pub mod logger;
const TOKIO_CHANNEL_SIZE: usize = 100;

pub static OMPAS_WORKING_DIR: EnvParam<String> = EnvParam::new("OMPAS_WORKING_DIR", "/tmp");

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
    sender_kill: Arc<mpsc::UnboundedSender<KillRequest>>,
    sender_death: Arc<RwLock<Option<mpsc::UnboundedSender<DeathNotification>>>>,
    next_topic_id: Arc<AtomicUsize>,
    next_process_id: Arc<AtomicUsize>,
    end_receiver: Arc<broadcast::Receiver<EndSignal>>,
    logger: Logger,
    date: DateTime<Utc>,
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
impl Default for Master {
    fn default() -> Self {
        Self::new()
    }
}

impl Master {
    fn new() -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        let (tx_death, rx_death) = mpsc::unbounded_channel();
        let (tx_end, rx_end) = broadcast::channel(TOKIO_CHANNEL_SIZE);
        let date = Utc::now() + chrono::Duration::hours(2);

        let (logger, tx_end_logger) = Logger::new(date);

        let master = Self {
            processes: Arc::new(Default::default()),
            topic_id: Arc::new(RwLock::new(hash_map! {
                PROCESS_TOPIC_ALL.to_string() => TOPIC_ALL_ID,
            })),
            topics: Arc::new(RwLock::new(hash_map! {
                TOPIC_ALL_ID => ProcessTopic {
                    label: PROCESS_TOPIC_ALL.to_string(),
                    processes: hash_set!{},
                    childs: hash_set!{}
                }
            })),
            sender_kill: Arc::new(tx),
            sender_death: Arc::new(RwLock::new(Some(tx_death))),
            next_topic_id: Arc::new(AtomicUsize::new(1)),
            next_process_id: Arc::new(Default::default()),
            end_receiver: Arc::new(rx_end),
            logger,
            date,
        };

        let master2 = master.clone();

        tokio::spawn(async move {
            Master::run_middleware(master2, rx, rx_death, tx_end, tx_end_logger).await
        });

        master
    }

    pub fn end() {
        MASTER
            .sender_kill
            .send(KillRequest::end())
            .unwrap_or_else(|_| panic!("Error while sending end message"));
    }

    pub async fn wait_end() {
        let mut end = MASTER.end_receiver.resubscribe();
        let _ = end.recv().await;
    }

    async fn remove_process(&self, process_id: ProcessId) {
        let process: ProcessDescriptor = self.processes.lock().await.remove(&process_id).unwrap();
        self.logger.log(LogMessage {
            level: Level::Info,
            source: process.label.to_string(),
            topic: MASTER.logger.subscribe_to_topic(LOG_TOPIC_ROOT).await,
            message: format!("Process {} is dead.", process.label),
        });
    }

    async fn run_middleware(
        master: Master,
        mut receiver_kill: mpsc::UnboundedReceiver<KillRequest>,
        mut receiver_death: mpsc::UnboundedReceiver<DeathNotification>,
        tx_end_master: broadcast::Sender<EndSignal>,
        tx_end_logger: mpsc::Sender<EndSignal>,
    ) {
        let log: LogClient = LogClient::new(MASTER_LABEL, LOG_TOPIC_ROOT).await;
        log.info("INITIATE MASTER");

        'main: loop {
            tokio::select! {
                kill_request = receiver_kill.recv() => {
                    if let Some(kill_request) = kill_request {
                        let id = master.topic_id.write().await.remove(&kill_request.topic);
                        if let Some(id) = id {
                            log.info(format!(
                                "{} requested termination of process topic {}.",
                                kill_request.requestor, kill_request.topic,
                            ));

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
                                                    ;
                                            }
                                            Err(err) => {
                                                log.warn(format!(
                                                    "Error request termination of process {}: {}",
                                                    process.label, err
                                                ))
                                                ;
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
                                    ));
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

        log.info("END MASTER");
        let _ = tx_end_logger.send(END_SIGNAL).await;
        master.logger.end().await;
        let _ = tx_end_master.send(END_SIGNAL);
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

    async fn set_parent(&self, child: impl Display, parent: impl Display) {
        let p: Option<String> = None;
        let id_child = self.new_topic(child.to_string(), p.clone()).await;

        let id_parent = self.new_topic(parent.to_string(), p).await;

        let log = LogClient::new(MASTER_LABEL, LOG_TOPIC_ROOT).await;
        log.debug(format!("Process {} is child of {}", child, parent));
        self.topics
            .write()
            .await
            .get_mut(&id_parent)
            .unwrap()
            .childs
            .insert(id_child);
    }

    async fn new_topic(&self, name: impl Display, parent: Option<impl Display>) -> ProcessTopidId {
        let id = self.get_topic_id(&name).await;
        match id {
            Some(id) => id,
            None => {
                let log = LogClient::new(MASTER_LABEL, LOG_TOPIC_ROOT).await;
                log.debug(format!("Creating process topic {}", name));
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
                            log.debug(format!("Creating process topic {}", name));
                            let parent_id = get_and_update_id_counter(self.next_topic_id.clone());
                            self.topic_id.write().await.insert(parent.to_string(), id);
                            topics.insert(
                                parent_id,
                                ProcessTopic {
                                    label: parent.to_string(),
                                    processes: Default::default(),
                                    childs: hash_set! {id},
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

    async fn format_hierarchy(&self) -> String {
        let topics = self.topics.read().await;
        let processes = self.processes.lock().await;
        let mut str = "PROCESS HIERARCHY".to_string();
        let n_topic: usize = topics.len();
        for id in 0..n_topic {
            let topic: &ProcessTopic = topics.get(&id).unwrap();
            write!(str, "{}: {{processes = {{", topic.label).unwrap();
            for (i, id) in topic.processes.iter().enumerate() {
                if i != 0 {
                    str.push_str(", ");
                }
                write!(str, "{}", processes.get(id).unwrap().label).unwrap();
            }
            write!(str, "}}, childs = {{").unwrap();
            for (i, id) in topic.childs.iter().enumerate() {
                if i != 0 {
                    str.push_str(", ");
                }
                write!(str, "{}", topics.get(id).unwrap().label).unwrap();
            }
            writeln!(str, "}}}}").unwrap();
        }
        str
    }

    pub async fn get_topic_id(&self, topic: impl Display) -> Option<ProcessTopidId> {
        self.topic_id.read().await.get(&topic.to_string()).copied()
    }

    async fn subscribe_new_process(
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

        self.subscribe_to_topic(id, process_topic.to_string()).await;
        /*let log = LogClient::new(MASTER_LABEL, LOG_TOPIC_ROOT).await;
        log.debug(format!(
            "Creating process {} of topic {}",
            label, process_topic
        ))
        .await;*/
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

    pub async fn set_child_process(child: impl Display, parent: impl Display) {
        MASTER.set_parent(child, parent).await;
    }

    pub async fn format_process_hierarchy() -> String {
        MASTER.format_hierarchy().await
    }

    pub async fn new_log_topic(name: impl Display, file_descriptor: Option<FileDescriptor>) {
        MASTER.logger.new_topic(name, file_descriptor).await;
    }

    pub async fn subscribe_to_log_topic(topic: impl Display) -> LogTopicId {
        MASTER.logger.subscribe_to_topic(topic).await
    }

    //pub async fn get_log_topic_id() {}

    pub async fn start_display_log_topic(topic: impl Display) {
        MASTER.logger.start_display_log_topic(topic).await;
    }

    pub async fn stop_display_log_topic(topic: impl Display) {
        MASTER.logger.stop_display_log_topic(topic).await;
    }

    pub async fn set_log_level(level: LogLevel) {
        MASTER.logger.set_max_log_level(level.into()).await;
    }

    pub async fn get_log_level() -> LogLevel {
        MASTER.logger.get_max_log_level().await.into()
    }

    pub fn get_string_date() -> String {
        MASTER.date.format("%Y-%m-%d_%H-%M-%S").to_string()
    }

    pub fn reinit() {
        lazy_static::initialize(&MASTER)
    }
}

pub type ProcessTopicLabel = String;

pub struct ProcessInterface {
    label: String,
    #[allow(dead_code)]
    id: ProcessId,
    sender_kill: mpsc::UnboundedSender<KillRequest>,
    sender_death: mpsc::UnboundedSender<DeathNotification>,
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

    pub fn end() -> Self {
        Self {
            requestor: "".to_string(),
            topic: PROCESS_TOPIC_ALL.to_string(),
        }
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

    pub fn kill(&self, topic: impl Display) {
        self.sender_kill
            .send(KillRequest::new(self.label.to_string(), topic.to_string()))
            .unwrap_or_else(|e| {
                panic!(
                    "Error sending kill signal for topic {} requested by {}: {}",
                    topic, self.label, e
                )
            });
    }
    pub async fn recv(&mut self) -> Option<EndSignal> {
        self.receiver.recv().await
    }

    /*
    LOG FUNCTIONS
     */
    pub fn log(&self, message: impl Display, level: LogLevel) {
        self.log.log(message, level);
    }

    pub fn log_error(&self, message: impl Display) {
        self.log.error(message)
    }

    pub fn log_warn(&self, message: impl Display) {
        self.log.warn(message)
    }

    pub fn log_info(&self, message: impl Display) {
        self.log.info(message)
    }

    pub fn log_debug(&self, message: impl Display) {
        self.log.debug(message)
    }

    pub fn log_trace(&self, message: impl Display) {
        self.log.trace(message)
    }

    pub fn get_log_client(&self) -> LogClient {
        self.log.clone()
    }

    fn die(&self) {
        self.sender_death
            .send(DeathNotification {
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

#[derive(Copy, Clone, Debug)]
pub enum LogLevel {
    Error = 1,
    Warn,
    Info,
    Debug,
    Trace,
}

impl Display for LogLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LogLevel::Error => "error",
                LogLevel::Warn => "warn",
                LogLevel::Info => "info",
                LogLevel::Debug => "debug",
                LogLevel::Trace => "trace",
            }
        )
    }
}

impl TryFrom<&str> for LogLevel {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, <LogLevel as TryFrom<&str>>::Error> {
        match value {
            "error" => Ok(Self::Error),
            "warn" => Ok(Self::Warn),
            "info" => Ok(Self::Info),
            "debug" => Ok(Self::Debug),
            "trace" => Ok(Self::Trace),
            _ => Err(()),
        }
    }
}

impl From<Level> for LogLevel {
    fn from(l: Level) -> Self {
        match l {
            Level::Error => Self::Error,
            Level::Warn => Self::Warn,
            Level::Info => Self::Info,
            Level::Debug => Self::Debug,
            Level::Trace => Self::Trace,
        }
    }
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
