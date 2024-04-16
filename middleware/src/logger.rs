use crate::{LogLevel, LOG_TOPIC_ROOT, MASTER, MASTER_LABEL, TOKIO_CHANNEL_SIZE, TOPIC_ALL_ID};
use crate::{LOGS_DIR, OMPAS_WORKING_DIR};
use chrono::{DateTime, Local, Utc};
use log::Level;
use ompas_utils::other::get_and_update_id_counter;
use std::collections::HashMap;
use std::fmt::Display;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::ops::Deref;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use std::{fs, mem};
use tokio::sync::{broadcast, mpsc, RwLock};
use tokio::time::sleep;

const DEFAULT_MAX_LOG_LEVEL: Level = Level::Info;
pub const END_SIGNAL: EndSignal = EndSignal {};
pub const PROCESS_LOGGER: &str = "__PROCESS_LOGGER__";

#[derive(Debug, Copy, Clone)]
pub struct EndSignal {}

pub struct LogTopic {
    label: String,
    absolute_path: PathBuf,
    file: File,
    displayer_killer: Option<mpsc::Sender<EndSignal>>,
}

pub enum FileDescriptor {
    Name(String),
    Directory(PathBuf),
    AbsolutePath(PathBuf),
    RelativePath(PathBuf),
}

pub type LogTopicId = usize;

#[derive(Clone)]
pub struct Logger {
    collection: LogTopicCollection,
    #[allow(unused)]
    absolute_start: DateTime<Local>,
    system_start: SystemTime,
    logs_dir: PathBuf,
    max_log_level: Arc<RwLock<Level>>,
    end_receiver: Arc<broadcast::Receiver<EndSignal>>,
    sender_log: Arc<mpsc::UnboundedSender<LogMessage>>,
}

impl Logger {
    pub fn new(date: DateTime<Local>, mut run_dir: PathBuf) -> (Self, mpsc::Sender<EndSignal>) {
        run_dir.push(LOGS_DIR);

        let (tx, rx) = mpsc::unbounded_channel();
        let (tx_end_logger, rx_end) = broadcast::channel(TOKIO_CHANNEL_SIZE);

        let logger = Self {
            collection: Default::default(),
            absolute_start: date,
            system_start: SystemTime::now(),
            logs_dir: run_dir,
            max_log_level: Arc::new(RwLock::new(DEFAULT_MAX_LOG_LEVEL)),
            end_receiver: Arc::new(rx_end),
            sender_log: Arc::new(tx),
        };

        let logger2 = logger.clone();

        let (tx_end, rx_end) = mpsc::channel(1);

        tokio::spawn(async move { logger2.start(rx_end, rx, tx_end_logger).await });
        (logger, tx_end)
    }

    pub(crate) async fn start(
        self,
        end_signal: mpsc::Receiver<EndSignal>,
        log_receiver: mpsc::UnboundedReceiver<LogMessage>,
        tx_end: broadcast::Sender<EndSignal>,
    ) {
        tokio::spawn(
            async move { Logger::run_logger(self, end_signal, log_receiver, tx_end).await },
        );
    }

    async fn log_to_file(&self, message: LogMessage) {
        if self.enabled(message.level).await {
            let topic = message.topic;
            let mut topics = self.collection.inner.write().await;
            if let Some(topic) = topics.get_mut(&topic) {
                let string = format!(
                    "[{:^.3},{:^16}] {:^6}: {}\n",
                    self.system_start.elapsed().unwrap().as_secs_f64(),
                    message.source,
                    message.level,
                    message.message
                );

                topic.file.write_all(string.as_bytes()).expect("");
            } else {
                drop(topics);
                let topic_id = self.subscribe_to_topic(LOG_TOPIC_ROOT).await;
                let mut topics = self.collection.inner.write().await;
                let topic = topics.get_mut(&topic_id).unwrap();
                let string = format!(
                    "[{:^.3},{:^16}] {:^6}: {}\n",
                    self.system_start.elapsed().unwrap().as_secs_f64(),
                    message.source,
                    Level::Error,
                    message.message
                );
                topic
                    .file
                    .write_all(string.as_bytes())
                    .unwrap_or_else(|_| panic!("Error writing to root log"));
            }
        }
    }

    async fn run_logger(
        logger: Logger,
        mut end_signal: mpsc::Receiver<EndSignal>,
        mut receiver: mpsc::UnboundedReceiver<LogMessage>,
        end: broadcast::Sender<EndSignal>,
    ) {
        'main: loop {
            tokio::select! {
                _ = end_signal.recv() => {
                    logger.log(LogMessage::new(LogLevel::Info, "MASTER_LOGGER", logger.subscribe_to_topic(LOG_TOPIC_ROOT).await, "Ending logger"));
                    break 'main;
                }
                message = receiver.recv() => {
                    if let Some(message) = message {
                        logger.log_to_file(message).await;
                    }
                }
            }
        }
        receiver.close();

        while let Some(message) = receiver.recv().await {
            logger.log_to_file(message).await;
        }

        for topic in logger.collection.inner.write().await.values() {
            //println!("debug topic {}", topic.label);
            if let Some(killer) = &topic.displayer_killer {
                killer
                    .send(END_SIGNAL)
                    .await
                    .unwrap_or_else(|e| panic!("could not kill log window: {e}"));
            }
        }

        let _ = end.send(END_SIGNAL);
    }

    pub(crate) async fn end(&self) {
        let mut end_receiver = self.end_receiver.resubscribe();
        let _ = end_receiver.recv().await;
    }

    async fn enabled(&self, level: Level) -> bool {
        level <= self.get_max_log_level().await
    }

    pub(crate) fn log(&self, message: LogMessage) {
        let sender = self.sender_log.deref().clone();

        let from = message.source.to_string();
        if let Err(e) = sender.send(message) {
            eprintln!("Error sending log from {}\nerror = {}", from, e)
        }
    }

    pub(crate) async fn get_topic_id(&self, topic: impl Display) -> Option<LogTopicId> {
        self.collection
            .topic_id
            .read()
            .await
            .get(&topic.to_string())
            .cloned()
    }

    /*
    PUBLIC Interface
     */
    pub(crate) async fn start_display_log_topic(&self, topic: impl Display) {
        if let Some(topic) = self.get_topic_id(topic).await {
            if let Some(topic) = self.collection.inner.write().await.get_mut(&topic) {
                let (killer, mut killed) = mpsc::channel(1);
                topic.displayer_killer = Some(killer);
                let path: PathBuf = topic.absolute_path.clone();
                let topic_name = topic.label.to_string();
                tokio::spawn(async move {
                    //TODO: Make the terminal opening platform independant
                    let mut command = Command::new("x-terminal-emulator");
                    let pid_file = format!("/tmp/pid_logger_{}", Utc::now().timestamp());
                    command
                        .args(["--title", topic_name.as_str()])
                        .args([
                            "-e",
                            "echo",
                            "$$",
                            ">",
                            pid_file.as_str(),
                            ";",
                            "tail",
                            "-f",
                            path.to_str().unwrap(),
                        ])
                        .stdout(Stdio::null())
                        .stderr(Stdio::null());

                    command.spawn().expect("could not spawn terminal");
                    sleep(Duration::from_millis(1000)).await;
                    let pid = fs::read_to_string(&pid_file)
                        .unwrap_or_else(|e| panic!("pid_file = {}: {}", pid_file, e));
                    //println!("pid = {}", pid);
                    let pid = pid.replace('\n', "");
                    //println!("pid = {}", pid);
                    //let pid: i64 = pid.parse().unwrap();
                    killed.recv().await.expect("error on receiver");
                    //println!("killing rae log process : {}", pid);
                    Command::new("kill")
                        .args(["-9", pid.as_str()])
                        .spawn()
                        .expect("error on killing process");
                });
            }
        }
    }

    pub(crate) async fn stop_display_log_topic(&self, topic: impl Display) {
        if let Some(topic) = self.get_topic_id(topic).await {
            let mut topics = self.collection.inner.write().await;
            if let Some(topic) = topics.get_mut(&topic) {
                let mut killer: Option<_> = None;
                mem::swap(&mut topic.displayer_killer, &mut killer);
                let name = topic.label.to_string();
                drop(topics);
                let topic_id = self.subscribe_to_topic(LOG_TOPIC_ROOT).await;

                if let Some(killer) = killer {
                    match killer.send(END_SIGNAL).await {
                        Ok(_) => {
                            self.log(LogMessage {
                                level: Level::Error,
                                source: PROCESS_LOGGER.to_string(),
                                topic: topic_id,
                                message: format!("Stop display log topic {}.", name),
                            });
                        }
                        Err(err) => self.log(LogMessage {
                            level: Level::Error,
                            source: PROCESS_LOGGER.to_string(),
                            topic: topic_id,
                            message: format!("Error stop display log topic {}: {:?}.", name, err),
                        }),
                    };
                }
            }
        }
    }

    pub(crate) async fn set_max_log_level(&self, level: Level) {
        *self.max_log_level.write().await = level
    }

    pub(crate) async fn get_max_log_level(&self) -> Level {
        *self.max_log_level.read().await
    }

    pub(crate) async fn subscribe_to_topic(&self, topic_name: impl Display) -> LogTopicId {
        let name = topic_name.to_string();
        let id = self.collection.topic_id.read().await.get(&name).cloned();
        if let Some(id) = id {
            id
        } else {
            self.new_topic(name, None).await
        }
    }

    pub(crate) async fn new_topic(
        &self,
        name: impl Display,
        file_descriptor: Option<FileDescriptor>,
    ) -> LogTopicId {
        let id = get_and_update_id_counter(self.collection.next_topic_id.clone());

        let path: PathBuf = match &file_descriptor {
            None => {
                let mut path = self.logs_dir.clone();
                path.push(format!("{}.log", name));
                path
            }
            Some(FileDescriptor::AbsolutePath(ap)) => ap.clone(),
            Some(FileDescriptor::RelativePath(rp)) => {
                let mut path: PathBuf = OMPAS_WORKING_DIR.get_ref().into();
                path.push(rp);
                path
            }
            Some(FileDescriptor::Directory(d)) => {
                let mut path = d.clone();
                path.push(name.to_string());
                path
            }
            Some(FileDescriptor::Name(n)) => {
                let mut path = self.logs_dir.clone();
                path.push(n);
                path
            }
        };

        let mut dir_path: PathBuf = path.clone();
        dir_path.pop();
        fs::create_dir_all(dir_path.clone()).unwrap_or_else(|e| {
            panic!(
                "Error creating log directory for topic {}:\npath ={} \n error = {}",
                name,
                dir_path.to_str().unwrap(),
                e
            )
        });
        let file = OpenOptions::new()
            .read(true)
            .append(true)
            .create(true)
            .open(path.clone())
            .unwrap_or_else(|e| {
                panic!(
                    "Error creating log file for topic {}:\npath ={} \n error = {}",
                    name,
                    path.to_str().unwrap(),
                    e
                )
            });
        self.collection.inner.write().await.insert(
            id,
            LogTopic {
                label: name.to_string(),
                absolute_path: path,
                file,
                displayer_killer: None,
            },
        );
        self.collection
            .topic_id
            .write()
            .await
            .insert(name.to_string(), id);
        id
    }
}

#[derive(Default, Clone)]
pub struct LogTopicCollection {
    inner: Arc<RwLock<HashMap<LogTopicId, LogTopic>>>,
    topic_id: Arc<RwLock<HashMap<String, LogTopicId>>>,
    next_topic_id: Arc<AtomicUsize>,
}

#[derive(Debug)]
pub struct LogMessage {
    pub level: Level,
    pub source: String,
    pub topic: LogTopicId,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogClient {
    pub(crate) topic_id: LogTopicId,
    pub(crate) source: Arc<String>,
}

impl Default for LogClient {
    fn default() -> Self {
        Self {
            topic_id: TOPIC_ALL_ID,
            source: Arc::new(MASTER_LABEL.to_string()),
        }
    }
}

impl LogClient {
    pub async fn new(source: impl Display, topic: impl Display) -> Self {
        let topic_id = MASTER.logger.subscribe_to_topic(topic).await;
        let source = source.to_string();
        Self {
            topic_id,
            source: Arc::new(source),
        }
    }

    pub fn log(&self, message: impl Display, level: LogLevel) {
        MASTER.logger.log(LogMessage {
            level: level.into(),
            source: self.source.to_string(),
            topic: self.topic_id,
            message: message.to_string(),
        });
    }

    pub fn info(&self, message: impl Display) {
        MASTER.logger.log(LogMessage {
            level: Level::Info,
            source: self.source.to_string(),
            topic: self.topic_id,
            message: message.to_string(),
        });
    }
    pub fn warn(&self, message: impl Display) {
        MASTER.logger.log(LogMessage {
            level: Level::Warn,
            source: self.source.to_string(),
            topic: self.topic_id,
            message: message.to_string(),
        });
    }
    pub fn debug(&self, message: impl Display) {
        MASTER.logger.log(LogMessage {
            level: Level::Debug,
            source: self.source.to_string(),
            topic: self.topic_id,
            message: message.to_string(),
        });
    }
    pub fn error(&self, message: impl Display) {
        MASTER.logger.log(LogMessage {
            level: Level::Error,
            source: self.source.to_string(),
            topic: self.topic_id,
            message: message.to_string(),
        });
    }
    pub fn trace(&self, message: impl Display) {
        MASTER.logger.log(LogMessage {
            level: Level::Trace,
            source: self.source.to_string(),
            topic: self.topic_id,
            message: message.to_string(),
        });
    }
}

impl LogMessage {
    pub fn new(
        level: LogLevel,
        source: impl Display,
        topic: LogTopicId,
        message: impl Display,
    ) -> Self {
        Self {
            level: level.into(),
            source: source.to_string(),
            topic,
            message: message.to_string(),
        }
    }
}
