use chrono::{DateTime, Utc};
use lazy_static::lazy_static;
use log::Level;
use ompas_utils::other::get_and_update_id_counter;
use std::collections::{HashMap, HashSet};
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::mem;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use std::time::SystemTime;
use tokio::sync::{Mutex, RwLock};

const DEFAULT_LOG_DIRECTORY: &str = "/home/jeremy/ompas_logs";
const DEFAULT_MAX_LOG_LEVEL: Level = Level::Info;
pub const END_SIGNAL: EndSignal = EndSignal {};

lazy_static! {
    pub static ref LOGGER: Logger = Logger::new();
}

pub struct EndSignal {}

pub struct LogTopic {
    name: String,
    absolute_path: PathBuf,
    file: File,
    displayer_killer: Option<tokio::sync::oneshot::Sender<EndSignal>>,
}

pub enum FileDescriptor {
    Name(String),
    Directory(PathBuf),
    AbsolutePath(PathBuf),
    RelativePath(PathBuf),
}

pub type TopicId = usize;

pub struct Logger {
    collection: LogTopicCollection,
    absolute_start: DateTime<Utc>,
    system_start: SystemTime,
    current_log_dir: String,
    max_log_level: Arc<RwLock<Level>>,
}

impl Logger {
    pub fn new() -> Logger {
        let start = Utc::now() + chrono::Duration::hours(2);
        let current_log_dir = start.format("%Y-%m-%d_%H-%M-%S").to_string();

        Logger {
            collection: Default::default(),
            absolute_start: start,
            system_start: SystemTime::now(),
            current_log_dir,
            max_log_level: Arc::new(RwLock::new(DEFAULT_MAX_LOG_LEVEL)),
        }
    }

    pub async fn enabled(&self, level: Level) -> bool {
        level <= self.get_max_log_level().await
    }

    pub async fn log(&self, message: LogMessage) {
        if self.enabled(message.level).await {
            for topic in message.topics {
                if let Some(topic) = self.collection.inner.lock().await.get_mut(&topic) {
                    let string = format!(
                        "[{},{}] {}: {}\n",
                        self.system_start.elapsed().unwrap().as_secs_f64(),
                        topic.name,
                        message.level,
                        message.message
                    );

                    topic.file.write_all(string.as_bytes()).expect("");
                } else {
                    todo!()
                }
            }
        }
    }

    pub async fn get_topic_id(&self, topic: &str) -> Option<TopicId> {
        self.collection.topic_id.lock().await.get(topic).cloned()
    }

    pub async fn start_display_log_topic(&self, topic: &TopicId) {
        if let Some(topic) = self.collection.inner.lock().await.get_mut(topic) {
            let (killer, mut killed) = tokio::sync::oneshot::channel();
            topic.displayer_killer = Some(killer);
            let path: PathBuf = topic.absolute_path.clone();
            let topic_name = format!("LOG: {}", topic.name);
            tokio::spawn(async move {
                let child = Command::new("gnome-terminal")
                    .args(&["--title", topic_name.as_str(), "--disable-factory"])
                    .args(&["--", "tail", "-f", path.to_str().unwrap()])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .spawn()
                    .expect("could not spawn terminal");
                killed.try_recv().expect("error on receiver");
                //println!("killing rae log process : {}", child.id());
                Command::new("pkill")
                    .args(["-P", child.id().to_string().as_str()])
                    .spawn()
                    .expect("error on killing process");
            });
        }
    }

    pub async fn stop_display_log_topic(&self, topic: &TopicId) {
        if let Some(topic) = self.collection.inner.lock().await.get_mut(topic) {
            let mut killer: Option<_> = None;
            mem::swap(&mut topic.displayer_killer, &mut killer);
            if let Some(killer) = killer {
                killer.send(END_SIGNAL);
            }
        }
    }

    pub async fn set_max_log_level(&self, level: Level) {
        *self.max_log_level.write().await = level
    }

    pub async fn get_max_log_level(&self) -> Level {
        *self.max_log_level.read().await
    }

    pub async fn subscribe_to_topic(&self, topic_name: String) -> TopicId {
        match self.collection.topic_id.lock().await.get(&topic_name) {
            Some(id) => *id,
            None => self.new_topic(topic_name, None).await,
        }
    }

    pub async fn new_topic(
        &self,
        name: String,
        file_descriptor: Option<FileDescriptor>,
    ) -> TopicId {
        let id = get_and_update_id_counter(self.collection.next_topic_id.clone());

        let path: String = match &file_descriptor {
            None => format!(
                "{}/{}/{}",
                DEFAULT_LOG_DIRECTORY, self.current_log_dir, name
            )
            .into(),
            Some(FileDescriptor::AbsolutePath(ap)) => ap.to_str().unwrap().to_string(),
            Some(FileDescriptor::RelativePath(rp)) => format!(
                "{}/{}/{}",
                DEFAULT_LOG_DIRECTORY,
                self.current_log_dir,
                rp.to_str().unwrap()
            )
            .into(),
            Some(FileDescriptor::Directory(d)) => format!(
                "{}/{}/{}/{}",
                DEFAULT_LOG_DIRECTORY,
                self.current_log_dir,
                d.to_str().unwrap(),
                name
            )
            .into(),
            Some(FileDescriptor::Name(n)) => {
                format!("{}/{}/{}", DEFAULT_LOG_DIRECTORY, self.current_log_dir, n).into()
            }
        };

        let path: PathBuf = path.into();
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(path.clone())
            .unwrap_or_else(|_| panic!("Error creating log file for topic {}", name));
        self.collection.inner.lock().await.insert(
            id,
            LogTopic {
                name: name.to_string(),
                absolute_path: path.into(),
                file,
                displayer_killer: None,
            },
        );
        self.collection.topic_id.lock().await.insert(name, id);
        id
    }
}

#[derive(Default)]
pub struct LogTopicCollection {
    inner: Arc<Mutex<HashMap<TopicId, LogTopic>>>,
    topic_id: Arc<Mutex<HashMap<String, TopicId>>>,
    next_topic_id: Arc<AtomicUsize>,
}

pub struct LogMessage {
    pub level: Level,
    pub source: String,
    pub topics: HashSet<TopicId>,
    pub message: String,
}
