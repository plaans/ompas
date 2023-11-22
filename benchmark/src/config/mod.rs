use crate::config::job_config::JobConfig;
use crate::MailConfig;
use ompas_middleware::LogLevel;
use serde::{Deserialize, Serialize};

pub mod job_config;
pub mod mail_config;

#[derive(Serialize, Deserialize)]
pub struct BenchConfig {
    pub mail: Option<MailConfig>,
    pub jobs: Vec<JobConfig>,
    pub view: bool,
    pub log_level: LogLevel,
}
