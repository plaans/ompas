use crate::MailConfig;

pub mod job_config;
pub mod mail_config;

use crate::config::job_config::JobConfig;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct BenchConfig {
    pub mail: MailConfig,
    pub jobs: Vec<JobConfig>,
    pub view: bool,
}
