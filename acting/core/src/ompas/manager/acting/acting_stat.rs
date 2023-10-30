use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ActingStat {
    pub n_root_task: u32,
    pub n_command_task: u32,
}
