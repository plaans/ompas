use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ActingStat {
    pub n_root_task: u32,
}
