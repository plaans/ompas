use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Debug, Serialize, Deserialize)]
pub struct StatosConfig {
    pub configs: Vec<StatConfig>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StatConfig {
    pub input_dir: PathBuf,
    pub output_file: PathBuf,
}
