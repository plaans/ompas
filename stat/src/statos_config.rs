use crate::output::bar::Bar;
use crate::output::plot::Plot;
use crate::output::tabular::Tabular;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Serialize, Deserialize)]
pub struct StatosConfig {
    pub configs: Vec<StatConfig>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StatConfig {
    pub input_dirs: Vec<InputDir>,
    pub outputs: Vec<Tabular>,
    pub output_bars: Vec<Bar>,
    pub output_plots: Vec<Plot>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InputDir {
    pub path: PathBuf,
    pub escapes: Option<Vec<String>>,
}
