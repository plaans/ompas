pub mod config;
pub mod formatter;
pub mod instance;
pub mod problem;
pub mod system;

pub struct RunStat {
    pub config_name: Vec<String>,
    pub execution_time: f64,
    pub deliberation_time: f64,
    pub planning_time: Option<f64>,
    pub best_config_ratio: bool,
    pub distance_to_best: f64,
    pub success: bool,
    pub number_failures: u32,
}

pub struct DurationStat {
    mean: f64,
    _min: f64,
    _max: f64,
}

pub struct RatioStat {
    _mean: f64,
    _min: f64,
    _max: f64,
}
