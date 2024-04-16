use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize)]
pub struct JobConfig {
    pub domain: DomainConfig,
    pub problem_config: ProblemConfig,
    pub heuristics: Vec<HeuristicConfig>,
    pub timeout: u32,
    pub min_time: Option<u32>,
    pub n_run: u32,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct DomainConfig {
    pub label: String,
    pub binary: Option<BinaryConfig>,
    pub domain_path: PathBuf,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct BinaryConfig {
    pub label: String,
    pub path: PathBuf,
}

impl DomainConfig {
    pub fn get_binary(&self) -> String {
        match &self.binary {
            Some(b) => b.label.to_string(),
            None => "ompas".to_string(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ProblemConfig {
    pub problem_dir_path: PathBuf,
    pub specific_problems: Option<Vec<String>>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct HeuristicConfig {
    pub select: SelectConfig,
    pub deliberation_reactivity: f64,
    pub continuous_planning: ContinuousPlanningConfig,
    pub pre_compute_models: Option<bool>,
    pub specific_domain: Option<SpecificDomain>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct SpecificDomain {
    pub label: String,
    pub domain: PathBuf,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SelectConfig {
    Greedy,
    Cost,
    Random,
    UPOM,
    Aries,
    AriesOpt,
}

#[derive(Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ContinuousPlanningConfig {
    No,
    Satisfactory(f64),
    Optimality(f64),
}
