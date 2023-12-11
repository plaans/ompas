use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Serialize, Deserialize)]
pub struct StatosConfig {
    pub configs: Vec<StatConfig>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StatConfig {
    pub input_dir: PathBuf,
    pub csv_output: Option<PathBuf>,
    pub latex_output: Option<PathBuf>,
    pub fields: Vec<Field>,
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum Field {
    ExecutionTime,
    BestExecutionTimeRatio,
    DistanceToBestExecutionTime,
    DeliberationTime,
    DeliberationTimeRatio,
    Coverage,
    Score,
    BestScoreRatio,
    DistanceToBestScore,
    NumberRetries,
    NumberFailures,
    PlanningTime,
    PlanningTimeRatio,
    NumberPlanningInstance,
    AveragePlanningTime,
    PlanningSuccessRate,
}
