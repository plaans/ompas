use crate::planning::planner::ompas_lcp::PMetric;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Copy, Clone, Serialize, Deserialize)]
pub enum ContinuousPlanningMode {
    #[default]
    None,
    Satisfactory,
    Optimality(PMetric),
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum ContinuousPlanningModeSerde {
    None,
    Satisfactory,
    Optimality,
}

impl From<ContinuousPlanningMode> for ContinuousPlanningModeSerde {
    fn from(value: ContinuousPlanningMode) -> Self {
        match value {
            ContinuousPlanningMode::None => Self::None,
            ContinuousPlanningMode::Satisfactory => Self::Satisfactory,
            ContinuousPlanningMode::Optimality(_) => Self::Optimality,
        }
    }
}
