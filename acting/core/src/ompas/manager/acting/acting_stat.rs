use crate::ompas::interface::continuous_planning_mode::ContinuousPlanningModeSerde;
use crate::ompas::interface::select_mode::SelectModeSerde;
use crate::ompas::manager::acting::interval::Duration;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ActingStat {
    pub n_root_task: u32,
    pub n_command_task: u32,
    pub acting_time: Duration,
    pub select_mode: SelectModeSerde,
    pub continuous_planning_mode: ContinuousPlanningModeSerde,
    pub deliberation_reactivity: f64,
    pub planner_reactivity: f64,
}
