use crate::ompas::manager::acting::interval::Duration;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct PlannerStat {
    inner: Vec<PlanningInstanceStat>,
}

impl PlannerStat {
    pub fn add_stat(&mut self, stat: PlanningInstanceStat) {
        self.inner.push(stat)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanningInstanceStat {
    pub id: u32,
    pub duration: Duration,
    pub optimal: bool,
    pub interrupted: bool,
}
