use crate::ompas::manager::acting::acting_stat::ActingStat;
use crate::ompas::manager::acting::process::process_stat::ActingProcessStat;
use crate::ompas::manager::planning::planner_stat::PlannerStat;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct OMPASRunStat {
    inner: Vec<OMPASStat>,
}

impl OMPASRunStat {
    pub fn new() -> Self {
        Self { inner: vec![] }
    }

    pub fn add_stat(&mut self, stat: impl Into<OMPASStat>) {
        self.inner.push(stat.into());
    }

    pub fn inner(&self) -> &Vec<OMPASStat> {
        &self.inner
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OMPASStat {
    Process(ActingProcessStat),
    Planner(PlannerStat),
    Acting(ActingStat),
}

impl From<ActingProcessStat> for OMPASStat {
    fn from(value: ActingProcessStat) -> Self {
        Self::Process(value)
    }
}

impl From<PlannerStat> for OMPASStat {
    fn from(value: PlannerStat) -> Self {
        Self::Planner(value)
    }
}

impl From<ActingStat> for OMPASStat {
    fn from(value: ActingStat) -> Self {
        Self::Acting(value)
    }
}
