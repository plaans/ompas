use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::interval::{Duration, Timepoint};
use crate::ompas::manager::acting::ActingProcessId;
use crate::ompas::manager::state::action_status::ProcessStatus;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct ActingProcessStat {
    pub id: ActingProcessId,
    pub kind: ActingProcessKind,
    pub label: String,
    pub status: ProcessStatus,
    pub start: Timepoint,
    pub duration: Duration,
    pub deliberation_time: Duration,
    pub n_refinement: u32,
    pub n_failure: u32,
    pub n_retry: u32,
    pub number_subprocesses: HashMap<ActingProcessKind, u32>,
}
