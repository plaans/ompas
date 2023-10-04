use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::interval::Duration;
use crate::ompas::manager::acting::ActingProcessId;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct ActingProcessStat {
    pub id: ActingProcessId,
    pub kind: ActingProcessKind,
    pub label: String,
    pub execution_time: Duration,
    pub deliberation_time: Duration,
    pub number_subprocesses: HashMap<ActingProcessKind, u32>,
}
