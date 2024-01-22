use crate::ompas::manager::acting::acting_stat::ActingStat;
use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::interval::Duration;
use crate::ompas::manager::acting::process::process_stat::ActingProcessStat;
use crate::ompas::manager::planning::planner_stat::{PlannerStat, PlanningStatus};
use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct OMPASRunData {
    pub inner: Vec<OMPASStat>,
}

impl OMPASRunData {
    pub fn add_stat(&mut self, stat: impl Into<OMPASStat>) {
        self.inner.push(stat.into());
    }

    pub fn inner(&self) -> &Vec<OMPASStat> {
        &self.inner
    }

    pub fn get_execution_time(&self) -> f64 {
        let acting_time = self.get_acting_time();
        //println!("acting_time:{}", acting_time);
        let mut min_start: Option<f64> = None;
        let mut max_end: Option<f64> = None;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                let start = p.start.as_secs();
                let end = start
                    + match p.duration.is_finite() {
                        true => p.duration.as_secs(),
                        false => acting_time,
                    };
                match min_start {
                    None => {
                        min_start = Some(start);
                    }
                    Some(min) => min_start = Some(min.min(start)),
                }
                match max_end {
                    None => {
                        max_end = Some(end);
                    }
                    Some(max) => max_end = Some(max.max(end)),
                }
            }
        }

        max_end.unwrap() - min_start.unwrap()
    }

    pub fn get_acting_time(&self) -> f64 {
        self.inner
            .iter()
            .find_map(|run| {
                if let OMPASStat::Acting(a) = run {
                    Some(a)
                } else {
                    None
                }
            })
            .unwrap()
            .acting_time
            .as_secs()
    }

    pub fn get_bench_min_time(&self) -> f64 {
        self.inner
            .iter()
            .find_map(|run| {
                if let OMPASStat::Bench(a) = run {
                    Some(a)
                } else {
                    None
                }
            })
            .unwrap()
            .min_time
            .as_secs()
    }

    pub fn get_bench_max_time(&self) -> f64 {
        self.inner
            .iter()
            .find_map(|run| {
                if let OMPASStat::Bench(a) = run {
                    Some(a)
                } else {
                    None
                }
            })
            .unwrap()
            .max_time
            .as_secs()
    }

    pub fn get_deliberation_time(&self) -> f64 {
        let mut deliberation_time = 0.0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                deliberation_time +=
                    p.deliberation_time.mean.as_secs() * p.deliberation_time.instance as f64;
            }
        }
        deliberation_time
    }

    pub fn get_deliberation_time_ratio(&self) -> f64 {
        let mut deliberation_time = 0.0;
        let mut i = 0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                deliberation_time += p.deliberation_time.mean.as_secs()
                    * p.deliberation_time.instance as f64
                    / p.duration.as_secs();
                i += 1;
            }
        }
        deliberation_time / i as f64
    }

    pub fn get_planning_waiting_time(&self) -> f64 {
        let mut planning_waiting_time = 0.0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                let mut pwt = p.planning_waiting_time.mean.as_secs();
                if pwt.is_nan() {
                    pwt = 0.0;
                }
                let instance = p.planning_waiting_time.instance as f64;
                planning_waiting_time += pwt * instance;
            }
        }

        planning_waiting_time
    }

    pub fn get_planning_waiting_time_ratio(&self) -> f64 {
        let mut planning_waiting_time = 0.0;
        let mut i = 0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                let mut pwt = p.planning_waiting_time.mean.as_secs();
                if pwt.is_nan() {
                    pwt = 0.0;
                }
                let instance = p.planning_waiting_time.instance as f64;
                planning_waiting_time += pwt * instance
                    / (p.deliberation_time.mean.as_secs() * (p.deliberation_time.instance as f64));
                i += 1;
            }
        }
        planning_waiting_time / i as f64
    }

    pub fn get_planning_time(&self) -> f64 {
        let mut planning_time = 0.0;
        for stat in &self.inner {
            if let OMPASStat::Planner(p) = stat {
                for stat in &p.inner {
                    planning_time += stat.duration.as_secs();
                }
            }
        }
        planning_time
    }

    pub fn get_planning_time_ratio(&self) -> f64 {
        self.get_planning_time() / self.get_acting_time()
    }

    pub fn get_number_planning_instance(&self) -> f64 {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => 0.0,
            Some(p) => p.inner.len() as f64,
        }
    }

    pub fn get_average_planning_time(&self) -> f64 {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => 0.0,
            Some(p) => self.get_planning_time() / p.inner.len() as f64,
        }
    }

    pub fn get_planning_success_rate(&self) -> f64 {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => 0.0,
            Some(p) => {
                let mut success = 0.0;
                for p_stat in &p.inner {
                    if p_stat.status == PlanningStatus::Sat {
                        success += 1.0;
                    }
                }
                success / p.inner.len() as f64
            }
        }
    }

    pub fn get_average_number_planning_solutions(&self) -> f64 {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => 0.0,
            Some(p) => {
                let mut solutions = 0;
                for p_stat in &p.inner {
                    solutions += p_stat.n_solution;
                }
                solutions as f64 / p.inner.len() as f64
            }
        }
    }

    pub fn get_coverage(&self) -> f64 {
        let mut success = 0;
        let mut i = 0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                if p.status.is_success() {
                    success += 1;
                }
                i += 1;
            }
        }
        success as f64 / i as f64
    }

    pub fn get_n_failures(&self) -> u32 {
        let mut n_failures = 0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                n_failures += p.n_failure;
            }
        }
        n_failures
    }

    pub fn get_n_retries(&self) -> u32 {
        let mut n_retries = 0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                if p.status.is_failed() {
                    n_retries += p.n_retry;
                }
            }
        }
        n_retries
    }

    pub fn get_planning_times(&self) -> Vec<f64> {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => vec![],
            Some(p) => p.inner.iter().map(|run| run.duration.as_secs()).collect(),
        }
    }

    pub fn get_planning_solutions(&self) -> Vec<f64> {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => vec![],
            Some(p) => p.inner.iter().map(|run| run.n_solution as f64).collect(),
        }
    }

    pub fn get_planning_successes(&self) -> Vec<f64> {
        match self.inner.iter().find_map(|stat| {
            if let OMPASStat::Planner(p) = stat {
                Some(p)
            } else {
                None
            }
        }) {
            None => vec![],
            Some(p) => p
                .inner
                .iter()
                .map(|run| {
                    if run.status == PlanningStatus::Sat {
                        1.0
                    } else {
                        0.0
                    }
                })
                .collect(),
        }
    }

    pub fn get_number_process(&self, kind: &ActingProcessKind) -> f64 {
        let mut n_process = 0;
        for stat in &self.inner {
            if let OMPASStat::Process(p) = stat {
                n_process += p.number_subprocesses.get(kind).unwrap_or(&0)
            }
        }

        n_process as f64
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OMPASStat {
    Process(ActingProcessStat),
    Planner(PlannerStat),
    Acting(ActingStat),
    Bench(BenchStat),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BenchStat {
    pub min_time: Duration,
    pub max_time: Duration,
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

impl From<BenchStat> for OMPASStat {
    fn from(value: BenchStat) -> Self {
        Self::Bench(value)
    }
}
