use crate::stat::system::Cell;
use crate::stat::DurationStat;
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Hash, Default)]
pub struct ConfigName {
    pub select_heuristic: String,
    pub continuous_planning_config: String,
    pub other: Vec<String>,
}

impl Display for ConfigName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}_{}",
            self.select_heuristic, self.continuous_planning_config
        )?;
        for o in &self.other {
            write!(f, "_{}", o)?;
        }
        Ok(())
    }
}

pub struct ConfigRunData {
    pub inner: Vec<OMPASRunData>,
}

impl ConfigRunData {
    pub fn add_run(&mut self, run_stat: OMPASRunData) {
        self.inner.push(run_stat);
    }

    pub fn get_config_instance_stat(&self) -> ConfigInstanceStat {
        ConfigInstanceStat {
            execution_time: self.get_execution_time_stat(),
            score: self.get_score(),
            deliberation_ratio: self.get_deliberation_ratio(),
            planning_ratio: self.get_planning_ratio(),
            best_config_ratio: 0.0,
            distance_to_best: 0.0,
            success_rate: self.get_success_rate(),
            number_failures: self.get_n_failures(),
        }
    }

    fn get_execution_time_stat(&self) -> DurationStat {
        let times: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_execution_time())
            .collect();
        let min = times
            .iter()
            .fold(None, |prev, val| match prev {
                None => Some(*val),
                Some(prev) => Some(prev.min(*val)),
            })
            .unwrap();
        let max = times
            .iter()
            .fold(None, |prev, val| match prev {
                None => Some(*val),
                Some(prev) => Some(prev.max(*val)),
            })
            .unwrap();
        let mean = times.iter().sum::<f64>() / times.len() as f64;
        DurationStat { min, max, mean }
    }

    fn get_score(&self) -> f64 {
        self.get_success_rate() * 100.0 / self.get_execution_time_stat().mean
    }

    fn get_deliberation_ratio(&self) -> f64 {
        let ratios: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_deliberation_time_ratio())
            .collect();
        ratios.iter().sum::<f64>() / ratios.len() as f64
    }

    fn get_planning_ratio(&self) -> Option<f64> {
        None
    }

    fn get_success_rate(&self) -> f64 {
        let success: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_success_rate())
            .collect();

        success.iter().sum::<f64>() / success.len() as f64
    }

    fn get_n_failures(&self) -> f64 {
        let failures: Vec<_> = self.inner.iter().map(|run| run.get_n_failures()).collect();
        failures.iter().sum::<u32>() as f64 / failures.len() as f64
    }
}

pub struct ConfigInstanceStat {
    pub execution_time: DurationStat,
    pub score: f64,
    pub deliberation_ratio: f64,
    pub planning_ratio: Option<f64>,
    pub best_config_ratio: f64,
    pub distance_to_best: f64,
    pub success_rate: f64,
    pub number_failures: f64,
}

pub const EXECUTION_TIME: &str = "T_{exec}";
pub const DELIBERATION_TIME_RATIO: &str = "R_{delib}";
pub const SCORE: &str = "score";
pub const PLANNING_TIME_RATIO: &str = "R_{plan}";
pub const VIRTUAL_BEST_RATIO: &str = "R_{VBest}";
pub const DISTANCE_TO_BEST: &str = "D_{VBest}";
pub const SUCCESS_RATE: &str = "P_{Success}";
pub const N_FAILURES: &str = "N_{Fail}";

pub struct ConfigProblemStat {
    pub execution_time: f64,
    pub score: f64,
    pub deliberation_ratio: f64,
    pub planning_ratio: Option<f64>,
    pub virtual_best_ratio: f64,
    pub distance_to_best: f64,
    pub success_rate: f64,
    pub number_failures: f64,
}

impl ConfigProblemStat {
    pub fn header() -> Vec<Cell> {
        vec![
            //Cell::start(EXECUTION_TIME.to_string()),
            //Cell::start(DELIBERATION_TIME_RATIO.to_string()),
            //Cell::start(PLANNING_TIME_RATIO.to_string()),
            Cell::start(SCORE.to_string()),
            Cell::start(VIRTUAL_BEST_RATIO.to_string()),
            Cell::start(DISTANCE_TO_BEST.to_string()),
            // Cell::start(SUCCESS_RATE.to_string()),
            Cell::double(SUCCESS_RATE.to_string()),
            //Cell::double(N_FAILURES.to_string()),
        ]
    }

    pub fn to_formatted(&self) -> Vec<Cell> {
        vec![
            //Cell::start(format!("{:.1}", self.execution_time)),
            //Cell::start(format!("{:.1}", self.deliberation_ratio * 100.0)),
            // Cell::start(match self.planning_ratio {
            //     Some(p) => format!("{:.1}", p * 100.0),
            //     None => "None".to_string(),
            // }),
            Cell::start(format!("{:.1}", self.score)),
            Cell::start(format!("{:.1}", self.virtual_best_ratio * 100.0)),
            Cell::start(format!("{:.1}", self.distance_to_best)),
            //Cell::start(format!("{:.1}", self.success_rate * 100.0)),
            Cell::double(format!("{:.1}", self.success_rate * 100.0)),
            //Cell::double(format!("{:.1}", self.number_failures)),
        ]
    }
}

impl From<&[ConfigInstanceStat]> for ConfigProblemStat {
    fn from(stats: &[ConfigInstanceStat]) -> Self {
        let n = stats.len() as f64;

        Self {
            execution_time: stats
                .iter()
                .map(|stat| stat.execution_time.mean)
                .sum::<f64>()
                / n,
            score: stats.iter().map(|stat| stat.score).sum::<f64>() / n,
            deliberation_ratio: stats
                .iter()
                .map(|stat| stat.deliberation_ratio)
                .sum::<f64>()
                / n,
            planning_ratio: None,
            virtual_best_ratio: stats.iter().map(|stat| stat.best_config_ratio).sum::<f64>() / n,
            distance_to_best: stats.iter().map(|stat| stat.distance_to_best).sum::<f64>() / n,
            success_rate: stats.iter().map(|stat| stat.success_rate).sum::<f64>() / n,
            number_failures: stats.iter().map(|stat| stat.number_failures).sum::<f64>() / n,
        }
    }
}
