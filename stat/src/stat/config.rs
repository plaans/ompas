use crate::stat::formatter::Cell;
use crate::stat::instance::ContinuousPlanningStat;
use crate::stat::Stat;
use crate::statos_config::Field;
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Hash, Default)]
pub struct ConfigName {
    pub select_heuristic: String,
    pub continuous_planning_config: String,
    pub other: Vec<String>,
}

impl ConfigName {
    pub fn format(&self) -> String {
        let mut default = match self.select_heuristic.as_str() {
            "random" => "R".to_string(),
            "upom" => "U".to_string(),
            "aries" => "A".to_string(),
            t => t.to_string(),
        };
        for o in &self.other {
            write!(default, "+{}", o).unwrap();
        }
        let continuous_planning = match self.continuous_planning_config.as_str() {
            "satisfactory" => "Sat",
            "optimality" => "Opt",
            "reactive" => "",
            t => t,
        };
        format!("$OMPAS_{{{}}}^{{{}}}$", continuous_planning, default)
    }
}

impl Display for ConfigName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.select_heuristic,)?;
        for o in &self.other {
            write!(f, "_{}", o)?;
        }
        write!(f, "_{}", self.continuous_planning_config)
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
            bench_min_time: self.get_bench_min_time(),
            bench_max_time: self.get_bench_max_time(),
            execution_time: self.get_execution_time_stat(),
            best_config_execution_time_ratio: Default::default(),
            distance_to_best_execution_time: Default::default(),
            score: self.get_score_stat(),
            deliberation_time: self.get_deliberation_time_stat(),
            deliberation_ratio: self.get_deliberation_time_ratio_stat(),
            best_score_ratio: Default::default(),
            distance_to_best_score: Default::default(),
            number_failures: self.get_n_failures(),
            number_retries: self.get_n_retries(),
            continuous_planning_stat: self.get_continuous_planning_stat(),
            coverage: self.get_coverage_stat(),
        }
    }

    fn get_bench_min_time(&self) -> f64 {
        let times: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_bench_min_time())
            .collect();
        times.iter().sum::<f64>() / times.len() as f64
    }

    fn get_bench_max_time(&self) -> f64 {
        let times: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_bench_min_time() + run.get_bench_max_time())
            .collect();
        times.iter().sum::<f64>() / times.len() as f64
    }

    fn get_execution_time_stat(&self) -> Stat {
        let times: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_execution_time())
            .collect();
        times.as_slice().into()
    }

    fn get_score_stat(&self) -> Stat {
        let stat_1 = self.get_coverage_stat();
        let stat_2: Stat = Stat::new(self.get_bench_min_time());
        let stat_3 = self.get_execution_time_stat();
        stat_1 * stat_2 / stat_3
    }

    fn get_deliberation_time_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_deliberation_time())
            .collect();
        times.as_slice().into()
    }

    fn get_deliberation_time_ratio_stat(&self) -> Stat {
        let ratios: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_deliberation_time_ratio())
            .collect();
        ratios.as_slice().into()
    }

    fn get_continuous_planning_stat(&self) -> Option<ContinuousPlanningStat> {
        None
    }

    fn get_coverage_stat(&self) -> Stat {
        let coverages: Vec<f64> = self.inner.iter().map(|run| run.get_coverage()).collect();
        coverages.as_slice().into()
    }

    fn get_n_failures(&self) -> Stat {
        let failures: Vec<_> = self.inner.iter().map(|run| run.get_n_failures()).collect();
        failures.as_slice().into()
    }

    fn get_n_retries(&self) -> Stat {
        let retries: Vec<_> = self.inner.iter().map(|run| run.get_n_retries()).collect();
        retries.as_slice().into()
    }
}

pub struct ConfigInstanceStat {
    pub bench_min_time: f64,
    pub bench_max_time: f64,
    pub execution_time: Stat,
    pub best_config_execution_time_ratio: Stat,
    pub distance_to_best_execution_time: Stat,
    pub deliberation_time: Stat,
    pub deliberation_ratio: Stat,
    pub coverage: Stat,
    pub score: Stat,
    pub best_score_ratio: Stat,
    pub distance_to_best_score: Stat,
    pub number_failures: Stat,
    pub number_retries: Stat,
    pub continuous_planning_stat: Option<ContinuousPlanningStat>,
}

pub const EXECUTION_TIME: &str = "$T$";
pub const DISTANCE_TO_BEST_EXECUTION_TIME: &str = "$D_{BT}$";
pub const BEST_EXECUTION_TIME_RATIO: &str = "$R_{BT}$";
pub const DELIBERATION_TIME: &str = "$T_D$";
pub const DELIBERATION_TIME_RATIO: &str = "$R_D$";
pub const COVERAGE: &str = "SR";
pub const SCORE: &str = "ES";
pub const NUMBER_RETRIES: &str = "$N_R$";
pub const NUMBER_FAILURES: &str = "$N_F$";
pub const DISTANCE_TO_BEST_SCORE: &str = "$D_{BES}$";
pub const BEST_SCORE_RATIO: &str = "$R_{BES}$";
pub const PLANNING_TIME: &str = "$T_P$";
pub const PLANNING_TIME_RATIO: &str = "$R_P$";
pub const NUMBER_PLANNING_INSTANCE: &str = "$N_{PI}$";
pub const AVERAGE_PLANNING_TIME: &str = "$E_{\\Pi T}$";
pub const PLANNING_SUCCESS_RATE: &str = "$R(\\PI SR)$";

pub struct ConfigProblemStat {
    pub execution_time: (f64, bool),
    pub best_execution_time_ratio: f64,
    pub distance_to_best_execution_time: f64,
    pub deliberation_time: f64,
    pub deliberation_ratio: f64,
    pub score: (f64, bool),
    pub best_score_ratio: f64,
    pub distance_to_best_score: f64,
    pub coverage: f64,
    pub number_failures: f64,
    pub number_retries: f64,
    pub continuous_planning_stat: Option<ContinuousPlanningStat>,
}

impl ConfigProblemStat {
    pub fn header(fields: &[Field]) -> Vec<Cell> {
        let mut cells = vec![];
        let last = fields.len() - 1;
        for (i, field) in fields.iter().enumerate() {
            let info = match field {
                Field::ExecutionTime => EXECUTION_TIME,
                Field::DistanceToBestExecutionTime => DISTANCE_TO_BEST_EXECUTION_TIME,
                Field::DeliberationTime => DELIBERATION_TIME,
                Field::DeliberationTimeRatio => DELIBERATION_TIME_RATIO,
                Field::Coverage => COVERAGE,
                Field::Score => SCORE,
                Field::DistanceToBestScore => DISTANCE_TO_BEST_SCORE,
                Field::NumberRetries => NUMBER_RETRIES,
                Field::NumberFailures => NUMBER_FAILURES,
                Field::PlanningTime => PLANNING_TIME,
                Field::PlanningTimeRatio => PLANNING_TIME_RATIO,
                Field::NumberPlanningInstance => NUMBER_PLANNING_INSTANCE,
                Field::AveragePlanningTime => AVERAGE_PLANNING_TIME,
                Field::PlanningSuccessRate => PLANNING_SUCCESS_RATE,
                Field::BestExecutionTimeRatio => BEST_EXECUTION_TIME_RATIO,
                Field::BestScoreRatio => BEST_SCORE_RATIO,
            }
            .to_string();
            if i == last {
                cells.push(Cell::double(info))
            } else {
                cells.push(Cell::start(info))
            }
        }

        cells
    }

    pub fn to_formatted(&self, fields: &[Field]) -> Vec<Cell> {
        let mut cells = vec![];
        let last = fields.len() - 1;
        for (i, field) in fields.iter().enumerate() {
            let info = match field {
                Field::ExecutionTime => {
                    if self.execution_time.1 {
                        format!("\\textbf{{{:.1}}}", self.execution_time.0)
                    } else {
                        format!("{:.1}", self.execution_time.0)
                    }
                }
                Field::DistanceToBestExecutionTime => {
                    todo!()
                }
                Field::DeliberationTime => {
                    format!("{:.1}", self.deliberation_time)
                }
                Field::DeliberationTimeRatio => {
                    format!("{:.1}", self.deliberation_ratio * 100.0)
                }
                Field::Coverage => {
                    format!("{:.1}", self.coverage * 100.0)
                }
                Field::Score => {
                    if self.score.1 {
                        format!("\\textbf{{{:.3}}}", self.score.0)
                    } else {
                        format!("{:.3}", self.score.0)
                    }
                }
                Field::DistanceToBestScore => {
                    format!("{:.0}\\%", self.distance_to_best_score * 100.0)
                }
                Field::NumberRetries => {
                    format!("{:.1}", self.number_retries)
                }
                Field::NumberFailures => {
                    format!("{:.1}", self.number_failures)
                }
                Field::PlanningTime => match &self.continuous_planning_stat {
                    None => "ND".to_string(),
                    Some(c) => format!("{:.1}", c.planning_time.mean),
                },
                Field::PlanningTimeRatio => match &self.continuous_planning_stat {
                    None => "ND".to_string(),
                    Some(c) => format!("{:.1}", c.planning_time_ratio.mean * 100.0),
                },
                Field::NumberPlanningInstance => match &self.continuous_planning_stat {
                    None => "ND".to_string(),
                    Some(c) => format!("{:.1}", c.planning_time_ratio.mean),
                },
                Field::AveragePlanningTime => match &self.continuous_planning_stat {
                    None => "ND".to_string(),
                    Some(c) => format!("{:.1}", c.average_planning_time.mean),
                },
                Field::PlanningSuccessRate => match &self.continuous_planning_stat {
                    None => "ND".to_string(),
                    Some(c) => format!("{:.1}", c.planning_success_rate.mean * 100.0),
                },
                Field::BestExecutionTimeRatio => {
                    format!("{:.1}", self.best_execution_time_ratio * 100.0)
                }
                Field::BestScoreRatio => {
                    format!("{:.1}", self.best_score_ratio * 100.0)
                }
            };

            if i == last {
                cells.push(Cell::double(info))
            } else {
                cells.push(Cell::start(info))
            }
        }
        cells
        /*vec![
            //Cell::start(format!("{:.1}", self.execution_time)),
            //Cell::start(format!("{:.1}", self.deliberation_ratio * 100.0)),
            // Cell::start(match self.planning_ratio {
            //     Some(p) => format!("{:.1}", p * 100.0),
            //     None => "None".to_string(),
            // }),
            Cell::start(format!("{:.1}", self.score)),
            Cell::start(format!("{:.1}", self.virtual_best_ratio),
                        Cell::start(format!("{:.1}", self.distance_to_best)),
                        //Cell::start(format!("{:.1}", self.success_rate * 100.0)),
                        //Cell::double(format!("{:.1}", self.number_failures)),
        ]*/
    }
}

impl From<&[ConfigInstanceStat]> for ConfigProblemStat {
    fn from(stats: &[ConfigInstanceStat]) -> Self {
        let n = stats.len() as f64;

        let stat = Self {
            execution_time: (
                stats
                    .iter()
                    .map(|stat| stat.execution_time.mean)
                    .sum::<f64>()
                    / n,
                false,
            ),
            best_execution_time_ratio: stats
                .iter()
                .map(|stat| stat.best_config_execution_time_ratio)
                .sum::<f64>()
                / n,
            distance_to_best_execution_time: stats
                .iter()
                .map(|stat| stat.distance_to_best_execution_time)
                .sum::<f64>()
                / n,
            score: (stats.iter().map(|stat| stat.score).sum::<f64>() / n, false),
            best_score_ratio: stats.iter().map(|stat| stat.best_score_ratio).sum::<f64>() / n,
            deliberation_ratio: stats
                .iter()
                .map(|stat| stat.deliberation_ratio)
                .sum::<f64>()
                / n,
            coverage: stats.iter().map(|stat| stat.coverage).sum::<f64>() / n,
            number_failures: stats.iter().map(|stat| stat.number_failures).sum::<f64>() / n,
            number_retries: stats.iter().map(|stat| stat.number_retries).sum::<f64>() / n,
            continuous_planning_stat: None, //stats.iter().map(|stat| stat.number_retries).sum::<f64>() / n,
            deliberation_time: stats.iter().map(|stat| stat.deliberation_time).sum::<f64>() / n,
            distance_to_best_score: stats
                .iter()
                .map(|stat| stat.distance_to_best_score)
                .sum::<f64>()
                / n,
        };

        stat
    }
}
