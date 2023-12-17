use crate::output::tabular::Cell;
use crate::stat::planning::{ConfigPlanningStat, PlanningField};
use crate::stat::Field;
use crate::stat::Field::*;
use crate::stat::Stat;
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::collections::HashMap;
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
        let mut map = HashMap::default();
        map.insert(BenchMinTime, self.get_bench_min_time_stat());
        map.insert(BenchMaxTime, self.get_bench_max_time_stat());
        map.insert(ExecutionTime, self.get_execution_time_stat());
        map.insert(BestExecutionTimeRatio, Default::default());
        map.insert(DistanceToBestExecutionTime, Default::default());
        map.insert(DeliberationTime, self.get_deliberation_time_stat());
        map.insert(
            DeliberationTimeRatio,
            self.get_deliberation_time_ratio_stat(),
        );
        map.insert(Coverage, self.get_coverage_stat());
        map.insert(Score, self.get_score_stat());
        map.insert(BestScoreRatio, Default::default());
        map.insert(DistanceToBestScore, Default::default());
        map.insert(NumberFailures, self.get_n_failures());
        map.insert(NumberRetries, self.get_n_retries());
        map.insert(PlanningWaitingTime, self.get_planning_waiting_time_stat());
        map.insert(
            PlanningWaitingTimeRatio,
            self.get_planning_waiting_time_ratio_stat(),
        );
        map.insert(PlanningTime, self.get_planning_time_stat());
        map.insert(PlanningTimeRatio, self.get_planning_time_ratio_stat());
        map.insert(
            NumberPlanningInstance,
            self.get_number_planning_instance_stat(),
        );
        map.insert(AveragePlanningTime, self.get_average_planning_time_stat());
        map.insert(PlanningSuccessRate, self.get_planning_success_rate_stat());
        map.insert(
            AveragePlanningSolutions,
            self.get_average_number_planning_solutions_stat(),
        );

        ConfigInstanceStat { stat_map: map }
    }

    pub fn get_config_instance_planning_stat(&self) -> ConfigPlanningStat {
        let mut map = HashMap::default();
        map.insert(
            PlanningField::PlanningTimes,
            self.get_planning_time_per_instance(),
        );
        map.insert(
            PlanningField::PlanningSolutions,
            self.get_planning_solution_per_instance(),
        );
        ConfigPlanningStat { stat_map: map }
    }

    fn get_planning_time_per_instance(&self) -> Vec<Stat> {
        let times: Vec<Vec<f64>> = self
            .inner
            .iter()
            .map(|run| run.get_planning_times())
            .collect();
        let mut instances: Vec<Vec<f64>> = vec![];
        for run in &times {
            for (i, time) in run.iter().enumerate() {
                if instances.len() > i {
                    instances[i].push(*time);
                } else {
                    instances.push(vec![*time]);
                }
            }
        }
        instances
            .drain(..)
            .map(|instance| instance.as_slice().into())
            .collect()
    }

    fn get_planning_solution_per_instance(&self) -> Vec<Stat> {
        let times: Vec<Vec<f64>> = self
            .inner
            .iter()
            .map(|run| run.get_planning_solutions())
            .collect();
        let mut instances: Vec<Vec<f64>> = vec![];
        for run in &times {
            for (i, time) in run.iter().enumerate() {
                if instances.len() > i {
                    instances[i].push(*time);
                } else {
                    instances.push(vec![*time]);
                }
            }
        }
        instances
            .drain(..)
            .map(|instance| instance.as_slice().into())
            .collect()
    }

    fn get_bench_min_time_stat(&self) -> Stat {
        let times: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_bench_min_time())
            .collect();
        times.as_slice().into()
    }

    fn get_bench_max_time_stat(&self) -> Stat {
        let times: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_bench_min_time() + run.get_bench_max_time())
            .collect();
        times.as_slice().into()
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
        let stat_2: Stat = self.get_bench_min_time_stat();
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

    fn get_planning_waiting_time_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_waiting_time())
            .collect();
        times.as_slice().into()
    }

    fn get_planning_waiting_time_ratio_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_waiting_time_ratio())
            .collect();
        times.as_slice().into()
    }

    fn get_planning_time_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_time())
            .collect();
        times.as_slice().into()
    }

    fn get_planning_time_ratio_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_time_ratio())
            .collect();
        times.as_slice().into()
    }

    fn get_number_planning_instance_stat(&self) -> Stat {
        let pis: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_number_planning_instance())
            .collect();
        pis.as_slice().into()
    }

    fn get_average_planning_time_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_average_planning_time())
            .collect();
        times.as_slice().into()
    }

    fn get_planning_success_rate_stat(&self) -> Stat {
        let srs: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_success_rate())
            .collect();
        srs.as_slice().into()
    }

    fn get_average_number_planning_solutions_stat(&self) -> Stat {
        let srs: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_average_number_planning_solutions())
            .collect();
        srs.as_slice().into()
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
    pub stat_map: HashMap<Field, Stat>,
}

impl ConfigInstanceStat {
    pub fn get(&self, field: &Field) -> Option<&Stat> {
        self.stat_map.get(field)
    }

    pub fn get_mut(&mut self, field: &Field) -> Option<&mut Stat> {
        self.stat_map.get_mut(field)
    }
}

#[derive(Default)]
pub struct ConfigProblemStat {
    pub stat_map: HashMap<Field, Stat>,
}

impl ConfigProblemStat {
    pub fn get(&self, field: &Field) -> Option<&Stat> {
        self.stat_map.get(field)
    }

    pub fn get_mut(&mut self, field: &Field) -> Option<&mut Stat> {
        self.stat_map.get_mut(field)
    }
}

impl ConfigProblemStat {
    pub fn header(fields: &[Field]) -> Vec<Cell> {
        let mut cells = vec![];
        let last = fields.len() - 1;
        for (i, field) in fields.iter().enumerate() {
            let info = field.to_latex();
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
            let info = match self.stat_map.get(field) {
                Some(stat) => {
                    if stat.best {
                        format!("\\textbf{{{:.1}}}", stat.mean)
                    } else {
                        format!("{:.1}", stat.mean)
                    }
                }
                None => "ND".to_string(),
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
        let fields = Field::all();

        let mut config_problem_stat = ConfigProblemStat::default();
        for field in &fields {
            let mut vec = vec![];
            for stat in stats {
                if let Some(stat) = stat.stat_map.get(field) {
                    vec.push(*stat);
                }
            }
            let stat: Stat = vec.drain(..).fold(Stat::new(0.0), |prev, now| prev + now)
                / Stat::new(vec.len() as f64);

            config_problem_stat.stat_map.insert(*field, stat);
        }

        // let stat = Self {
        //     execution_time: (
        //         stats
        //             .iter()
        //             .map(|stat| stat.execution_time.mean)
        //             .sum::<f64>()
        //             / n,
        //         false,
        //     ),
        //     best_execution_time_ratio: stats
        //         .iter()
        //         .map(|stat| stat.best_config_execution_time_ratio)
        //         .sum::<f64>()
        //         / n,
        //     distance_to_best_execution_time: stats
        //         .iter()
        //         .map(|stat| stat.distance_to_best_execution_time)
        //         .sum::<f64>()
        //         / n,
        //     score: (stats.iter().map(|stat| stat.score).sum::<f64>() / n, false),
        //     best_score_ratio: stats.iter().map(|stat| stat.best_score_ratio).sum::<f64>() / n,
        //     deliberation_ratio: stats
        //         .iter()
        //         .map(|stat| stat.deliberation_ratio)
        //         .sum::<f64>()
        //         / n,
        //     coverage: stats.iter().map(|stat| stat.coverage).sum::<f64>() / n,
        //     number_failures: stats.iter().map(|stat| stat.number_failures).sum::<f64>() / n,
        //     number_retries: stats.iter().map(|stat| stat.number_retries).sum::<f64>() / n,
        //     continuous_planning_stat: None, //stats.iter().map(|stat| stat.number_retries).sum::<f64>() / n,
        //     deliberation_time: stats.iter().map(|stat| stat.deliberation_time).sum::<f64>() / n,
        //     distance_to_best_score: stats
        //         .iter()
        //         .map(|stat| stat.distance_to_best_score)
        //         .sum::<f64>()
        //         / n,
        // };

        config_problem_stat
    }
}
