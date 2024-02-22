use crate::output::tabular::Cell;
use crate::stat::planning::{ConfigPlanningStat, PlanningField};
use crate::stat::Field;
use crate::stat::Field::*;
use crate::stat::Stat;
use ompas_core::ompas::interface::stat::{OMPASRunData, OMPASStat};
use ompas_core::ompas::manager::acting::inner::ActingProcessKind;
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
        let default = match self.select_heuristic.as_str() {
            "random" => "R".to_string(),
            "upom" => "U".to_string(),
            "aries" => "A".to_string(),
            "cost" => "C".to_string(),
            "greedy" => "G".to_string(),
            t => t.to_string(),
        };

        let continuous_planning = match self.continuous_planning_config.as_str() {
            "satisfactory" => "Sat",
            "optimality" => "Opt",
            "reactive" => "",
            t => t,
        };
        let mut name = "$\\textit{C}(".to_string();
        write!(name, "{}", default).unwrap();
        if continuous_planning != "" {
            write!(name, "/{}", continuous_planning).unwrap();
        }

        name.push_str(")$");
        name
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
    pub fn add_run(&mut self, mut run_stat: OMPASRunData) {
        run_stat.inner.retain(|stat| {
            if let OMPASStat::Process(p) = stat {
                !p.label.contains("charge")
            } else {
                true
            }
        });
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
        map.insert(EfficiencyScore, self.get_score_stat());
        map.insert(BestScoreRatio, Default::default());
        map.insert(DistanceToBestScore, Default::default());
        map.insert(NumberFailures, self.get_n_failures());
        map.insert(NumberRetries, self.get_n_retries());
        map.insert(PlanningWaitingTime, self.get_planning_waiting_time_stat());
        map.insert(
            PlanningWaitingTimeRatio,
            self.get_planning_waiting_time_ratio_stat(),
        );
        map.insert(
            MaxPlanningWaitingTime,
            self.get_max_planning_waiting_time_stat(),
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
        map.insert(
            OptimalSolutionRatio,
            self.get_planning_optimal_solution_ratio_stat(),
        );

        map.insert(
            NumberCommands,
            self.get_n_process(&ActingProcessKind::Command),
        );
        map.insert(NumberTasks, self.get_n_process(&ActingProcessKind::Task));
        map.insert(
            NumberMethods,
            self.get_n_process(&ActingProcessKind::Method),
        );
        map.insert(
            NumberAcquisitions,
            self.get_n_process(&ActingProcessKind::Acquire),
        );
        map.insert(
            NumberArbitraries,
            self.get_n_process(&ActingProcessKind::Arbitrary),
        );

        ConfigInstanceStat { stat_map: map }
    }

    pub fn get_max_planning_waiting_time_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_max_planning_waiting_time())
            .collect();
        let stat: Stat = times.as_slice().into();
        //println!("stat: {stat:?}");
        stat
    }

    pub fn get_planning_optimal_solution_ratio_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_optimal_solution_ratio())
            .collect();
        Stat::from(times.as_slice()) * Stat::new(100.0)
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

        map.insert(
            PlanningField::PlanningSuccesses,
            self.get_planning_successes_per_instance(),
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

    fn get_planning_successes_per_instance(&self) -> Vec<Stat> {
        let times: Vec<Vec<f64>> = self
            .inner
            .iter()
            .map(|run| run.get_planning_successes())
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

    pub fn get_field_per_instance(&self, field: &Field) -> Vec<f64> {
        self.inner
            .iter()
            .map(|run| match field {
                EfficiencyScore => run.get_score(),
                ExecutionTime => run.get_execution_time(),
                _ => {
                    todo!()
                }
            })
            .collect()
    }

    fn get_score_stat(&self) -> Stat {
        // let stat_1 = self.get_coverage_stat() / Stat::new(100.0);
        // let stat_2: Stat = self.get_bench_max_time_stat();
        // let stat_3 = self.get_execution_time_stat();
        // stat_1 * stat_2 / stat_3
        self.inner
            .iter()
            .map(|run| run.get_score())
            .collect::<Vec<_>>()
            .as_slice()
            .into()
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
        self.get_deliberation_time_stat() * Stat::new(100.0) / self.get_execution_time_stat()
    }

    fn get_planning_waiting_time_stat(&self) -> Stat {
        let times: Vec<f64> = self
            .inner
            .iter()
            .map(|run| run.get_planning_waiting_time())
            .collect();
        let stat: Stat = times.as_slice().into();
        stat
    }

    fn get_planning_waiting_time_ratio_stat(&self) -> Stat {
        self.get_planning_waiting_time_stat() * Stat::new(100.0) / self.get_deliberation_time_stat()
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
        Stat::from(times.as_slice()) * Stat::new(100.0)
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
        Stat::from(srs.as_slice()) * Stat::new(100.0)
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
        Stat::from(coverages.as_slice())
    }

    fn get_n_failures(&self) -> Stat {
        let failures: Vec<_> = self.inner.iter().map(|run| run.get_n_failures()).collect();
        failures.as_slice().into()
    }

    fn get_n_retries(&self) -> Stat {
        let retries: Vec<_> = self.inner.iter().map(|run| run.get_n_retries()).collect();
        retries.as_slice().into()
    }

    fn get_n_process(&self, process_kind: &ActingProcessKind) -> Stat {
        let processes: Vec<_> = self
            .inner
            .iter()
            .map(|run| run.get_number_process(process_kind))
            .collect();
        processes.as_slice().into()
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
            let mut info = field.to_latex();
            let unit = field.unit_short();
            if unit != "" {
                write!(info, "({})", unit).unwrap();
            }
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
    }
}

impl From<&[ConfigInstanceStat]> for ConfigProblemStat {
    fn from(stats: &[ConfigInstanceStat]) -> Self {
        let fields = Field::all();

        let mut config_problem_stat = ConfigProblemStat::default();
        for field in &fields {
            let mut new_stat = Stat::default();
            let mut i = 0;
            for stat in stats {
                if let Some(stat) = stat.stat_map.get(field) {
                    new_stat = new_stat + *stat;
                    i += 1;
                }
            }
            let new_stat = new_stat / Stat::new(i);
            config_problem_stat.stat_map.insert(*field, new_stat);
        }

        config_problem_stat
    }
}
