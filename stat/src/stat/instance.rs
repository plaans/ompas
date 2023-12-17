use crate::stat::config::{ConfigInstanceStat, ConfigName, ConfigRunData};
use crate::stat::planning::ConfigPlanningStat;
use crate::stat::Field;
use crate::stat::Stat;
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub type InstanceName = String;

pub struct InstanceRunStat {
    pub inner: HashMap<ConfigName, ConfigInstanceStat>,
}

pub struct InstancePlanningStat {
    pub inner: HashMap<ConfigName, ConfigPlanningStat>,
}

#[derive(Default)]
pub struct InstanceRunData {
    pub inner: HashMap<ConfigName, ConfigRunData>,
}

impl InstanceRunData {
    pub fn add_run(&mut self, config_name: ConfigName, run_stat: OMPASRunData) {
        match self.inner.entry(config_name) {
            Entry::Occupied(mut o) => {
                o.get_mut().add_run(run_stat);
            }
            Entry::Vacant(v) => {
                let config_run_data = ConfigRunData {
                    inner: vec![run_stat],
                };
                v.insert(config_run_data);
            }
        }
    }

    pub fn get_planning_stat(&self) -> InstancePlanningStat {
        let mut config_stat: Vec<_> = self
            .inner
            .iter()
            .map(|(c, run)| (c.clone(), run.get_config_instance_planning_stat()))
            .collect();

        InstancePlanningStat {
            inner: config_stat.drain(..).collect(),
        }
    }

    pub fn get_stat(&self) -> InstanceRunStat {
        let mut config_stat: Vec<_> = self
            .inner
            .iter()
            .map(|(c, run)| (c.clone(), run.get_config_instance_stat()))
            .collect();

        let ((c_score, best_score), (c_exec_time, best_exec_time)) = config_stat
            .iter()
            .fold(None, |prev, (c, run)| match prev {
                None => Some((
                    (c.clone(), *run.get(&Field::Score).unwrap()),
                    (c.clone(), *run.get(&Field::ExecutionTime).unwrap()),
                )),
                Some(((c_score, b_score), (c_exec_time, best_exec_time))) => {
                    let run_score = run.get(&Field::Score).unwrap();
                    let score = if run_score.mean > b_score.mean {
                        (c.clone(), *run_score)
                    } else {
                        (c_score, b_score)
                    };

                    let run_exec_time = run.get(&Field::Score).unwrap();
                    let exec_time = if run_exec_time.mean > best_exec_time.mean {
                        (c.clone(), *run_exec_time)
                    } else {
                        (c_exec_time, best_exec_time)
                    };

                    Some((score, exec_time))
                }
            })
            .unwrap();

        config_stat.iter_mut().for_each(|(c, run)| {
            if c == &c_score {
                *run.get_mut(&Field::BestScoreRatio).unwrap() = Stat::new(1.0);
                *run.get_mut(&Field::DistanceToBestScore).unwrap() = Stat::new(0.0);
            } else {
                let score = *run.get(&Field::Score).unwrap();
                *run.get_mut(&Field::DistanceToBestScore).unwrap() =
                    (score - best_score).abs() / best_score;
            }
            if c == &c_exec_time {
                *run.get_mut(&Field::BestExecutionTimeRatio).unwrap() = Stat::new(1.0);
                *run.get_mut(&Field::DistanceToBestExecutionTime).unwrap() = Stat::new(0.0);
            } else {
                let exec_time = *run.get(&Field::Score).unwrap();
                *run.get_mut(&Field::DistanceToBestExecutionTime).unwrap() =
                    (exec_time - best_exec_time).abs() / best_exec_time;
            }
        });

        InstanceRunStat {
            inner: config_stat.drain(..).collect(),
        }
    }
}
