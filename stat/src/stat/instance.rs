use crate::stat::config::{ConfigInstanceStat, ConfigName, ConfigRunData};
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub type InstanceName = String;

pub struct InstanceRunStat {
    pub inner: HashMap<ConfigName, ConfigInstanceStat>,
}

pub struct ContinuousPlanningStat {
    pub planning_time: f64,
    pub planning_time_ratio: f64,
    pub number_planning_instance: f64,
    pub average_planning_time: f64,
    pub planning_success_rate: f64,
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

    pub fn get_stat(&self) -> InstanceRunStat {
        let mut config_stat: Vec<_> = self
            .inner
            .iter()
            .map(|(c, run)| (c.clone(), run.get_config_instance_stat()))
            .collect();

        let ((c_score, best_score), (c_exec_time, best_exec_time)) = config_stat
            .iter()
            .fold(None, |prev, (c, run)| match prev {
                None => Some(((c.clone(), run.score), (c.clone(), run.execution_time.mean))),
                Some(((c_score, b_score), (c_exec_time, best_exec_time))) => {
                    let score = if run.score > b_score {
                        (c.clone(), run.score)
                    } else {
                        (c_score, b_score)
                    };

                    let exec_time = if run.execution_time.mean > best_exec_time {
                        (c.clone(), run.execution_time.mean)
                    } else {
                        (c_exec_time, best_exec_time)
                    };

                    Some((score, exec_time))
                }
            })
            .unwrap();

        config_stat.iter_mut().for_each(|(c, run)| {
            if c == &c_score {
                run.best_score_ratio = 1.0;
                run.distance_to_best_score = 0.0;
            } else {
                run.distance_to_best_score = (run.score - best_score) / best_score;
            }
            if c == &c_exec_time {
                run.best_config_execution_time_ratio = 1.0;
                run.distance_to_best_execution_time = 0.0;
            } else {
                run.distance_to_best_execution_time = (run.score - best_exec_time) / best_exec_time;
            }
        });

        InstanceRunStat {
            inner: config_stat.drain(..).collect(),
        }
    }
}
