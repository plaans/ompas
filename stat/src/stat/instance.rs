use crate::stat::config::{ConfigInstanceStat, ConfigName, ConfigRunData};
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub type InstanceName = String;

pub struct InstanceRunStat {
    pub inner: HashMap<ConfigName, ConfigInstanceStat>,
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

        // let best = config_stat
        //     .iter()
        //     .fold(None, |prev, (c, run)| match prev {
        //         None => Some((c.clone(), run.execution_time.mean)),
        //         Some(d) => {
        //             if d.1 > run.execution_time.mean {
        //                 Some((c.clone(), run.execution_time.mean))
        //             } else {
        //                 Some(d)
        //             }
        //         }
        //     })
        //     .unwrap();
        //
        // config_stat.iter_mut().for_each(|(c, run)| {
        //     if c == &best.0 {
        //         run.best_config_ratio = 1.0
        //     }
        //     run.distance_to_best = (run.execution_time.mean - best.1) / best.1
        // });

        let best = config_stat
            .iter()
            .fold(None, |prev, (c, run)| match prev {
                None => Some((c.clone(), run.score)),
                Some(d) => {
                    if d.1 > run.score {
                        Some((c.clone(), run.score))
                    } else {
                        Some(d)
                    }
                }
            })
            .unwrap();

        config_stat.iter_mut().for_each(|(c, run)| {
            if c == &best.0 {
                run.best_config_ratio = 1.0
            }
            run.distance_to_best = (run.score - best.1) / best.1
        });

        InstanceRunStat {
            inner: config_stat.drain(..).collect(),
        }
    }
}
