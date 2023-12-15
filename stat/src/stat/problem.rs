use crate::stat::config::{ConfigInstanceStat, ConfigName, ConfigProblemStat};
use crate::stat::instance::{InstanceName, InstanceRunData};
use ompas_core::ompas::interface::stat::OMPASRunData;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Clone, Eq, PartialEq, Hash, Default, Ord)]
pub struct ProblemName {
    pub domain: String,
    pub difficulty: String,
}

impl PartialOrd for ProblemName {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self.domain.partial_cmp(&other.domain) {
            None => None,
            Some(ord) => {
                if Ordering::Equal == ord {
                    self.difficulty.partial_cmp(&other.difficulty)
                } else {
                    Some(ord)
                }
            }
        }
    }
}

impl Display for ProblemName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}", self.domain, self.difficulty)
    }
}

#[derive(Default)]
pub struct ProblemRunData {
    pub inner: HashMap<InstanceName, InstanceRunData>,
}

impl ProblemRunData {
    pub fn add_run(
        &mut self,
        instance_name: InstanceName,
        config_name: ConfigName,
        run_stat: OMPASRunData,
    ) {
        match self.inner.entry(instance_name) {
            Entry::Occupied(mut o) => {
                o.get_mut().add_run(config_name, run_stat);
            }
            Entry::Vacant(v) => {
                let mut instance_run_data = InstanceRunData::default();
                instance_run_data.add_run(config_name, run_stat);
                v.insert(instance_run_data);
            }
        }
    }

    pub fn get_problem_stat(&self) -> ProblemStat {
        let mut config_stats: HashMap<ConfigName, Vec<ConfigInstanceStat>> = Default::default();
        for (_, run) in &self.inner {
            for (c, stat) in run.get_stat().inner {
                match config_stats.entry(c) {
                    Entry::Occupied(mut o) => o.get_mut().push(stat),
                    Entry::Vacant(v) => {
                        v.insert(vec![stat]);
                    }
                }
            }
        }

        let mut problems = ProblemStat {
            inner: config_stats
                .drain()
                .map(|(c, stat)| (c, (stat.as_slice()).into()))
                .collect(),
        };

        problems
            .inner
            .iter_mut()
            .fold(
                None,
                |prev: Option<&mut ConfigProblemStat>, (_, stat)| match prev {
                    Some(prev) => {
                        if prev.score.0 > stat.score.0 {
                            Some(prev)
                        } else {
                            Some(stat)
                        }
                    }
                    None => Some(stat),
                },
            )
            .unwrap()
            .score
            .1 = true;

        problems
            .inner
            .iter_mut()
            .fold(
                None,
                |prev: Option<&mut ConfigProblemStat>, (_, stat)| match prev {
                    Some(prev) => {
                        if prev.execution_time.0 > stat.execution_time.0 {
                            Some(prev)
                        } else {
                            Some(stat)
                        }
                    }
                    None => Some(stat),
                },
            )
            .unwrap()
            .execution_time
            .1 = true;

        problems
    }
}

#[derive(Default)]
pub struct ProblemStat {
    pub inner: HashMap<ConfigName, ConfigProblemStat>,
}
