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
    pub difficulty: Difficulty,
}

#[derive(Clone, Eq, PartialEq, Hash, Default, Ord)]
pub enum Difficulty {
    #[default]
    Easy,
    Medium,
    Hard,
    Other(String),
}

impl Difficulty {
    fn as_u8(&self) -> u8 {
        match self {
            Difficulty::Easy => 0,
            Difficulty::Medium => 1,
            Difficulty::Hard => 2,
            Difficulty::Other(_) => 3,
        }
    }
}

impl PartialOrd for Difficulty {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Other(s), Self::Other(s2)) => s.partial_cmp(s2),
            (e1, e2) => {
                let e1: u8 = e1.as_u8();
                let e2: u8 = e2.as_u8();
                e1.partial_cmp(&e2)
            }
        }
    }
}

impl From<&str> for Difficulty {
    fn from(value: &str) -> Self {
        match value {
            "easy" | "simple" => Self::Easy,
            "medium" => Self::Medium,
            "difficult" | "hard" => Self::Hard,
            _ => Self::Other(value.to_string()),
        }
    }
}

impl Display for Difficulty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Difficulty::Easy => "Easy",
                Difficulty::Medium => "Medium",
                Difficulty::Hard => "Hard",
                Difficulty::Other(s) => s.as_str(),
            }
        )
    }
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
