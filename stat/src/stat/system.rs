use crate::stat::config::ConfigName;
use crate::stat::problem::{ProblemName, ProblemRunData, ProblemStat};
use crate::statos_config::StatConfig;
use ompas_core::ompas::interface::stat::OMPASRunData;
use ompas_language::output::{JSON_FORMAT, YAML_FORMAT};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

pub type RunName = Vec<String>;

pub struct SystemStat {
    pub problem_stats: HashMap<ProblemName, ProblemStat>,
    pub config: StatConfig,
}

pub struct SystemRunData {
    inner: HashMap<ProblemName, ProblemRunData>,
    config: StatConfig,
}

impl SystemRunData {
    pub fn new(config: &StatConfig) -> Self {
        let mut collection = Self {
            inner: Default::default(),
            config: config.clone(),
        };

        for input_dir in &config.input_dirs {
            let dir = &input_dir.path;
            let json_files = Self::get_all_files_in_dir(dir, JSON_FORMAT);
            //println!("Found {} json files...", json_files.len());

            let yaml_files = Self::get_all_files_in_dir(dir, YAML_FORMAT);
            //println!("Found {} yaml files...", yaml_files.len());

            'loop_file: for file in json_files {
                if let Some(escapes) = &input_dir.escapes {
                    for escape in escapes {
                        if format!("{}", file.display()).contains(escape) {
                            println!("escape file: {}", file.display());
                            continue 'loop_file;
                        }
                    }
                }
                collection.read_file(&file, JSON_FORMAT)
            }

            'loop_file: for file in yaml_files {
                if let Some(escapes) = &input_dir.escapes {
                    for escape in escapes {
                        if format!("{}", file.display()).contains(escape) {
                            println!("escape file: {}", file.display());
                            continue 'loop_file;
                        }
                    }
                }
                collection.read_file(&file, YAML_FORMAT)
            }
        }

        // println!(
        //     "Loaded {} stat file in {:.3} ms",
        //     collection.get_number_of_run(),
        //     now.elapsed().unwrap().as_secs_f64() * 1000.0
        // );

        collection
    }

    fn read_file(&mut self, file: &Path, format: &str) {
        let run_name = file
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .replace(format!(".{}", format).as_str(), "")
            .split("_")
            .map(|s| s.to_string())
            .collect();
        let content = match fs::read_to_string(&file) {
            Ok(s) => s,
            Err(_) => {
                return;
            }
        };
        let stat: OMPASRunData = match format {
            YAML_FORMAT => match serde_yaml::from_str(&content) {
                Ok(s) => s,
                Err(_) => {
                    return;
                }
            },
            JSON_FORMAT => match serde_json::from_str(&content) {
                Ok(s) => s,
                Err(_) => {
                    return;
                }
            },
            _ => unreachable!(),
        };

        //println!("Adding stat of file {}", file.display());
        self.add_stat(run_name, stat);
    }

    fn get_all_files_in_dir(path: &Path, pat: &str) -> Vec<PathBuf> {
        let mut files = vec![];

        let mut queue = vec![path.to_path_buf()];

        while let Some(path) = queue.pop() {
            let path = path.canonicalize().unwrap();
            let dir = fs::read_dir(path).unwrap();

            for entry in dir {
                let entry = entry.unwrap();

                let path = entry.path();
                let str = path.display().to_string();
                if path.is_dir() {
                    queue.push(path);
                } else if str.contains(pat) && path.is_file() {
                    files.push(path);
                }
            }
        }

        files
    }

    pub fn get_number_of_run(&self) -> usize {
        self.inner
            .values()
            .map(|p| {
                p.inner
                    .values()
                    .map(|i| i.inner.values().map(|c| c.inner.len()).sum::<usize>())
                    .sum::<usize>()
            })
            .sum::<usize>()
    }

    pub fn add_stat(&mut self, file_name: RunName, run_stat: OMPASRunData) {
        let problem_name = ProblemName {
            domain: file_name[0].to_string(),
            difficulty: file_name[1].as_str().into(),
        };

        let instance_name = file_name[2].to_string();

        let len_file_name = file_name.len();
        let last = file_name.last().unwrap();
        let config_name = ConfigName {
            select_heuristic: file_name[3].to_string(),
            continuous_planning_config: last.to_string(),
            other: file_name[4..len_file_name - 1].to_vec(),
        };
        let entry = self.inner.entry(problem_name);
        match entry {
            Entry::Occupied(o) => o.into_mut().add_run(instance_name, config_name, run_stat),
            Entry::Vacant(v) => {
                let mut problem_run_data = ProblemRunData::default();
                problem_run_data.add_run(instance_name, config_name, run_stat);
                v.insert(problem_run_data);
            }
        }
    }

    pub fn compute_stat(&self) -> SystemStat {
        let mut problem_stats: HashMap<ProblemName, ProblemStat> = Default::default();

        for (name, problem) in &self.inner {
            problem_stats.insert(name.clone(), problem.get_problem_stat());
        }

        SystemStat {
            problem_stats,
            config: self.config.clone(),
        }
    }
}
