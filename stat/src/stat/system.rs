use crate::stat::config::{ConfigName, ConfigProblemStat};
use crate::stat::problem::{ProblemName, ProblemRunData, ProblemStat};
use ompas_core::ompas::interface::stat::OMPASRunData;
use ompas_language::output::{JSON_FORMAT, YAML_FORMAT};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
pub type RunName = Vec<String>;

#[derive(Default)]
pub struct SystemStat {
    pub problem_stats: HashMap<ProblemName, ProblemStat>,
}

#[derive(Clone)]
pub struct Cell {
    pre_sep: Option<char>,
    post_sep: Option<char>,
    info: String,
}

impl Cell {
    pub fn empty() -> Self {
        Self {
            pre_sep: None,
            post_sep: None,
            info: "".to_string(),
        }
    }

    pub fn end(info: String) -> Self {
        Self {
            pre_sep: None,
            post_sep: Some('|'),
            info,
        }
    }

    pub fn start(info: String) -> Self {
        Self {
            pre_sep: Some('|'),
            post_sep: None,
            info,
        }
    }

    pub fn double(info: String) -> Self {
        Self {
            pre_sep: Some('|'),
            post_sep: Some('|'),
            info,
        }
    }

    pub fn format(&self, cell_size: usize) -> String {
        let mut string = "".to_string();
        if let Some(pre) = self.pre_sep {
            string.push(pre)
        }

        write!(string, "{:^cell_size$}", self.info).unwrap();

        if let Some(post) = self.post_sep {
            string.push(post)
        }

        string
    }
}

pub struct SystemStatFormatter {
    lines: Vec<Vec<Cell>>,
}

impl SystemStatFormatter {
    pub fn to_csv(&self) -> String {
        let mut string = String::new();
        for cells in &self.lines {
            for (i, cell) in cells.iter().enumerate() {
                if i != 0 {
                    string.push(';');
                }
                write!(string, "{}", cell.info).unwrap();
            }
            string.push('\n');
        }

        string
    }
}

impl From<&SystemStat> for SystemStatFormatter {
    fn from(value: &SystemStat) -> Self {
        let mut configs: HashMap<&ConfigName, HashMap<&ProblemName, &ConfigProblemStat>> =
            Default::default();
        let mut config_order: Vec<&ConfigName> = Default::default();
        for (name, problem) in &value.problem_stats {
            for (config_name, config) in &problem.inner {
                match configs.entry(config_name) {
                    Entry::Occupied(mut o) => {
                        o.get_mut().insert(name, config);
                    }
                    Entry::Vacant(v) => {
                        config_order.push(config_name);
                        let mut map = HashMap::default();
                        map.insert(name, config);
                        v.insert(map);
                    }
                };
            }
        }

        let mut lines = vec![];
        let mut first_line = vec![Cell::start("".to_string()), Cell::double("".to_string())];
        let mut second_line = vec![
            Cell::start("Problem".to_string()),
            Cell::double("Difficulty".to_string()),
        ];
        let header = ConfigProblemStat::header();
        let header_len = header.len();
        let mut empty_info = vec![Cell::start("".to_string()); header_len - 1];
        empty_info.push(Cell::double("".to_string()));
        for config_name in &config_order {
            first_line.push(Cell::start(config_name.to_string()));
            first_line.append(&mut vec![
                Cell {
                    pre_sep: Some(' '),
                    post_sep: None,
                    info: "".to_string(),
                };
                header_len - 2
            ]);
            first_line.push(Cell {
                pre_sep: Some(' '),
                post_sep: Some('|'),
                info: "".to_string(),
            });
            second_line.append(&mut header.clone())
        }
        lines.push(first_line);
        lines.push(second_line);

        let mut names: Vec<_> = value.problem_stats.keys().collect();
        names.sort();

        for problem_name in names {
            let mut line = vec![
                Cell::start(problem_name.domain.to_string()),
                Cell::double(problem_name.difficulty.to_string()),
            ];
            for config in &config_order {
                let config = configs.get(config).unwrap();
                match config.get(problem_name) {
                    None => {
                        line.append(&mut empty_info.clone());
                    }
                    Some(config) => line.append(&mut config.to_formatted()),
                }
            }
            lines.push(line);
        }
        SystemStatFormatter { lines }
    }
}

impl Display for SystemStatFormatter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut column_size: Vec<usize> = vec![0; self.lines[0].len()];
        for cells in &self.lines {
            for (i, cell) in cells.iter().enumerate() {
                column_size[i] = column_size[i].max(cell.info.len())
            }
        }

        for cells in &self.lines {
            for (i, cell) in cells.iter().enumerate() {
                write!(f, "{}", cell.format(column_size[i]))?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Default)]
pub struct SystemRunData {
    inner: HashMap<ProblemName, ProblemRunData>,
}

impl SystemRunData {
    pub fn new(path: &Path) -> Self {
        let now = SystemTime::now();
        let mut collection = Self {
            inner: Default::default(),
        };

        let json_files = Self::get_all_files_in_dir(path, JSON_FORMAT);
        //println!("Found {} json files...", json_files.len());

        let yaml_files = Self::get_all_files_in_dir(path, YAML_FORMAT);
        //println!("Found {} yaml files...", yaml_files.len());

        for file in json_files {
            collection.read_file(&file, JSON_FORMAT)
        }

        for file in yaml_files {
            collection.read_file(&file, YAML_FORMAT)
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
            difficulty: file_name[1].to_string(),
        };

        let instance_name = file_name[2].to_string();

        let config_name = ConfigName {
            select_heuristic: file_name[3].to_string(),
            continuous_planning_config: file_name[4].to_string(),
            other: file_name[5..].to_vec(),
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
        let mut system_stat = SystemStat::default();

        for (name, problem) in &self.inner {
            system_stat
                .problem_stats
                .insert(name.clone(), problem.get_problem_stat());
        }

        system_stat
    }
}
