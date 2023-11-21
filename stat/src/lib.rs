use ompas_core::ompas::interface::stat::OMPASRunStat;
use ompas_language::output::{JSON_FORMAT, YAML_FORMAT};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

pub mod config;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FileName {
    inner: Vec<String>,
}

pub struct OMPASStatCollection {
    inner: HashMap<FileName, Vec<OMPASRunStat>>,
}

impl OMPASStatCollection {
    pub fn new(path: &Path) -> Self {
        let now = SystemTime::now();
        let mut collection = Self {
            inner: Default::default(),
        };

        let json_files = Self::get_all_files_in_dir(path, JSON_FORMAT);
        println!("Found {} json files...", json_files.len());

        let yaml_files = Self::get_all_files_in_dir(path, YAML_FORMAT);
        println!("Found {} yaml files...", yaml_files.len());

        for file in json_files {
            collection.read_file(&file, JSON_FORMAT)
        }

        for file in yaml_files {
            collection.read_file(&file, YAML_FORMAT)
        }

        println!(
            "Loaded {} stat file in {:.3} ms",
            collection.get_number_of_stat(),
            now.elapsed().unwrap().as_secs_f64() * 1000.0
        );

        collection
    }

    fn read_file(&mut self, file: &Path, format: &str) {
        let file_name = FileName {
            inner: file
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .split("_")
                .map(|s| s.to_string())
                .collect(),
        };
        let content = match fs::read_to_string(&file) {
            Ok(s) => s,
            Err(_) => {
                return;
            }
        };
        let stat: OMPASRunStat = match format {
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

        println!("Adding stat of file {}", file.display());
        self.add_stat(file_name, stat);
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

    pub fn add_stat(&mut self, file_name: FileName, stat: OMPASRunStat) {
        let entry = self.inner.entry(file_name);
        match entry {
            Entry::Occupied(o) => {
                o.into_mut().push(stat);
            }
            Entry::Vacant(v) => {
                v.insert(vec![stat]);
            }
        }
    }

    pub fn get_number_of_stat(&self) -> usize {
        self.inner.iter().fold(0, |d, (_, vec)| d + vec.len())
    }
}
