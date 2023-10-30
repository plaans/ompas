use ompas_core::ompas::interface::stat::OMPASRunStat;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

pub mod config;

pub struct OMPASStatCollection {
    inner: HashMap<String, Vec<OMPASRunStat>>,
}

impl OMPASStatCollection {
    pub fn new(path: &Path) -> Self {
        let now = SystemTime::now();
        let mut collection = Self {
            inner: Default::default(),
        };

        let json_files = Self::get_all_json_files_in_dir(path);

        println!("Found {} json files...", json_files.len());

        let mut n_files = 0;

        'loop_file: for file in json_files {
            let name = file.file_name().unwrap().to_str().unwrap().to_string();

            let content = match fs::read_to_string(&file) {
                Ok(s) => s,
                Err(e) => {
                    println!(
                        "WARNING! Could not read content of file {}: {}",
                        file.display(),
                        e
                    );
                    continue 'loop_file;
                }
            };
            let stat: OMPASRunStat = match serde_json::from_str(&content) {
                Ok(s) => s,
                Err(e) => {
                    println!(
                        "WARNING! Could not extract state from file {}: {}",
                        file.display(),
                        e
                    );
                    continue 'loop_file;
                }
            };

            //println!("Adding state of file {}", file.display());
            n_files += 1;
            collection.add_stat(name, stat);
        }

        println!(
            "Loaded {} stat file in {:.3} ms",
            n_files,
            now.elapsed().unwrap().as_secs_f64() * 1000.0
        );

        collection
    }

    fn get_all_json_files_in_dir(path: &Path) -> Vec<PathBuf> {
        let mut json_files = vec![];

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
                } else if str.contains(".json") && path.is_file() {
                    json_files.push(path);
                }
            }
        }

        json_files
    }

    pub fn add_stat(&mut self, name: String, stat: OMPASRunStat) {
        let entry = self.inner.entry(name);
        match entry {
            Entry::Occupied(o) => {
                o.into_mut().push(stat);
            }
            Entry::Vacant(v) => {
                v.insert(vec![stat]);
            }
        }
    }
}
