use crate::Generator;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Default)]
pub struct GeneratorConfig {
    pub jobs: Vec<Job>,
    pub output_path: Option<PathBuf>,
    pub generate_report: bool,
    #[serde(skip_serializing, skip_deserializing)]
    pub generators: HashMap<String, Box<dyn Generator>>,
}

#[derive(Serialize, Deserialize, Default)]
pub struct Job {
    pub name: String,
    pub recipes: HashMap<String, Recipe>,
    pub generate: HashMap<String, u32>,
}

pub type Recipe = HashMap<String, u32>;

pub trait GetElement {
    fn get_element(&self, e: &str) -> Result<u32, String>;
}

impl GetElement for Recipe {
    fn get_element(&self, e: &str) -> Result<u32, String> {
        match self.get(e) {
            None => Err(format!("{} is undefined", e)),
            Some(e) => Ok(*e),
        }
    }
}
