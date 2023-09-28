use crate::domain::gripper::GripperProblem;
use crate::{Generator, Problem};
use serde::{Deserialize, Serialize};
use std::any::Any;
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
