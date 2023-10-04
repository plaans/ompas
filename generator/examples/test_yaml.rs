use generator::config::{GeneratorConfig, Job, Recipe};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "test_yaml",
    about = "Test Yaml serialize and deserialize for config of generator"
)]
struct Opt {}

pub fn main() {
    let mut config = GeneratorConfig::default();
    let mut gripper_job = Job::default();

    gripper_job.name = "gripper".to_string();
    let mut simple = Recipe::default();
    simple.insert("task".to_string(), 1);
    simple.insert("room".to_string(), 2);
    simple.insert("ball".to_string(), 2);
    gripper_job.recipes.insert("simple".to_string(), simple);
    gripper_job.generate.insert("simple".to_string(), 2);
    config.jobs.push(gripper_job);
    let yaml = serde_yaml::to_string(&config).unwrap();
    println!("{yaml}")
}
