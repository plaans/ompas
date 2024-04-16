use ompas_generator::config::{GeneratorConfig, Job, Recipe};
use ompas_generator::generator::gripper::{BALL, ROOM, TASK};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "test_yaml",
    about = "Test Yaml serialize and deserialize for config of generator"
)]
struct Opt {}

pub fn main() {
    let mut config = GeneratorConfig::default();
    let mut gripper_job = Job {
        name: "gripper".to_string(),
        ..Default::default()
    };

    let mut simple = Recipe::default();
    simple.insert(TASK.to_string(), 1);
    simple.insert(ROOM.to_string(), 2);
    simple.insert(BALL.to_string(), 2);
    gripper_job.recipes.insert("simple".to_string(), simple);
    gripper_job.generate.insert("simple".to_string(), 2);
    config.jobs.push(gripper_job);
    let yaml = serde_yaml::to_string(&config).unwrap();
    println!("{yaml}")
}
