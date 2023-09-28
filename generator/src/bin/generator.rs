use generator::config::GeneratorConfig;
use generator::domain::gripper::GripperGenerator;
use generator::Problem;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "test_yaml",
    about = "Generation of problems for gripper domain"
)]
struct Opt {
    #[structopt(short = "c", long = "config")]
    config: PathBuf,
}

pub fn main() -> Result<(), String> {
    let opt = Opt::from_args();

    let str = fs::read_to_string(opt.config).expect("Could not read config file");

    let mut config: GeneratorConfig =
        serde_yaml::from_str(&str).expect("Could not deserialize content of config");

    config
        .generators
        .insert("gripper".to_string(), Box::new(GripperGenerator::default()));

    println!("{} to do...", config.jobs.len());
    for job in config.jobs {
        let generator = config
            .generators
            .get(&job.name)
            .ok_or_else(|| format!("No generator for {} domain", job.name))?;
        for (recipe, n) in job.generate {
            let recipe = job
                .recipes
                .get(&recipe)
                .ok_or_else(|| format!("Recipe {} is undefined", recipe))?;
            for i in 0..n {
                let pb = generator.new_problem(recipe)?;
                println!("{}", pb.to_sompas())
            }
        }
    }

    Ok(())
}
