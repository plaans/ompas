use generator::config::GeneratorConfig;
use generator::generator::gripper::GripperGenerator;
use generator::generator::gripper_build::GripperBuildGenerator;
use generator::generator::gripper_door::GripperDoorGenerator;
use generator::generator::gripper_multi::GripperMultiGenerator;
use ompas_middleware::OMPAS_WORKING_DIR;
use std::fs;
use std::fs::{create_dir_all, File};
use std::io::Write;
use std::path::PathBuf;
use structopt::StructOpt;

const GRIPPER: &str = "gripper";
const GRIPPER_DOOR: &str = "gripper-door";
const GRIPPER_MULTI: &str = "gripper-multi";
const GRIPPER_BUILD: &str = "gripper-build";

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
        .insert(GRIPPER.to_string(), Box::<GripperGenerator>::default());

    config.generators.insert(
        GRIPPER_DOOR.to_string(),
        Box::<GripperDoorGenerator>::default(),
    );
    config.generators.insert(
        GRIPPER_MULTI.to_string(),
        Box::<GripperMultiGenerator>::default(),
    );
    config.generators.insert(
        GRIPPER_BUILD.to_string(),
        Box::<GripperBuildGenerator>::default(),
    );

    println!("{} to do...", config.jobs.len());
    let mut path = config
        .output_path
        .unwrap_or(OMPAS_WORKING_DIR.get_ref().into());
    path.push("generated_problems");
    for job in config.jobs {
        let mut path = path.clone();
        path.push(&job.name);
        let generator = config
            .generators
            .get(&job.name)
            .ok_or_else(|| format!("No generator for {} domain", job.name))?;
        for (recipe_label, n) in job.generate {
            let mut path = path.clone();
            path.push(&recipe_label);
            let recipe = job
                .recipes
                .get(&recipe_label)
                .ok_or_else(|| format!("Recipe {} is undefined", recipe_label))?;
            fs::create_dir_all(&path).unwrap();
            for i in 0..n {
                let mut file_path = path.clone();
                let name = format!("{}_{}_{}", job.name, recipe_label, i);
                file_path.push(format!("{}.lisp", name));
                let pb = generator.new_problem(recipe)?;

                //println!("{}", pb.to_sompas());

                let mut file = File::create(file_path).unwrap();
                file.write_all(pb.to_sompas().as_bytes()).unwrap();
                if config.generate_report {
                    let mut report_path = path.clone();
                    report_path.push(format!("{}_report", name));
                    create_dir_all(report_path.clone()).unwrap();
                    let _ = pb.report(report_path);
                }
            }
        }
    }

    Ok(())
}
