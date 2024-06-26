use ompas_generator::config::GeneratorConfig;
use ompas_generator::generator::continuous_shop::ContinuousShopGenerator;
use ompas_generator::generator::gripper::GripperGenerator;
use ompas_generator::generator::gripper_build::GripperBuildGenerator;
use ompas_generator::generator::gripper_door::GripperDoorGenerator;
use ompas_generator::generator::gripper_multi::GripperMultiGenerator;
use ompas_generator::generator::jobshop::JobshopGenerator;
use ompas_middleware::OMPAS_WORKING_DIR;
use std::fs;
use std::fs::create_dir_all;
use std::path::PathBuf;
use structopt::StructOpt;

const GRIPPER: &str = "gripper";
const GRIPPER_DOOR: &str = "gripper-door";
const GRIPPER_MULTI: &str = "gripper-multi";
const GRIPPER_BUILD: &str = "gripper-build";
const JOBSHOP: &str = "jobshop";
const CONTINUOUS_SHOP: &str = "continuous-shop";

#[derive(Debug, StructOpt)]
#[structopt(
    name = "Generator",
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
    config
        .generators
        .insert(JOBSHOP.to_string(), Box::<JobshopGenerator>::default());
    config.generators.insert(
        CONTINUOUS_SHOP.to_string(),
        Box::<ContinuousShopGenerator>::default(),
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
                file_path.push(format!("{}.scm", name));
                let mut pb = generator.new_problem(recipe)?;

                pb.store(&file_path);

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
