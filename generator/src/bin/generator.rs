use generator::config::GeneratorConfig;
use generator::domain::gripper::GripperGenerator;
use ompas_middleware::OMPAS_WORKING_DIR;
use std::fs;
use std::fs::File;
use std::io::Write;
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
                    let report = pb.report();
                    let mut report_path = path.clone();
                    report_path.push(format!("{}_report.{}", name, report.extension));
                    let mut file = File::create(report_path).unwrap();
                    file.write_all(report.content.as_bytes()).unwrap();
                }
            }
        }
    }

    Ok(())
}
