use generator::config::Recipe;
use generator::generator::gobot::{MAX_TIME, MIN_TIME, PACKAGE, PROCESS};
use generator::generator::jobshop::JobshopGenerator;
use generator::Generator;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "jobshop-generator",
    about = "Generation of jobshop problems in GobotSim domain"
)]
struct Opt {
    #[structopt(short = "p", long = "process")]
    process: u32,
    #[structopt(short = "j", long = "job")]
    package: u32,
    #[structopt(short = "l", long = "min_time")]
    min_time: u32,
    #[structopt(short = "u", long = "max_time")]
    max_time: u32,
}

pub fn main() {
    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);

    let mut recipe = Recipe::new();
    recipe.insert(PACKAGE.to_string(), opt.package);
    recipe.insert(PROCESS.to_string(), opt.process);
    recipe.insert(MIN_TIME.to_string(), opt.min_time);
    recipe.insert(MAX_TIME.to_string(), opt.max_time);
    let mut problem = JobshopGenerator::default().new_problem(&recipe).unwrap();
    let mut path: PathBuf = "/tmp/jobshop".into();
    let _ = fs::create_dir_all(&path);
    path.push("problem.scm");
    problem.store(&path);
    let sompas = problem.to_sompas();
    println!("{sompas}");
}
