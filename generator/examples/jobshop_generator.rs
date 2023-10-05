use generator::config::Recipe;
use generator::generator::jobshop::{JobshopGenerator, JOB, MAX_TIME, MIN_TIME};
use generator::Generator;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "jobshop-generator",
    about = "Generation of jobshop problems in GobotSim domain"
)]
struct Opt {
    #[structopt(short = "j", long = "job")]
    job: u32,
    #[structopt(short = "l", long = "min_time")]
    min_time: u32,
    #[structopt(short = "u", long = "max_time")]
    max_time: u32,
}

pub fn main() {
    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);

    let mut recipe = Recipe::new();
    recipe.insert(JOB.to_string(), opt.job);
    recipe.insert(MIN_TIME.to_string(), opt.min_time);
    recipe.insert(MAX_TIME.to_string(), opt.max_time);
    let mut problem = JobshopGenerator::new(2).new_problem(&recipe).unwrap();
    problem.store(&"/tmp/jobshop_problem.lisp".into());
    let sompas = problem.to_sompas();
    println!("{sompas}");
}
