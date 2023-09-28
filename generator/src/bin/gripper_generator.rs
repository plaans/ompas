use generator::config::Recipe;
use generator::domain::gripper::{GripperConfig, GripperGenerator, GripperProblem};
use generator::{Generator, Problem};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "gripper-generator",
    about = "Generation of problems for gripper domain"
)]
struct Opt {
    #[structopt(short = "b", long = "ball")]
    ball: u32,
    #[structopt(short = "t", long = "task")]
    task: u32,
    #[structopt(short = "r", long = "room")]
    room: u32,
}

pub fn main() {
    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);

    //let config = GripperConfig::new(opt.ball, opt.room, opt.task);

    let mut recipe = Recipe::new();
    recipe.insert("ball".to_string(), opt.ball);
    recipe.insert("room".to_string(), opt.room);
    recipe.insert("task".to_string(), opt.task);

    let problem = GripperGenerator::default().new_problem(&recipe).unwrap();
    let sompas = problem.to_sompas();
    println!("{sompas}")
}
