use generator::config::Recipe;
use generator::generator::gripper::GripperGenerator;
use generator::Generator;
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
    let generator = GripperGenerator::gripper_door();

    let recipe = Recipe::default();
    let problem = generator.new_problem(&recipe).unwrap();
    let sompas = problem.to_sompas();
    println!("{sompas}")
}
