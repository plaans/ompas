use ompas_generator::config::Recipe;
use ompas_generator::generator::gripper::{BALL, ROOM, TASK};
use ompas_generator::generator::gripper_door::{GripperDoorGenerator, MAX_DISTANCE, MAX_EDGE};
use ompas_generator::Generator;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "gripper-door-generator",
    about = "Generation of problems for gripper-door domain"
)]
struct Opt {
    #[structopt(short = "b", long = "ball")]
    ball: u32,
    #[structopt(short = "t", long = "task")]
    task: u32,
    #[structopt(short = "r", long = "room")]
    room: u32,
    #[structopt(short = "d", long = "distance")]
    max_distance: u32,
    #[structopt(short = "e", long = "edge")]
    max_edge: u32,
}

pub fn main() {
    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);

    let mut recipe = Recipe::new();
    recipe.insert(BALL.to_string(), opt.ball);
    recipe.insert(ROOM.to_string(), opt.room);
    recipe.insert(TASK.to_string(), opt.task);
    recipe.insert(MAX_DISTANCE.to_string(), opt.max_distance);
    recipe.insert(MAX_EDGE.to_string(), opt.max_edge);

    let problem = GripperDoorGenerator::default()
        .new_problem(&recipe)
        .unwrap();
    let sompas = problem.to_sompas();
    println!("{sompas}")
}
