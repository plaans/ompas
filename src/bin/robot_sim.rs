use fact_base::lisp_modules::robot::{CtxRobot, new_robot};
use fact_base::lisp_root::RefLEnv;
use std::time::Duration;
use std::thread;

const START_MSG: &str = "|||SIMPLE ROBOT SIMULATOR|||";

fn main() {
    println!("{}", START_MSG);
    let ctx_robot = &mut CtxRobot::default();
    let env = &mut RefLEnv::root();
    let robot1 = new_robot(&[], env, ctx_robot);
    let robot2 = new_robot(&[], env, ctx_robot);

    thread::sleep(Duration::from_secs(10));
}
