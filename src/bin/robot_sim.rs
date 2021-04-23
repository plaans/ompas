use fact_base::lisp_modules::robot::{CtxRobot, new_robot, start_robot_handler, exec};
use fact_base::lisp_root::RefLEnv;
use std::time::Duration;
use std::thread;
use fact_base::lisp_root::lisp_struct::{LValue, LError};

const START_MSG: &str = "|||SIMPLE ROBOT SIMULATOR|||";

fn main() {
    match test() {
        Ok(_) => {}
        Err(_) => {}
    }
}

pub fn test() -> Result<LValue, LError> {
    println!("{}", START_MSG);
    let ctx_robot = &mut CtxRobot::default();
    let env = &mut RefLEnv::root();
    let robot_handler = start_robot_handler(&[], env, ctx_robot);
    let robot1 = new_robot(&[], env, ctx_robot)?;
    let robot2 = new_robot(&[], env, ctx_robot)?;
    let command_1 = &[robot1, "move to kitchen".to_string().into()];
    let command_2 = &[robot2, "move to bedroom".to_string().into()];

    for i in 1..10 {
        exec(command_1, env, ctx_robot);
        //exec(command_2, env, ctx_robot);
        thread::sleep(Duration::from_secs(1));
    }

    Ok(LValue::None)

}
