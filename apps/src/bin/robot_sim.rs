use ompas_lisp::core::LEnv;
use ompas_lisp::structs::{LError, LValue};
use ompas_modules::robot::{exec, new_robot, start_robot_handler, CtxRobot};
use std::thread;
use std::time::Duration;

const START_MSG: &str = "|||SIMPLE ROBOT SIMULATOR|||";

fn main() {
    if test().is_ok() {}
}

pub fn test() -> Result<LValue, LError> {
    println!("{}", START_MSG);
    let ctx_robot = &mut CtxRobot::default();
    let env = &mut LEnv::root();
    let _robot_handler = start_robot_handler(&[], env, ctx_robot);
    let robot1 = new_robot(&[], env, ctx_robot)?;
    let robot2 = new_robot(&[], env, ctx_robot)?;
    let command_1 = &[robot1, "move to kitchen".to_string().into()];
    let command_2 = &[robot2, "move to bedroom".to_string().into()];

    for _i in 1..10 {
        exec(command_1, env, ctx_robot)?;
        exec(command_2, env, ctx_robot)?;
        thread::sleep(Duration::from_secs(1));
    }

    Ok(LValue::Nil)
}
