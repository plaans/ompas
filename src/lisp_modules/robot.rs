
use crate::lisp_root::lisp_struct::{AsModule, Module, LError, LValue};
use crate::lisp_root::{LEnv, RefLEnv};
use std::thread;
use std::time::Duration;
use std::thread::JoinHandle;
use im::HashMap;
use aries_utils::input::Sym;
use std::sync::mpsc::{Sender, Receiver, RecvError, channel};

/*
LANGUAGE
 */

pub const NEW_ROBOT: &str= "new-robot";
pub const KILL_ROBOT: &str = "kill-robot";
pub const START_ROBOT: &str= "start-robot";
pub const STOP_ROBOT: &str = "stop-robot";
pub const EXEC: &str = "exec";
pub const START_ROBOT_HANDLER: &str= "start-robot-handler";

/*
Name of the commands
 */

pub const COMMAND_MOVE: &str = "move";
pub const COMMAND_PICK: &str = "pick";
pub const COMMAND_PLACE: &str = "place";

type RobotId = usize;

pub struct CtxRobot {
    robots: Vec<VirtualRobot>,
    map_robot_id: HashMap<Sym, RobotId>,
    robot_handler: RobotHandler,
}

impl Default for CtxRobot {
    fn default() -> Self {
        todo!()
    }
}

impl CtxRobot {
    pub fn push_robot(&mut self, virtual_robot: VirtualRobot) -> RobotId {
        self.robots.push(virtual_robot);
        self.robots.len() - 1
    }
}

struct RobotHandler {
    receiver: Receiver<String>,
    sender: Sender<String>,
}


pub const ROBOT_HANDLER_START_MSG: &str = "Robot handler started!!!\n\
                                                Listening...";
fn robot_handler(rx: Receiver<String>) {
    println!("{}", ROBOT_HANDLER_START_MSG);

    loop {
        match rx.recv() {
            Ok(lv) => println!("{}", lv),
            Err(e) => panic!(e),
        }
    }
}

struct ArgRobot {
    sender: Sender<String>,
    receiver: Receiver<String>,
}

pub struct VirtualRobot {
    join_handle: JoinHandle<()>,
    sender: Sender<String>,
    //receiver: Receiver<String>,

}

impl VirtualRobot {
    pub fn new(join_handle: JoinHandle<()>, sender: Sender<String>, receiver: Receiver<String>) -> Self {
        VirtualRobot {
            join_handle,
            sender,
            //receiver,
        }
    }
}


impl AsModule for CtxRobot {
    fn get_module() -> Module {

        Module {
            ctx: Box::new(()),
            prelude: vec![]
        }
    }
}

pub fn exec(args: &[LValue], env: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}

pub fn command_move(args: &[LValue], env: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}
pub fn command_pick(args: &[LValue], env: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}
pub fn command_place(args: &[LValue], env: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}

fn robot(arg_robot: ArgRobot) {
    println!("Hi!! I'm a new robot\n");
    loop {
        println!("waiting for order...");
        match arg_robot.receiver.recv() {
            Ok(lv) => println!("{}", lv),
            Err(e) => panic!(e),
        };
    }
}


pub fn new_robot(args: &[LValue], env: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {

    let channel_sup_to_robot = channel();
    let arg_robot = ArgRobot {
        sender: ctx.robot_handler.sender.clone(),
        receiver: channel_sup_to_robot.1,
    };

    let join_handle:JoinHandle<()> = thread::spawn(move || {
        robot(arg_robot)
    });

    let virtual_robot = VirtualRobot {
        join_handle,
        sender: channel_sup_to_robot.0,
    };



    let robot_id = ctx.push_robot(virtual_robot);
    Ok(robot_id.into())
}

pub fn start_robot_handler(args: &[LValue], env: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    let (tx, rx) = channel();
    ctx.robot_handler.sender = tx;
    thread::spawn(move || {
        robot_handler(rx)
    });
    Ok(LValue::None)
}