/*#![allow(dead_code)]
use crate::doc::{Documentation, LHelp};
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;
use std::thread;
use std::thread::JoinHandle;
use std::sync::Arc;
use tokio::sync::mpsc::{Sender, Receiver};

/*
LANGUAGE
 */

const MOD_ROBOT: &str = "mod-robot";
const DOC_MOD_ROBOT: &str = "module to handle functions to access robots";
const DOC_MOD_ROBOT_VERBOSE: &str = "functions:\n\
                                        - new-robot\n\
                                        - start-robot-handler\n\
                                        - exec";

const NEW_ROBOT: &str = "new-robot";
const KILL_ROBOT: &str = "kill-robot";
const START_ROBOT: &str = "start-robot";
const STOP_ROBOT: &str = "stop-robot";
const EXEC: &str = "exec";
const START_ROBOT_HANDLER: &str = "start-robot-handler";

/*
Name of the commands
 */

const COMMAND_MOVE: &str = "move";
const COMMAND_PICK: &str = "pick";
const COMMAND_PLACE: &str = "place";

/*
OTHER
 */

type RobotId = usize;

pub struct CtxRobot {
    robots: Vec<VirtualRobot>,
    map_robot_id: im::HashMap<String, RobotId>,
    robots_sender: Option<Sender<String>>,
}

impl Default for CtxRobot {
    fn default() -> Self {
        CtxRobot {
            robots: vec![],
            map_robot_id: Default::default(),
            robots_sender: None,
        }
    }
}

impl CtxRobot {
    pub fn push_robot(&mut self, virtual_robot: VirtualRobot) -> RobotId {
        self.robots.push(virtual_robot);
        self.robots.len() - 1
    }
}

impl GetModule for CtxRobot {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_ROBOT,
        };

        /*module.add_mut_fn_prelude(NEW_ROBOT, Box::new(new_robot));
        //module.add_fn_prelude(EXEC, Box::new(exec));
        module.add_mut_fn_prelude(START_ROBOT_HANDLER, Box::new(start_robot_handler));*/

        /*
        NOT YET IMPLEMENTED
         */
        //
        //module.add_mut_fn_prelude(START_ROBOT_HANDLER, Box::new(start_robot_handler));
        //module.add_mut_fn_prelude(NEW_ROBOT, Box::new(new_robot));
        module
    }
}

/*
DOCUMENTATION
 */

const DOC_NEW_ROBOT: &str = "Creates a new-robot. Return its id";
const DOC_NEW_ROBOT_VERBOSE: &str = "Takes no argument.\n
                                     Example: (define bob (new-robot))";
const DOC_EXEC: &str = "Execute a command on a robot";
const DOC_EXEC_VERBOSE: &str = "Takes a robot and a list of symbol as arguments.\n
                              Example: (exec bob move bedroom kitchen)";
const DOC_START_ROBOT_HANDLER: &str = "Start the thread to handle messages from the robots";
const DOC_START_ROBOT_HANDLER_VERBOSE: &str = "Only to do once at the beginning of the program. Creating a new-robot will fail if not ran before.";

impl Documentation for CtxRobot {
    /// Add to the CtxDoc the description of the functions added by CtxRobot
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_ROBOT, DOC_MOD_ROBOT, Some(DOC_MOD_ROBOT_VERBOSE)),
            LHelp::new(NEW_ROBOT, DOC_NEW_ROBOT, Some(DOC_NEW_ROBOT_VERBOSE)),
            LHelp::new(EXEC, DOC_EXEC, Some(DOC_EXEC_VERBOSE)),
            LHelp::new(
                START_ROBOT_HANDLER,
                DOC_START_ROBOT_HANDLER,
                Some(DOC_START_ROBOT_HANDLER_VERBOSE),
            ),
        ]
    }
}

const ROBOT_HANDLER_START_MSG: &str = "Robot handler started!!!\n\
                                           Listening...";

/// Handles the response of the robots.
/// Should be deprecated in further updates.
/// Takes as argument the channel object receiver that receives all response.
/// Loops on it and exit at the end of the process.
fn robot_handler(mut rx: Receiver<String>) {
    println!("{}", ROBOT_HANDLER_START_MSG);

    loop {
        match rx.recv() {
            Ok(lv) => println!("{}", lv),
            Err(e) => panic!("{}", e),
        }
    }
}

struct ArgRobot {
    sender: Sender<String>,
    receiver: Receiver<String>,
}

pub struct VirtualRobot {
    _join_handle: JoinHandle<()>,
    sender: Sender<String>,
}

impl VirtualRobot {
    pub fn new(_join_handle: JoinHandle<()>, sender: Sender<String>) -> Self {
        VirtualRobot {
            _join_handle,
            sender,
        }
    }
}

/*
MODULE FUNCTIONS
 */

/// Execute a command on a given robot.
/// Test if the robot exists and return an error if not.
/// Send to the robot the command as a list of symbols.
/// Return an error if there is an error while sending command via the channel.
pub fn exec(args: &[LValue], _: &LEnv, ctx: &CtxRobot) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            args.into(),
            args.len(),
            2..std::usize::MAX,
        ));
    }

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => {
            let virtual_robot = match ctx.robots.get(*u) {
                None => return Err(SpecialError("Not a valid RobotId".to_string())),
                Some(vr) => vr,
            };
            virtual_robot
                .sender
                .send(args[1].to_string())
                .expect("couldn't send command to robot");
            Ok(LValue::Nil)
        }
        _ => Err(SpecialError("Expected a RobotId(usize)".to_string())),
    }
}

/// Function executed in the thread of each robot.
/// For the moment it only waits on an order and write it in stdout.
/// Return a formatted message of the action to robot handler as an ack.
fn robot(arg_robot: ArgRobot) {
    println!("Hi!! I'm a new robot");
    loop {
        println!("waiting for order...");
        match arg_robot.receiver.recv() {
            Ok(lv) => {
                println!("{}", lv);
                arg_robot
                    .sender
                    .send(format!("action {} OK!", lv))
                    .expect("couldn't send response");
            }
            Err(e) => panic!("{}", e),
        };
    }
}

/// Creates a new thread that runs the functions **robot**
///
/// Takes as optional argument a label for the robot.
/// Takes CtxRobot as mutable the new robot to it.
///
/// A new thread and a channel to communicate with it is created.
///
/// Returns the robot id.
pub fn new_robot(args: &[LValue], _: &LEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    let (robot_label, thread_label) = match args.len() {
        0 => (
            format!("unnamed_robot_{}", ctx.robots.len()),
            format!("unnamed_robot_{}", ctx.robots.len()),
        ),
        1 => match &args[0] {
            LValue::Symbol(s) => (s.clone(), format!("thread_{}", s)),
            lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        _ => return Err(WrongNumberOfArgument(args.into(), args.len(), 0..1)),
    };

    let channel_sup_to_robot = channel();
    let arg_robot = ArgRobot {
        sender: ctx
            .robots_sender
            .as_ref()
            .expect("robot handler missing")
            .clone(),
        receiver: channel_sup_to_robot.1,
    };

    let _join_handle: JoinHandle<()> = thread::Builder::new()
        .name(thread_label)
        .spawn(move || robot(arg_robot))
        .expect("Error creating new-robot");

    let virtual_robot = VirtualRobot {
        _join_handle,
        sender: channel_sup_to_robot.0,
    };

    let robot_id = ctx.push_robot(virtual_robot);
    ctx.map_robot_id.insert(robot_label, robot_id);
    Ok(robot_id.into())
}

/// Starts the robot handler.
/// Takes CtxRobot as mutable to add the sender object to the context.
///
/// Spawn a new thread running function **robot_handler**
pub fn start_robot_handler(
    _: &[LValue],
    _: &LEnv,
    ctx: &mut CtxRobot,
) -> Result<LValue, LError> {
    let (tx, rx) = channel();
    ctx.robots_sender = Some(tx);
    thread::Builder::new()
        .name("robot_handler".to_string())
        .spawn(move || robot_handler(rx))
        .expect("error creating robot handler");
    Ok(LValue::Nil)
}

/*
ROBOTS COMMAND
 */

/// Primitive of the robot to move between two places.
/// Not yet implemented
pub fn command_move(_: &[LValue], _: &LEnv, _: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}

/// Primitive of the robot to pick an object.
/// Not yet implemented
pub fn command_pick(_: &[LValue], _: &LEnv, _: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}

/// Primitive of the robot to place an object.
/// Not yet implemented.
pub fn command_place(_: &[LValue], _: &LEnv, _: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}*/
