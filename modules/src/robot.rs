#![allow(dead_code)]
use crate::doc::{Documentation, LHelp};
use aries_utils::input::Sym;
use ompas_lisp::core::RefLEnv;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::thread::JoinHandle;

/*
LANGUAGE
 */

const MOD_ROBOT: &str = "mod-robot";
const DOC_MOD_ROBOT: &str = "Documentation of the robot module";

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
    map_robot_id: im::HashMap<Sym, RobotId>,
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
    //TODO: doc
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            label: MOD_ROBOT,
        };

        module.add_mut_fn_prelude(NEW_ROBOT, Box::new(new_robot));
        module.add_fn_prelude(EXEC, Box::new(exec));
        module.add_mut_fn_prelude(START_ROBOT_HANDLER, Box::new(start_robot_handler));

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

const DOC_NEW_ROBOT: &str = "";
const DOC_EXEC: &str = "";
const DOC_START_ROBOT_HANDLER: &str = "";

impl Documentation for CtxRobot {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_ROBOT, DOC_MOD_ROBOT, None),
            LHelp::new(NEW_ROBOT, DOC_NEW_ROBOT, None),
            LHelp::new(EXEC, DOC_EXEC, None),
            LHelp::new(START_ROBOT_HANDLER, DOC_START_ROBOT_HANDLER, None),
        ]
    }
}

const ROBOT_HANDLER_START_MSG: &str = "Robot handler started!!!\n\
                                           Listening...";
fn robot_handler(rx: Receiver<String>) {
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

pub fn exec(args: &[LValue], _: &RefLEnv, ctx: &CtxRobot) -> Result<LValue, LError> {
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
            Ok(LValue::None)
        }
        _ => Err(SpecialError("Expected a RobotId(usize)".to_string())),
    }
}

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

pub fn new_robot(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxRobot) -> Result<LValue, LError> {
    let (robot_label, thread_label) = match args.len() {
        0 => (
            format!("unnamed_robot_{}", ctx.robots.len()).into(),
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

pub fn start_robot_handler(
    _: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxRobot,
) -> Result<LValue, LError> {
    let (tx, rx) = channel();
    ctx.robots_sender = Some(tx);
    thread::Builder::new()
        .name("robot_handler".to_string())
        .spawn(move || robot_handler(rx))
        .expect("error creating robot handler");
    Ok(LValue::None)
}

/*
ROBOTS COMMAND
 */

pub fn command_move(_: &[LValue], _: &mut RefLEnv, _: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}
pub fn command_pick(_: &[LValue], _: &mut RefLEnv, _: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}
pub fn command_place(_: &[LValue], _: &mut RefLEnv, _: &mut CtxRobot) -> Result<LValue, LError> {
    unimplemented!()
}
