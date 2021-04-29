use crate::core::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use crate::core::structs::{
    AsModule, LError, LFn, LMutFn, LNumber, LValue, Module, NameTypeLValue,
};
use crate::core::RefLEnv;
use aries_utils::input::Sym;
use im::HashMap;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::thread::JoinHandle;

/*
LANGUAGE
 */

pub const NEW_ROBOT: &str = "new-robot";
pub const KILL_ROBOT: &str = "kill-robot";
pub const START_ROBOT: &str = "start-robot";
pub const STOP_ROBOT: &str = "stop-robot";
pub const EXEC: &str = "exec";
pub const START_ROBOT_HANDLER: &str = "start-robot-handler";

/*
Name of the commands
 */

pub const COMMAND_MOVE: &str = "move";
pub const COMMAND_PICK: &str = "pick";
pub const COMMAND_PLACE: &str = "place";

/*
OTHER
 */

type RobotId = usize;

pub struct CtxRobot {
    robots: Vec<VirtualRobot>,
    map_robot_id: HashMap<Sym, RobotId>,
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

impl AsModule for CtxRobot {
    fn get_module() -> Module {
        let mut prelude = vec![];

        prelude.push((
            NEW_ROBOT.into(),
            LValue::MutFn(LMutFn::new(Box::new(new_robot), NEW_ROBOT.into())),
        ));
        prelude.push((
            STOP_ROBOT.into(),
            LValue::MutFn(LMutFn::new(Box::new(new_robot), STOP_ROBOT.into())),
        ));
        prelude.push((
            EXEC.into(),
            LValue::Fn(LFn::new(Box::new(exec), EXEC.into())),
        ));
        prelude.push((
            START_ROBOT_HANDLER.into(),
            LValue::MutFn(LMutFn::new(
                Box::new(start_robot_handler),
                START_ROBOT_HANDLER.into(),
            )),
        ));
        /*
        NOT YET IMPLEMENTED
         */
        //prelude.push((KILL_ROBOT.into(), LValue::MutFn(LMutFn::new(Box::new(kill_robot), KILL_ROBOT.into()))));
        //prelude.push((START_ROBOT.into(), LValue::MutFn(LMutFn::new(Box::new(start_robot), START_ROBOT.into()))));

        Module {
            ctx: Box::new(CtxRobot::default()),
            prelude,
        }
    }
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
            Err(e) => panic!(e),
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
