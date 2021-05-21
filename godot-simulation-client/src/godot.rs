use crate::serde::{GodotMessageType, GodotState};
use crate::tcp::{read_msg_from_buf, read_size_from_buf, BUFFER_SIZE};
use ompas_lisp::core::RefLEnv;
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};
use ompas_modules::io::TOKIO_CHANNEL_SIZE;
use std::net::SocketAddr;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt, BufReader, ReadHalf, WriteHalf};
use tokio::net::TcpStream;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};

/*
LANGUAGE
*/

const MOD_GODOT: &str = "mod-godot";

//commands

const OPEN_COM: &str = "open-com-godot";
const LAUNCH_GODOT: &str = "launch-godot";
const EXEC_GODOT: &str = "exec-godot";

//State variables
//Lambda functions that will be added natively to the environment
//Depends on module state and function get-state.
//Robot
const LAMBDA_COORDINATES: &str = "(define coordinates (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote coordinates) x))))";
const SF_COORDINATES: &str = "coordinates";
const DOC_SF_COORDINATES: &str =
    "Return the coordinates (float float) of a robot, a machine or a belt.";
const DOC_SF_COORDINATES_VERBOSE: &str = "Example: (coordinates robot0)";

const LAMBDA_BATTERY: &str = "(define battery (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote battery) x))))";
const SF_BATTERY: &str = "battery";
const DOC_SF_BATTERY: &str = "Return the battery level (float) in [0;1] of a robot.";
const DOC_SF_BATTERY_VERBOSE: &str = "Example: (battery robot0)";

const LAMBDA_ROTATION: &str = "(define rotation (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote rotation) x))))";
const SF_ROTATION: &str = "rotation";
const DOC_SF_ROTATION: &str = "Return the rotation value (float) of a robot."; //TODO: Check the interval of the rotation
const DOC_SF_ROTATION_VERBOSE: &str = "Example: (rotation robot0)";

const LAMBDA_VELOCITY: &str = "(define velocity (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote velocity) x))))";

const SF_VELOCITY: &str = "velocity";
const DOC_SF_VELOCITY: &str = "Return the velocity value (float float) in x and y of a robot.";
const DOC_SF_VELOCITY_VERBOSE: &str = "Example: (velocity robot0)";

const LAMBDA_ROTATION_SPEED: &str = "(define rotation_speed (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote rotation_speed) x))))";
const SF_ROTATION_SPEED: &str = "rotation_speed";
const DOC_SF_ROTATION_SPEED: &str = "Return the rotation speed value (float) of a robot.";
const DOC_SF_ROTATION_SPEED_VERBOSE: &str = "Example: (rotation_speed robot0)";

const LAMBDA_IN_STATION: &str = "(define in_station (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote in_station) x))))";

const SF_IN_STATION: &str = "in_station";
const DOC_SF_IN_STATION: &str = "Return true if a robot is in a station.";
const DOC_SF_IN_STATION_VERBOSE: &str = "Example: (in_station robot0)";

const LAMBDA_IN_INTERACT: &str = "(define in_interact (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote in_interact) x))))";

const SF_IN_INTERACT: &str = "in_interact";
const DOC_SF_IN_INTERACT: &str = "Return true if a robot is in an interact zone.";
const DOC_SF_IN_INTERACT_VERBOSE: &str = "Example: (in_interact robot0)";

const LAMBDA_INPUT_BELT: &str = "(define input_belt (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote input_belt) x))))";

const SF_INPUT_BELT: &str = "input_belt";
const DOC_SF_INPUT_BELT: &str = "Return the name (symbol) of the input belt of a machine.";
const DOC_SF_INPUT_BELT_VERBOSE: &str = "Example: (input_belt machine0)";

const LAMBDA_OUTPUT_BELT: &str = "(define output_belt (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote output_belt) x))))";

const SF_OUTPUT_BELT: &str = "output_belt";
const DOC_SF_OUTPUT_BELT: &str = "Return the name (symbol) of the output belt of a machine.";
const DOC_SF_OUTPUT_BELT_VERBOSE: &str = "Example: (output_belt machine0)";

const LAMBDA_PROCESSES: &str = "(define processes (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote processes_list) x))))";
const SF_PROCESSES: &str = "processes";
const DOC_SF_PROCESSES: &str = "Return the list of processes (id0 id1 ...) of a machine, or return the list of pair (process_id, duration) for a package.";
const DOC_SF_PROCESSES_VERBOSE: &str = "Example:\n
                                        \t- for a machine: (processes machine0) \n\
                                        \t- for a package: (processes package0)";

const LAMBDA_PROGRESS_RATE: &str =  "(define progress (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote progress_rate) x))))";

const SF_PROGRESS_RATE: &str = "progress";
const DOC_SF_PROGRESS_RATE: &str = "Return the progress rate (float) in [0;1] of a machine. If no task is in progress, the value is 0";
const DOC_SF_PROGRESS_RATE_VERBOSE: &str = "Example: (progress machine0)";

const LAMBDA_LOCATION: &str = "(define location (lambda (x)\
                                                  (get-map (get-state dynamic) ((quote location) x))))";

const SF_LOCATION: &str = "location";
const DOC_SF_LOCATION: &str = "Return the location (symbol) of a package.";
const DOC_SF_LOCATION_VERBOSE: &str = "Example: (location package0)";

const LAMBDA_BELT_TYPE: &str = "(define belt_type (lambda (x)\
                                                    (get-map (get-state dynamic) ((quote belt_type) x)))))";

const SF_BELT_TYPE: &str = "belt_type";
const DOC_SF_BELT_TYPE: &str = "Return the belt type (symbol) in {input, output} of a belt.";
const DOC_SF_BELT_TYPE_VERBOSE: &str = "Example: (belt_type belt0)";

const LAMBDA_POLYGONS: &str = "(define polygon (lambda (x)\
                                                    (get-map (get-state dynamic) ((quote polygon) x)))))";

const SF_POLYGONS: &str = "polygons";
const DOC_SF_POLYGONS: &str =
    "Return the coordinates of the polygon [(float float)] that represent the parking area";
const DOC_SF_POLYGONS_VERBOSE: &str = "Example: (polygons parking_area0)"; //TODO: check the name of the parking are

const LAMBDA_PACKAGES_LIST: &str = "(define packages_list (lambda (x)\
                                                (get-map (get-state dynamic) ((quote packages_list) x)))))";

const SF_PACKAGES_LIST: &str = "packages_list";
const DOC_SF_PACKAGES_LIST: &str = "Return the package list [symbol] on a belt.";
const DOC_SF_PACKAGES_LIST_VERBOSE: &str = "Example: (location package0)";

//Constants

//Documentation
const DOC_MOD_GODOT: &str = "Module to use the simulator developped in godot. It add functions and lambda for the state variables";
const DOC_MOD_GODOT_VERBOSE: &str = "functions:\n\
                                     \t- open-com-godot\n\
                                     \t- launch-godot (not implemented yet)\n\
                                     \t- exec-godot\n\n\
                                     lambdas for the state functions:\n\
                                     \t- for a robot: coordinates, battery, rotation, velocity, rotation_speed, in_station, in_interact\n\
                                     \t- for a machine: coordinates, input_belt, output_belt, processes, progress_rate\n\
                                     \t- for a package: location, processes\n\
                                     \t- for a belt: coordinates, belt_type, polygons, packages_list\n\
                                     \t- for a parking are: polygon";

const DOC_OPEN_COM: &str = "Connect via TCP to the simulator.";
const DOC_OPEN_COM_VERBOSE: &str = "Default socket address is 127.0.0.1:10000, but you can provide the IP address and the port that you want.";
const DOC_LAUNCH_GODOT: &str = "Not yet implemented";
const DOC_EXEC_GODOT: &str = "Send a command to the simulator";
const DOC_EXEC_GODOT_VERBOSE: &str = "Available commands:\n\
                                       \t- Navigate to : [navigate_to', robot_name, destination_x, destination_y] \n\
                                       \t- Pick : ['pickup', robot_name] \n\
                                       \t- Place : ['place', robot_name] \n\
                                       \t- Rotation : ['do_rotation', robot_name, angle, speed] \n\n\
                                       Example: (exec-godot navigate_to robot0 2 3)";

#[derive(Default)]
pub struct SocketInfo {
    pub addr: String,
    pub port: usize,
}

#[derive(Default)]
pub struct CtxGodot {
    socket_info: SocketInfo,
    sender_li: Option<Sender<String>>,
    sender_socket: Option<Sender<String>>,
}

impl CtxGodot {
    pub fn set_sender_li(&mut self, sender: Sender<String>) {
        self.sender_li = Some(sender);
    }

    pub fn set_socket_info(&mut self, socket_info: SocketInfo) {
        self.socket_info = socket_info;
    }

    pub fn get_sender_li(&self) -> &Option<Sender<String>> {
        &self.sender_li
    }

    pub fn get_socket_info(&self) -> &SocketInfo {
        &self.socket_info
    }

    pub fn get_sender_socket(&self) -> &Option<Sender<String>> {
        &self.sender_socket
    }
}

impl GetModule for CtxGodot {
    fn get_module(self) -> Module {
        let raw_lisp = vec![
            LAMBDA_BATTERY,
            LAMBDA_COORDINATES,
            LAMBDA_IN_INTERACT,
            LAMBDA_IN_STATION,
            LAMBDA_ROTATION,
            LAMBDA_ROTATION_SPEED,
            LAMBDA_VELOCITY,
            LAMBDA_BELT_TYPE,
            LAMBDA_PACKAGES_LIST,
            LAMBDA_POLYGONS,
            LAMBDA_LOCATION,
            LAMBDA_PROGRESS_RATE,
            LAMBDA_PROCESSES,
            LAMBDA_OUTPUT_BELT,
            LAMBDA_INPUT_BELT,
        ]
        .into();

        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            raw_lisp,
            label: MOD_GODOT,
        };

        module.add_mut_fn_prelude(OPEN_COM, Box::new(open_com));
        module.add_fn_prelude(LAUNCH_GODOT, Box::new(launch_godot));
        module.add_fn_prelude(EXEC_GODOT, Box::new(exec_godot));

        module
    }
}

impl Documentation for CtxGodot {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_GODOT, DOC_MOD_GODOT, Some(DOC_MOD_GODOT_VERBOSE)),
            LHelp::new(OPEN_COM, DOC_OPEN_COM, Some(DOC_OPEN_COM_VERBOSE)),
            LHelp::new(LAUNCH_GODOT, DOC_LAUNCH_GODOT, None),
            LHelp::new(EXEC_GODOT, DOC_EXEC_GODOT, Some(DOC_EXEC_GODOT_VERBOSE)),
            //Add doc for state functions
            LHelp::new(SF_BATTERY, DOC_SF_BATTERY, Some(DOC_SF_BATTERY_VERBOSE)),
            LHelp::new(
                SF_BELT_TYPE,
                DOC_SF_BELT_TYPE,
                Some(DOC_SF_BELT_TYPE_VERBOSE),
            ),
            LHelp::new(
                SF_COORDINATES,
                DOC_SF_COORDINATES,
                Some(DOC_SF_COORDINATES_VERBOSE),
            ),
            LHelp::new(
                SF_IN_INTERACT,
                DOC_SF_IN_INTERACT,
                Some(DOC_SF_IN_INTERACT_VERBOSE),
            ),
            LHelp::new(
                SF_IN_STATION,
                DOC_SF_IN_STATION,
                Some(DOC_SF_IN_STATION_VERBOSE),
            ),
            LHelp::new(
                SF_INPUT_BELT,
                DOC_SF_INPUT_BELT,
                Some(DOC_SF_INPUT_BELT_VERBOSE),
            ),
            LHelp::new(SF_LOCATION, DOC_SF_LOCATION, Some(DOC_SF_LOCATION_VERBOSE)),
            LHelp::new(
                SF_OUTPUT_BELT,
                DOC_SF_OUTPUT_BELT,
                Some(DOC_SF_OUTPUT_BELT_VERBOSE),
            ),
            LHelp::new(
                SF_PACKAGES_LIST,
                DOC_SF_PACKAGES_LIST,
                Some(DOC_SF_PACKAGES_LIST_VERBOSE),
            ),
            LHelp::new(SF_POLYGONS, DOC_SF_POLYGONS, Some(DOC_SF_POLYGONS_VERBOSE)),
            LHelp::new(
                SF_PROCESSES,
                DOC_SF_PROCESSES,
                Some(DOC_SF_PROCESSES_VERBOSE),
            ),
            LHelp::new(
                SF_PROGRESS_RATE,
                DOC_SF_PROGRESS_RATE,
                Some(DOC_SF_PROGRESS_RATE_VERBOSE),
            ),
            LHelp::new(SF_ROTATION, DOC_SF_ROTATION, Some(DOC_SF_ROTATION_VERBOSE)),
            LHelp::new(
                SF_ROTATION_SPEED,
                DOC_SF_ROTATION_SPEED,
                Some(DOC_SF_ROTATION_SPEED_VERBOSE),
            ),
            LHelp::new(SF_VELOCITY, DOC_SF_VELOCITY, Some(DOC_SF_VELOCITY_VERBOSE)),
        ]
    }
}

/*
Functions
 */

//const NAME_TASK_TCP_SOCKET_GODOT: &str = "thread_tcp_connection_godot";

async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    receiver: Receiver<String>,
    sender: Sender<String>,
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    let (rd, wr) = io::split(stream);

    tokio::spawn(async move { async_read_socket(rd, sender).await });

    tokio::spawn(async move { async_send_socket(wr, receiver).await });
}

async fn async_send_socket(mut stream: WriteHalf<TcpStream>, mut receiver: Receiver<String>) {
    loop {
        let command = receiver.recv().await.unwrap();
        let size = u32_to_u8_array(command.len() as u32);
        let msg: &[u8] = &[&size[0..4], &command.as_bytes()].concat();
        match stream.write_all(msg).await {
            Ok(_) => {}
            Err(_) => panic!("error sending via socket"),
        }
    }
}

fn u32_to_u8_array(x: u32) -> [u8; 4] {
    let b1: u8 = ((x >> 24) & 0xff) as u8;
    let b2: u8 = ((x >> 16) & 0xff) as u8;
    let b3: u8 = ((x >> 8) & 0xff) as u8;
    let b4: u8 = (x & 0xff) as u8;

    [b4, b3, b2, b1]
}

async fn async_read_socket(stream: ReadHalf<TcpStream>, sender: Sender<String>) {
    let mut buf_reader = BufReader::new(stream);

    let mut buf = [0; BUFFER_SIZE];
    let mut size_buf = [0; 4];

    loop {
        match buf_reader.read_exact(&mut size_buf).await {
            Ok(_) => {}
            Err(_) => panic!("Error while reading buffer"),
        };
        let size = read_size_from_buf(&size_buf);
        match buf_reader.read_exact(&mut buf[0..size]).await {
            Ok(_) => {}
            Err(_) => panic!("Error while reading buffer"),
        };

        let msg = read_msg_from_buf(&buf, size);

        if !msg.is_empty() {
            let gs: GodotState = serde_json::from_str(&msg).unwrap();
            let state_type = match gs._type {
                GodotMessageType::Static => "static",
                GodotMessageType::Dynamic => "dynamic",
                _ => panic!("should not receive robot_command"),
            };
            let lisp = gs.transform_data_into_lisp().unwrap();
            let sender_temp = sender.clone();
            tokio::spawn(async move {
                sender_temp
                    .send(format!("(update-state {} {})", state_type, lisp))
                    .await
                    .expect("could not send via channel");
            });
        }
    }
}

fn open_com(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxGodot) -> Result<LValue, LError> {
    let socket_addr: SocketAddr = match args.len() {
        0 => "127.0.0.1:10000".parse().unwrap(),
        2 => {
            let addr = match &args[0] {
                LValue::Symbol(s) => s.clone(),
                lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
            };

            let port: usize = match &args[1] {
                LValue::Number(n) => n.into(),
                lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Usize)),
            };

            format!("{}:{}", addr, port).parse().unwrap()
        }
        _ => return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2)),
    };

    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    ctx.sender_socket = Some(tx);
    let sender = match ctx.get_sender_li() {
        None => {
            return Err(SpecialError(
                "ctx godot has no sender to lisp interpreter".to_string(),
            ))
        }
        Some(s) => s.clone(),
    };

    tokio::spawn(async move { task_tcp_connection(&socket_addr, rx, sender).await });

    Ok(LValue::Nil)
}

fn launch_godot(_: &[LValue], _: &RefLEnv, ctx: &CtxGodot) -> Result<LValue, LError> {
    let sender = match ctx.get_sender_li() {
        None => return Err(SpecialError("ctx godot has no sender to l.i.".to_string())),
        Some(s) => s.clone(),
    };
    tokio::spawn(async move {
        sender
            .send("(print (quote (launch-godot not implemented yet)))".to_string())
            .await
            .expect("couldn't send via channel");
    });
    Ok(LValue::Nil)
}
/// Commands available
///- Navigate to : ['navigate_to', robot_name, destination_x, destination_y]
///- Pick : ['pickup', robot_name]
///- Place : ['place', robot_name]
///- Rotation : ['do_rotation', robot_name, angle, speed]

fn exec_godot(args: &[LValue], _: &RefLEnv, ctx: &CtxGodot) -> Result<LValue, LError> {
    let gs = GodotState {
        _type: GodotMessageType::RobotCommand,
        data: args.into(),
    };

    let command = serde_json::to_string(&gs).unwrap();

    let sender = match ctx.get_sender_socket() {
        None => {
            return Err(SpecialError(
                "ctx godot has no sender to simulation, try first to (open-com-godot)".to_string(),
            ))
        }
        Some(s) => s.clone(),
    };
    tokio::spawn(async move {
        sender
            .send(command)
            .await
            .expect("couldn't send via channel");
    });
    Ok(LValue::Nil)
}
