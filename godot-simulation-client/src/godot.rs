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

const LAMBDA_BATTERY: &str = "(define battery (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote battery) x))))";

const LAMBDA_ROTATION: &str = "(define rotation (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote rotation) x))))";

const LAMBDA_VELOCITY: &str = "(define velocity (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote velocity) x))))";

const LAMBDA_ROTATION_SPEED: &str = "(define rotation_speed (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote rotation_speed) x))))";

const LAMBDA_IN_STATION: &str = "(define in_station (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote in_station) x))))";

const LAMBDA_IN_INTERACT: &str = "(define in_interact (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote in_interact) x))))";

const LAMBDA_INPUT_BELT: &str = "(define input_belt (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote input_belt) x))))";

const LAMBDA_OUTPUT_BELT: &str = "(define output_belt (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote output_belt) x))))";

const LAMBDA_PROCESSES: &str = "(define processes (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote processes_list) x))))";

const LAMBDA_PROGRESS_RATE: &str =  "(define progress (lambda (x)\
                                                        (get-map (get-state dynamic) ((quote progress_rate) x))))";

const LAMBDA_LOCATION: &str = "(define location (lambda (x)\
                                                  (get-map (get-state dynamic) ((quote location) x))))";

const LAMBDA_BELT_TYPE: &str = "(define belt_type (lambda (x)\
                                                    (get-map (get-state dynamic) ((quote belt_type) x)))))";

const LAMBDA_POLYGONS: &str = "(define polygon (lambda (x)\
                                                    (get-map (get-state dynamic) ((quote polygon) x)))))";

const LAMBDA_PACKAGES_LIST: &str = "(define packages_list (lambda (x)\
                                                (get-map (get-state dynamic) ((quote packages_list) x)))))";

//TODO: doc for each lambda

//Constants

//Documentation
const DOC_MOD_GODOT: &str = "todo";
const DOC_OPEN_COM: &str = "todo";
const DOC_LAUNCH_GODOT: &str = "todo";
const DOC_EXEC_GODOT: &str = "todo";
const DOC_EXEC_GODOT_VERBOSE: &str = "todo";

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
            LHelp::new(MOD_GODOT, DOC_MOD_GODOT, None),
            LHelp::new(OPEN_COM, DOC_OPEN_COM, None),
            LHelp::new(LAUNCH_GODOT, DOC_LAUNCH_GODOT, None),
            LHelp::new(EXEC_GODOT, DOC_EXEC_GODOT, Some(DOC_EXEC_GODOT_VERBOSE)),
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
