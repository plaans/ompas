use tokio::sync::mpsc::{Sender, Receiver};
use tokio::sync::mpsc;

use ompas_lisp::core::RefLEnv;
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};

use crate::serde::{GodotState, GodotStateS, GodotMessageType};
use crate::tcp::{read_msg_from_buf, read_size_from_buf, TypeMessage, BUFFER_SIZE};
use tokio::net::TcpStream;
use tokio::io::{BufReader, AsyncReadExt};
use std::net::SocketAddr;
use ompas_modules::io::TOKIO_CHANNEL_SIZE;

//modules of the crate
pub mod serde;
pub mod state;
pub mod tcp;

/*
LANGUAGE
*/

const MOD_GODOT: &str = "mod-godot";

//commands

const OPEN_COM: &str = "open-com-godot";
const LAUNCH_GODOT: &str = "launch-godot";
const EXEC_GODOT: &str = "exec-godot";

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
}

impl GetModule for CtxGodot {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
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

const NAME_TASK_TCP_SOCKET_GODOT: &str = "thread_tcp_connection_godot";

async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    _receiver: Receiver<String>,
    sender: Sender<String>,
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    let mut buf_reader = BufReader::new(stream);

    let mut buf = [0; BUFFER_SIZE];
    let mut size_buf= [0;4];

    loop {
        buf_reader.read_exact(&mut size_buf).await;
        let size = read_size_from_buf(&size_buf);
        buf_reader.read_exact(&mut buf[0..size]).await;

        let msg = read_msg_from_buf(&buf, size);

        if !msg.is_empty() {
            let gs: GodotState = serde_json::from_str(&msg).unwrap();
            let state_type = match gs._type {
                GodotMessageType::Static => "static",
                GodotMessageType::Dynamic => "dynamic",
            };
            let lisp = gs.transform_data_into_lisp().unwrap();
            let sender_temp = sender.clone();
            tokio::spawn(async move {
               sender_temp
                   .send(format!("(update-state {} {})", state_type, lisp)).await
                   .expect("could not send via channel");
            });
        }
    }
}

fn open_com(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxGodot) -> Result<LValue, LError> {
    let socket_addr:SocketAddr  = match args.len() {
        0 => {
            "127.0.0.1:10000".parse().unwrap()
        }
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

    tokio::spawn(async move {
        task_tcp_connection(&socket_addr, rx, sender).await
    });

    Ok(LValue::Nil)
}

fn launch_godot(_: &[LValue], _: &RefLEnv, ctx: &CtxGodot) -> Result<LValue, LError> {
    let sender = match ctx.get_sender_li() {
        None => return Err(SpecialError("ctx godot has no sender to l.i.".to_string())),
        Some(s) => s.clone()
    };
    tokio::spawn(async move {
        sender
            .send("(print (quote (exec godot not implemented yet)))".to_string()).await
            .expect("couldn't send via channel");
    });
    Ok(LValue::Nil)
}

fn exec_godot(_: &[LValue], _: &RefLEnv, ctx: &CtxGodot) -> Result<LValue, LError> {
    let sender = match ctx.get_sender_li() {
        None => return Err(SpecialError("ctx godot has no sender to l.i.".to_string())),
        Some(s) => s.clone()
    };
    tokio::spawn(async move {
        sender
            .send("(print (quote (exec godot not implemented yet)))".to_string()).await
            .expect("couldn't send via channel");
    });
    Ok(LValue::Nil)

}
