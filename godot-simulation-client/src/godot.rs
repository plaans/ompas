use crate::state::{GodotState};
use std::convert::TryInto;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt, BufReader, ReadHalf, WriteHalf};
use tokio::net::TcpStream;
use tokio::sync::mpsc::Receiver;
use tokio::sync::Mutex;
use crate::serde::{GodotMessageSerde, GodotMessageType};
use ompas_acting::rae::state::{LState, ActionStatus, ActionStatusSet};

pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment

pub async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    receiver: Receiver<String>,
    state: Arc<Mutex<GodotState>>,
    status: Arc<Mutex<ActionStatusSet>>,
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    let (rd, wr) = io::split(stream);

    tokio::spawn(async move { async_read_socket(rd, state, status).await });

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

async fn async_read_socket(stream: ReadHalf<TcpStream>, state: Arc<Mutex<GodotState>>, status: Arc<Mutex<ActionStatusSet>>) {
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
            let message: GodotMessageSerde = serde_json::from_str(&msg.to_lowercase()).unwrap();
            match message._type {
                GodotMessageType::StaticState | GodotMessageType::DynamicState => {
                    let temp_state: LState = message.try_into().unwrap();
                    state.lock().await.set_state(temp_state);
                }
                GodotMessageType::ActionResponse => {

                    let action_status: (usize, ActionStatus) = message.try_into().unwrap();
                    //println!("{:?}", action_status.1);
                    if let ActionStatus::ActionResponse(server_id) = action_status.1 {
                        //println!("yey in action response");
                        status.lock().await.server_id_interal_id.insert(server_id, action_status.0);
                        status.lock().await.status.insert(action_status.0, action_status.1);
                    }
                }
                GodotMessageType::ActionFeedback |
                GodotMessageType::ActionResult |
                GodotMessageType::ActionPreempt |
                GodotMessageType ::ActionCancel => {
                    //println!("the action status is updated");
                    let action_status: (usize, ActionStatus) = message.try_into().unwrap();
                    status.lock().await.set_status_from_server(action_status.0, action_status.1);
                }
                _ => panic!("should not receive this kind of message")

            }

        }
    }
}

pub fn read_size_from_buf(buf: &[u8]) -> usize {
    let mut size = [0; 4];
    size.clone_from_slice(&buf[0..4]);
    u32::from_le_bytes(size) as usize
}

pub fn read_msg_from_buf(buf: &[u8], size: usize) -> String {
    String::from_utf8_lossy(&buf[0..size]).to_string()
}
