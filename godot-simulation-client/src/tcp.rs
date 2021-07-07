use crate::serde::{GodotMessageSerde, GodotMessageType};
use ompas_acting::rae::context::ActionsProgress;
use ompas_acting::rae::state::{ActionStatus, LState, RAEState, StateType};
use ompas_utils::task_handler;
use std::convert::TryInto;
use std::net::SocketAddr;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt, BufReader, ReadHalf, WriteHalf};
use tokio::net::TcpStream;
use tokio::sync::mpsc::Receiver;

pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment

pub const TEST_TCP: &str = "test_tcp";

pub async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    receiver: Receiver<String>,
    state: RAEState,
    status: ActionsProgress,
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    let (rd, wr) = io::split(stream);

    tokio::spawn(async move { async_read_socket(rd, state, status).await });

    tokio::spawn(async move { async_send_socket(wr, receiver).await });
}

async fn async_send_socket(mut stream: WriteHalf<TcpStream>, mut receiver: Receiver<String>) {
    let test = receiver.recv().await.unwrap();
    assert_eq!(test, TEST_TCP);
    //println!("socket ready to receive command !");
    let mut end_receiver = task_handler::subscribe_new_task();
    loop {
        tokio::select! {
            command = receiver.recv() => {
                let command = match command {
                    None => break,
                    Some(s) => s
                };
                    //println!("new command to send: {}", command);
                let size = u32_to_u8_array(command.len() as u32);
                let msg: &[u8] = &[&size[0..4], &command.as_bytes()].concat();
                match stream.write_all(msg).await {
                    Ok(_) => {}
                    Err(_) => panic!("error sending via socket"),
                }
            }
            _ = end_receiver.recv() => {
                println!("godot sender task ended");
                break;
            }
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

async fn async_read_socket(stream: ReadHalf<TcpStream>, state: RAEState, status: ActionsProgress) {
    let mut buf_reader = BufReader::new(stream);

    let mut buf = [0; BUFFER_SIZE];
    let mut size_buf = [0; 4];

    let mut map_server_id_action_id: im::HashMap<usize, usize> = Default::default();

    let mut end_receiver = task_handler::subscribe_new_task();

    loop {
        tokio::select! {
            msg = buf_reader.read_exact(&mut size_buf) => {
                match msg {
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
            let message: GodotMessageSerde = match serde_json::from_str(&msg.to_lowercase()) {
                Ok(m) => m,
                Err(e) => panic!(
                    "Error while casting message in GodotMessageSerde:\n msg: {},\n error: {}",
                    msg, e
                ),
            };
            match message._type {
                GodotMessageType::StaticState | GodotMessageType::DynamicState => {
                    let temp_state: LState = message.try_into().unwrap();

                    match &temp_state._type {
                        None => panic!("state should have a type"),
                        Some(_type) => match _type {
                            StateType::Static => {
                                //println!("updating static state: {:?}", temp_state);
                                state.update_state(temp_state);
                            }
                            StateType::Dynamic => state.set_state(temp_state),
                            StateType::InnerWorld => {
                                panic!("should not receive inner world fact from godot")
                            }
                        },
                    };
                }
                GodotMessageType::ActionResponse => {
                    let action_status: (usize, ActionStatus) = message.try_into().unwrap();
                    //println!("{:?}", action_status.1);
                    if let ActionStatus::ActionResponse(server_id) = action_status.1 {
                        //println!("yey in action response");

                        map_server_id_action_id.insert(server_id, action_status.0);

                        status
                            .status
                            .write()
                            .unwrap()
                            .insert(action_status.0, action_status.1.into());
                    }
                    match &status.sync.sender {
                        None => {}
                        Some(sender) => {
                            sender
                                .send(action_status.0)
                                .await
                                .expect("fail to send to status watcher!");
                        }
                    };
                }
                GodotMessageType::ActionFeedback
                | GodotMessageType::ActionResult
                | GodotMessageType::ActionPreempt
                | GodotMessageType::ActionCancel => {
                    //println!("the action status is updated");
                    let action_status: (usize, ActionStatus) = message.try_into().unwrap();
                    let id = map_server_id_action_id.get(&action_status.0).unwrap();

                    status
                        .status
                        .write()
                        .unwrap()
                        .insert(*id, action_status.1.into());

                    match &status.sync.sender {
                        None => {}
                        Some(sender) => {
                            sender
                                .send(*id)
                                .await
                                .expect("fail to send to status watcher!");
                        }
                    };
                }
                _ => panic!("should not receive this kind of message"),
            }
        }
                }
            _ = end_receiver.recv() => {
                println!("godot tcp receiver task ended.");
                break;
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
