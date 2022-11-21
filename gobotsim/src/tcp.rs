use crate::serde::{GodotMessageSerde, GodotMessageSerdeData, GodotMessageType, SerdeRobotCommand};
use ompas_rae_interface::platform_interface::command_request::Request;
use ompas_rae_interface::platform_interface::{
    Atom, CommandCancelRequest, CommandExecutionRequest, CommandRequest, CommandResponse,
    StateUpdate, StateVariable, StateVariableType,
};
use ompas_rae_structs::state::partial_state::PartialState;
use sompas_structs::lvalues::LValueS;
use sompas_utils::task_handler::EndSignal;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::net::SocketAddr;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt, BufReader, ReadHalf, WriteHalf};
use tokio::net::TcpStream;
use tokio::sync::mpsc::Receiver;
use tokio::sync::{broadcast, mpsc};

pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment

pub const TEST_TCP: &str = "test_tcp";

/// Opens the tcp connection with godot
pub async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    command_request_receiver: mpsc::Receiver<CommandRequest>,
    command_response_sender: broadcast::Sender<CommandResponse>,
    state_update_sender: broadcast::Sender<StateUpdate>,
    killer: broadcast::Sender<EndSignal>,
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    // splits the tcp connection into a read and write stream.
    let (rd, wr) = io::split(stream);

    let k1 = killer.clone();
    // Starts the task to read data from socket.
    tokio::spawn(async move {
        async_read_socket(rd, command_response_sender, state_update_sender, k1).await
    });

    let k2 = killer.clone();

    // Starts the task that awaits on data from inner process, and sends it to godot via tcp.
    tokio::spawn(async move { async_write_socket(wr, command_request_receiver, k2).await });
}

async fn async_write_socket(
    mut stream: WriteHalf<TcpStream>,
    mut receiver: Receiver<CommandRequest>,
    killer: broadcast::Sender<EndSignal>,
) {
    let test = receiver.recv().await.unwrap();
    //assert_eq!(test, TEST_TCP);
    //println!("socket ready to receive command !");
    //let mut end_receiver = task_handler::subscribe_new_task();

    let mut killed = killer.subscribe();
    loop {
        tokio::select! {
            command = receiver.recv() => {
                let command: CommandRequest = match command {
                    None => break,
                    Some(s) => s
                };

                let command = command_request_to_string(&command);
                    //println!("new command to send: {}", command);
                let size = u32_to_u8_array(command.len() as u32);
                let msg: &[u8] = &[&size[0..4], command.as_bytes()].concat();
                match stream.write_all(msg).await {
                    Ok(_) => {}
                    Err(_) => panic!("error sending via socket"),
                }
            }
            _ = killed.recv() => {
                println!("godot sender task ended");
                break;
            }
        }
    }
}

fn command_request_to_string(request: &CommandRequest) -> String {
    match &request.request {
        Some(Request::Execution(execution)) => command_execution_to_string(execution),
        Some(Request::Cancel(cancel)) => command_cancel_to_string(cancel),
        None => panic!("request should not be empty"),
    }
}

fn command_cancel_to_string(cancel: &CommandCancelRequest) -> String {
    todo!()
}

fn command_execution_to_string(execution: &CommandExecutionRequest) -> String {
    let mut command_info: Vec<LValueS> = vec![];
    for e in &execution.arguments {
        command_info.push(e.try_into().unwrap())
    }

    let _type = if command_info[0] == "process".into() {
        GodotMessageType::MachineCommand
    } else {
        GodotMessageType::RobotCommand
    };

    let gs = GodotMessageSerde {
        _type,
        data: GodotMessageSerdeData::RobotCommand(SerdeRobotCommand {
            command_info: command_info.into(),
            temp_id: execution.command_id as usize,
        }),
    };

    //println!("action status created");

    let command = serde_json::to_string(&gs).unwrap();
    command
}

/// converts a u32 into a array of u8.
/// Used to send size of the data via tcp.
fn u32_to_u8_array(x: u32) -> [u8; 4] {
    let b1: u8 = ((x >> 24) & 0xff) as u8;
    let b2: u8 = ((x >> 16) & 0xff) as u8;
    let b3: u8 = ((x >> 8) & 0xff) as u8;
    let b4: u8 = (x & 0xff) as u8;

    [b4, b3, b2, b1]
}

async fn async_read_socket(
    stream: ReadHalf<TcpStream>,
    command_response_sender: broadcast::Sender<CommandResponse>,
    state_update_sender: broadcast::Sender<StateUpdate>,
    killer: broadcast::Sender<EndSignal>,
) {
    let mut buf_reader = BufReader::new(stream);

    let mut buf = [0; BUFFER_SIZE];
    let mut size_buf = [0; 4];

    let mut map_server_id_action_id: im::HashMap<usize, usize> = Default::default();

    let mut killed = killer.subscribe();

    loop {
        //reading an incoming message from the tcp server of godot
        tokio::select! {
            _ = killed.recv() => {
                println!("godot tcp receiver task ended.");
                break;
            }
            msg = buf_reader.read_exact(&mut size_buf) => {
                match msg {
                    Ok(_) => {}
                    Err(_) => {
                        killer.send(true).expect("could send kill message to rae processes.");
                    }//panic!("Error while reading buffer"),
                };
                let size = read_size_from_buf(&size_buf);
                match buf_reader.read_exact(&mut buf[0..size]).await {
                    Ok(_) => {}
                    Err(_) => {
                                killer.send(true).expect("could send kill message to rae processes.");
                    }
                };

                let msg = read_msg_from_buf(&buf, size);

                if !msg.is_empty() {

                            //Getting a godot message from deserialization
                    let message: GodotMessageSerde = match serde_json::from_str(&msg.to_lowercase()) {
                        Ok(m) => m,
                        Err(e) => panic!(
                            "Error while casting message in GodotMessageSerde:\n msg: {},\n error: {}",
                            msg, e
                        ),
                    };

                    match message._type {
                        GodotMessageType::StaticState => {
                            let temps_state: PartialState = PartialState::try_from(message).unwrap();
                                    let mut state_variables = vec![];
                                    for (k, v) in &temps_state.inner {
                                        match k {
                                            LValueS::List(list) => {
                                        let mut list = list.clone();
                                                let state_function = list.remove(0);
                                                let mut parameters: Vec<Atom> = vec![];
                                                for e in list.drain(..) {
                                            parameters.push(e.try_into().unwrap())
                                        }

                                                 state_variables.push(StateVariable {
                                            r#type: StateVariableType::Static.into(),
                                            state_function: state_function.to_string(),
                                            parameters,
                                            value: Some(v.clone().try_into().unwrap())
                                        })
                                            }
                                    _=> todo!()
                                        }
                                    }
                                    let state_update = StateUpdate {
                                        state_variables,
                                    };

                                    state_update_sender.send(state_update);
                        }
                        GodotMessageType::DynamicState => {
                            let temps_state: PartialState = PartialState::try_from(message).unwrap();
                            let mut state_variables = vec![];
                            for (k, v) in &temps_state.inner {
                                match k {
                                    LValueS::List(list) => {
                                        let mut list = list.clone();
                                        let state_function = list.remove(0);
                                        let mut parameters: Vec<Atom> = vec![];
                                                for e in list.drain(..) {
                                            parameters.push(e.try_into().unwrap())
                                        }

                                                 state_variables.push(StateVariable {
                                            r#type: StateVariableType::Static.into(),
                                            state_function: state_function.to_string(),
                                            parameters,
                                            value: Some(v.clone().try_into().unwrap())
                                        })
                                            }
                                    _ => todo!()
                                        }
                                    }
                                    let state_update = StateUpdate {
                                        state_variables,
                                    };

                                    state_update_sender.send(state_update);
                        },
                        /*GodotMessageType::StaticState | GodotMessageType::DynamicState => {
                            let temp_state: PartialState = message.try_into().unwrap();
                            //println!("new state");
                            for (k,v) in &temp_state.inner {
                                if let LValueS::List(list)= &k {
                                    //println!("k: {}\nv: {}", k,v);
                                    if list.len() == 2 && list[0].to_string().contains(".instance") {
                                        let instance_val = &list[1];
                                                state.add_instance(instance_val.to_string(), v.to_string()).await;
                                        //println!("add instance {} {}", instance_val, v);
                                        //instance.add_instance_of(instance_val.to_string(), v.to_string()).await;
                                    }
                                }
                            }

                            match &temp_state._type {
                                None => panic!("state should have a type"),
                                Some(_type) => match _type {
                                    StateType::Static => {
                                        //println!("updating static state: {:?}", temp_state);
                                        state.update_state(temp_state).await;
                                    }
                                    StateType::Dynamic => state.set_state(temp_state).await,
                                    StateType::InnerWorld => {
                                        panic!("should not receive inner world fact from godot")
                                    }
                                    StateType::Instance => {
                                        panic!("should not receive instance fact from godot")
                                    }
                                },
                            };
                        }*/
                        /*GodotMessageType::ActionResponse => {
                            let (godot_id, command_status): (usize, CommandStatus) = message.try_into().unwrap();
                            match command_status {
                                CommandStatus::Accepted => {
                                    map_server_id_action_id.insert(server_id, godot_id);
                                }
                                CommandStatus::Rejected => {}
                                _ => unreachable!()
                            }

                            agenda.update_status(&godot_id, action_status.into()).await;
                        }
                        GodotMessageType::ActionFeedback
                        | GodotMessageType::ActionResult
                        | GodotMessageType::ActionPreempt
                        | GodotMessageType::ActionCancel => {
                            //println!("the action status is updated");
                            let action_status: (usize, CommandStatus) = message.try_into().unwrap();
                            let id = map_server_id_action_id.get(&action_status.0).unwrap();
                            agenda.update_status(id, action_status.1.into()).await;
                        }
                        _ => panic!("should not receive this kind of message"),
                    }
                }*/
                    _ => {}
                    }
                }
            }
        }
    }
}

/// Transforms slice of u8 (received characters) into a usize giving the size of the following message.
pub fn read_size_from_buf(buf: &[u8]) -> usize {
    let mut size = [0; 4];
    size.clone_from_slice(&buf[0..4]);
    u32::from_le_bytes(size) as usize
}

/// Reads the number of character from a slice.
pub fn read_msg_from_buf(buf: &[u8], size: usize) -> String {
    String::from_utf8_lossy(&buf[0..size]).to_string()
}
