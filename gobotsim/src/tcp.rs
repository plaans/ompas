use crate::serde::{
    GodotMessageSerde, GodotMessageSerdeData, GodotMessageType, SerdeCancelRequest, SerdeCommand,
};
use crate::PROCESS_TOPIC_GOBOT_SIM;
use ompas_middleware::ProcessInterface;
use ompas_rae_interface::platform_interface::command_request::Request;
use ompas_rae_interface::platform_interface::{
    Atom, CommandAccepted, CommandCancelRequest, CommandCancelled, CommandExecutionRequest,
    CommandProgress, CommandRejected, CommandRequest, CommandResponse, CommandResult, Instance,
    PlatformUpdate, StateUpdate, StateVariable, StateVariableType,
};
use ompas_rae_interface::{LOG_TOPIC_PLATFORM, PROCESS_TOPIC_PLATFORM};
use ompas_rae_structs::state::partial_state::PartialState;
use sompas_structs::lvalues::LValueS;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::net::SocketAddr;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt, BufReader, ReadHalf, WriteHalf};
use tokio::net::TcpStream;
use tokio::sync::mpsc::Receiver;
use tokio::sync::{broadcast, mpsc};

pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment

pub const TEST_TCP: &str = "test_tcp";
const PROCESS_GOBOT_READ_TCP: &str = "__PROCESS_GOBOT_READ_TCP__";
const PROCESS_GOBOT_WRITE_TCP: &str = "__PROCESS_GOBOT_WRITE_TCP__";

/// Opens the tcp connection with godot
pub async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    command_request_receiver: mpsc::Receiver<CommandRequest>,
    command_response_sender: broadcast::Sender<CommandResponse>,
    state_update_sender: broadcast::Sender<PlatformUpdate>,
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    // splits the tcp connection into a read and write stream.
    let (rd, wr) = io::split(stream);

    // Starts the task to read data from socket.
    tokio::spawn(async move {
        async_read_socket(rd, command_response_sender, state_update_sender).await
    });

    // Starts the task that awaits on data from inner process, and sends it to godot via tcp.
    tokio::spawn(async move { async_write_socket(wr, command_request_receiver).await });
}

async fn async_write_socket(
    mut stream: WriteHalf<TcpStream>,
    mut receiver: Receiver<CommandRequest>,
) {
    let mut process = ProcessInterface::new(
        PROCESS_GOBOT_WRITE_TCP,
        PROCESS_TOPIC_GOBOT_SIM,
        LOG_TOPIC_PLATFORM,
    )
    .await;

    loop {
        tokio::select! {
            _ = process.recv() => {
                //println!("godot sender task ended");
                break;
            }
            command = receiver.recv() => {
                //println!("[GodotTCP] new command request");
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
    let gs = GodotMessageSerde {
        _type: GodotMessageType::CancelRequest,
        data: GodotMessageSerdeData::ActionId(SerdeCancelRequest {
            action_id: cancel.command_id as usize,
        }),
    };

    //println!("action status created");

    let command = serde_json::to_string(&gs).unwrap();
    command
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
        data: GodotMessageSerdeData::RobotCommand(SerdeCommand {
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
    state_update_sender: broadcast::Sender<PlatformUpdate>,
) {
    let mut process = ProcessInterface::new(
        PROCESS_GOBOT_READ_TCP,
        PROCESS_TOPIC_GOBOT_SIM,
        LOG_TOPIC_PLATFORM,
    )
    .await;

    let mut buf_reader = BufReader::new(stream);

    let mut buf = [0; BUFFER_SIZE];
    let mut size_buf = [0; 4];

    let mut map_server_id_action_id: im::HashMap<usize, usize> = Default::default();

    'outer: loop {
        //reading an incoming message from the tcp server of godot
        tokio::select! {
            _ = process.recv() => {
                break 'outer ;//process.die().await;
            }
            msg = buf_reader.read_exact(&mut size_buf) => {

                match msg {
                    Ok(_) => {}
                    Err(_) => {
                        process.kill(PROCESS_TOPIC_PLATFORM).await;
                    }//panic!("Error while reading buffer"),
                };
                let size = read_size_from_buf(&size_buf);
                match buf_reader.read_exact(&mut buf[0..size]).await {
                    Ok(_) => {}
                    Err(_) => {
                        process.kill(PROCESS_TOPIC_PLATFORM).await;
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

                                                let state_function = state_function.to_string();
                                                if state_function.contains(".instance") {
                                                    let instance = Instance {
                                                        r#type: v.to_string(),
                                                        object: parameters[0].to_string()
                                                    };
                                                    if let Err(_) = state_update_sender.send(instance.into())
                                                    {
                                                        process.kill(PROCESS_TOPIC_PLATFORM).await;
                                                        //process.die().await;
                                                        break 'outer;
                                                    }
                                         };

                                                state_variables.push(StateVariable {
                                                    r#type: StateVariableType::Static.into(),
                                                    state_function,
                                                    parameters,
                                                    value: Some(v.clone().try_into().unwrap())
                                                })
                                            }
                                    _=> todo!()
                                        }
                                    }

                                    if let Err(_) = state_update_sender.send(StateUpdate {
                                        state_variables
                                    }.into())
                                    {
                                        process.kill(PROCESS_TOPIC_PLATFORM).await;
                                        //process.die().await;
                                        break 'outer;
                                    }
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
                                            r#type: StateVariableType::Dynamic.into(),
                                            state_function: state_function.to_string(),
                                            parameters,
                                            value: Some(v.clone().try_into().unwrap())
                                        })
                                            }
                                    _ => todo!()
                                        }
                                    }
                                    if let Err(_) = state_update_sender.send(StateUpdate {
                                        state_variables
                                    }.into())
                                    {
                                        process.kill(PROCESS_TOPIC_PLATFORM).await;
                                        //process.die().await;
                                        break 'outer;
                                    }
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
                        GodotMessageType::ActionResponse => {
                            if let GodotMessageSerdeData::ActionResponse(ar) = message.data {
                                match ar.action_id {
                                    -1 => {
                                        if let Err(_) = command_response_sender.send(CommandRejected {
                                                command_id : ar.temp_id as u64
                                            }.into()) {
                                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                                            //process.die().await;
                                            break 'outer;
                                        }
                                    }
                                    i => {
                                        if i < 0 {
                                            /*return Err(lruntimeerror!(
                                                "GodotMessageSerde",
                                                "action response is not in {-1} + N"
                                            ));*/
                                        } else {
                                            map_server_id_action_id.insert(ar.action_id as usize, ar.temp_id);
                                            if let Err(_) = command_response_sender.send(CommandAccepted {
                                                command_id : ar.temp_id as u64
                                            }.into())
                                             {
                                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                                            //process.die().await;
                                            break 'outer;
                                        }
                                        }
                                    }
                                };
                            } else {
                                unreachable!("{:?} and expected ActionResponse", message.data)
                            }
                        }
                        GodotMessageType::ActionFeedback => {
                            if let GodotMessageSerdeData::ActionFeedback(af) = message.data {
                                let command_id = *map_server_id_action_id.get(&af.action_id).expect("") as u64;
                                if let Err(_) = command_response_sender.send(CommandProgress {
                                    command_id,
                                    progress: af.feedback
                                }.into()) {
                                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                                            //process.die().await;
                                            break 'outer;
                                        }
                            } else {
                                unreachable!("{:?} and expected ActionFeedback", message.data)
                            }
                        }
                        GodotMessageType::ActionResult => {
                            if let GodotMessageSerdeData::ActionResult(ar) = message.data {
                                let command_id = *map_server_id_action_id.get(&ar.action_id).expect("") as u64;
                                if let Err(_) = command_response_sender.send(CommandResult {
                                    command_id,
                                    result: ar.result
                                }.into())  {
                                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                                            //process.die().await;
                                            break 'outer;
                                        }
                            } else {
                                unreachable!("{:?} and expected ActionResult", message.data)
                            }
                        }
                        GodotMessageType::ActionPreempt => {
                            unreachable!("{:?}: preempt is not handled", message.data)
                            /*if let GodotMessageSerdeData::ActionId(ai) = message.data {
                                let command_id = map_server_id_action_id.get(ar.action_id).expect("");
                                command_response_sender.send( {
                                    command_id,
                                    result: ar.result
                                }.into())
                            } else {
                                unreachable!("{:?} and expected ActionId", message.data)
                            }*/
                        }
                        GodotMessageType::ActionCancel => {
                            if let GodotMessageSerdeData::ActionCancel(ac) = message.data {
                                let command_id = *map_server_id_action_id.get(&ac.action_id).expect("") as u64;
                                if let Err(_) = command_response_sender.send(CommandCancelled {
                                    command_id,
                                    result: ac.cancelled
                                }.into())  {
                                            process.kill(PROCESS_TOPIC_PLATFORM).await;
                                            //process.die().await;
                                            break 'outer;
                                        }
                            } else {
                                unreachable!("{:?} and expected ActionCancel", message.data)
                            }
                        }
                        _ => {
                            unreachable!()
                        }
                        /*GodotMessageType::ActionResponse => {
                            match command_status {
                                CommandStatus::Accepted => {
                                    map_server_id_action_id.insert(server_id, godot_id);
                                }
                                CommandStatus::Rejected => {}
                                _ => unreachable!()
                            }
                        }*/
                        /*

                        GodotMessageType::ActionFeedback
                        | GodotMessageType::ActionResult
                        | GodotMessageType::ActionPreempt
                        | GodotMessageType::ActionCancel => {
                            //println!("the action status is updated");
                            let action_status: (usize, CommandStatus) = message.try_into().unwrap();
                            let id = map_server_id_action_id.get(&action_status.0).unwrap();
                            agenda.update_status(id, action_status.1.into()).await;
                        }
                        _ => panic!("should not receive this kind of message"),*/
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
