use crate::serde::{
    GodotMessageSerde, GodotMessageSerdeData, GodotMessageType, SerdeCancelRequest, SerdeCommand,
};
use ompas_core::ompas::manager::state::partial_state::PartialState;
use ompas_interface::platform_interface::command_request::Request;
use ompas_interface::platform_interface::{
    Atom, CommandAccepted, CommandCancelRequest, CommandExecutionRequest, CommandProgress,
    CommandRejected, CommandRequest, CommandResponse, CommandResult, Instance, PlatformUpdate,
    Resource, StateUpdate, StateVariable, StateVariableType,
};
use ompas_interface::platform_interface::{CommandCancelled, ResourceKind};
use ompas_language::interface::*;
use ompas_language::process::PROCESS_TOPIC_OMPAS;
use ompas_middleware::ProcessInterface;
use sompas_structs::lvalues::LValueS;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::net::SocketAddr;
use std::process::exit;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt, BufReader, ReadHalf, WriteHalf};
use tokio::net::TcpStream;
use tokio::sync::broadcast;
use tokio::sync::mpsc::UnboundedReceiver;

pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment

pub const TEST_TCP: &str = "test_tcp";
const PROCESS_GOBOT_READ_TCP: &str = "__PROCESS_GOBOT_READ_TCP__";
const PROCESS_GOBOT_WRITE_TCP: &str = "__PROCESS_GOBOT_WRITE_TCP__";

/// Opens the tcp connection with godot
pub async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    command_request_receiver: UnboundedReceiver<CommandRequest>,
    command_response_sender: broadcast::Sender<CommandResponse>,
    state_update_sender: broadcast::Sender<PlatformUpdate>,
) {
    let stream = match TcpStream::connect(socket_addr).await {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error opening tcp connection with gobot-sim : {e:?}");
            exit(0);
        }
    };

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
    mut receiver: UnboundedReceiver<CommandRequest>,
) {
    let mut process = ProcessInterface::new(
        PROCESS_GOBOT_WRITE_TCP,
        PROCESS_TOPIC_OMPAS,
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

    serde_json::to_string(&gs).unwrap()
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

    serde_json::to_string(&gs).unwrap()
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
        PROCESS_TOPIC_OMPAS,
        LOG_TOPIC_PLATFORM,
    )
    .await;

    //let mut global = Global::default();
    let mut global_2 = Global2::default();

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
                        process.kill(PROCESS_TOPIC_OMPAS);
                    }//panic!("Error while reading buffer"),
                };
                let size = read_size_from_buf(&size_buf);
                match buf_reader.read_exact(&mut buf[0..size]).await {
                    Ok(_) => {}
                    Err(_) => {
                        process.kill(PROCESS_TOPIC_OMPAS);
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

                            //let updates = post_process_state(temps_state, StateVariableType::Static, &mut global);
                            let updates = post_process_state(temps_state, StateVariableType::Static, &mut global_2);
                            for update in updates {
                                if state_update_sender.send(update).is_err()
                                {
                                    process.kill(PROCESS_TOPIC_OMPAS);
                                    break 'outer;
                                }
                            }
                        }
                        GodotMessageType::DynamicState => {
                            let temps_state: PartialState = PartialState::try_from(message).unwrap();
                            //let updates = post_process_state(temps_state, StateVariableType::Dynamic, &mut global);
                            let updates = post_process_state(temps_state, StateVariableType::Dynamic, &mut global_2);
                            for update in updates {
                                if state_update_sender.send(update).is_err()
                                {
                                    process.kill(PROCESS_TOPIC_OMPAS);
                                    break 'outer;
                                }
                            }
                        },
                        GodotMessageType::ActionResponse => {
                            if let GodotMessageSerdeData::ActionResponse(ar) = message.data {
                                match ar.action_id {
                                    -1 => {
                                        if command_response_sender.send(CommandRejected {
                                                command_id : ar.temp_id as u64
                                            }.into()).is_err() {
                                            process.kill(PROCESS_TOPIC_OMPAS);
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
                                            if command_response_sender.send(CommandAccepted {
                                                command_id : ar.temp_id as u64
                                            }.into()).is_err()
                                             {
                                            process.kill(PROCESS_TOPIC_OMPAS);
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
                                if command_response_sender.send(CommandProgress {
                                    command_id,
                                    progress: af.feedback
                                }.into()).is_err() {
                                            process.kill(PROCESS_TOPIC_OMPAS);
                                            break 'outer;
                                        }
                            } else {
                                unreachable!("{:?} and expected ActionFeedback", message.data)
                            }
                        }
                        GodotMessageType::ActionResult => {
                            if let GodotMessageSerdeData::ActionResult(ar) = message.data {
                                let command_id = *map_server_id_action_id.get(&ar.action_id).expect("") as u64;
                                if command_response_sender.send(CommandResult {
                                    command_id,
                                    result: ar.result
                                }.into()).is_err()  {
                                            process.kill(PROCESS_TOPIC_OMPAS);
                                            break 'outer;
                                        }
                            } else {
                                unreachable!("{:?} and expected ActionResult", message.data)
                            }
                        }
                        GodotMessageType::ActionPreempt => {
                            unreachable!("{:?}: preempt is not handled", message.data)
                        }
                        GodotMessageType::ActionCancel => {
                            if let GodotMessageSerdeData::ActionCancel(ac) = message.data {
                                let command_id = *map_server_id_action_id.get(&ac.action_id).expect("") as u64;
                                if command_response_sender.send(CommandCancelled {
                                    command_id,
                                    result: ac.cancelled
                                }.into()).is_err()  {
                                            process.kill(PROCESS_TOPIC_OMPAS);
                                            break 'outer;
                                        }
                            } else {
                                unreachable!("{:?} and expected ActionCancel", message.data)
                            }
                        }
                        _ => {
                            unreachable!()
                        }
                    }
                }

            }
        }
    }
}

const TILE_PREFIX: &str = "tile";

#[derive(Copy, Eq, Hash, PartialEq, Clone)]
struct Tile {
    x: i64,
    y: i64,
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}_{}", TILE_PREFIX, self.x, self.y)
    }
}

#[derive(Default)]
struct Global2 {
    def_velocity: Option<f64>,
    locations: HashMap<String, Tile>,
    exported: bool,
}

const TRAVEL_TIME: &str = "travel-time";

fn post_process_state(
    state: PartialState,
    r#type: StateVariableType,
    global: &mut Global2,
) -> Vec<PlatformUpdate> {
    let mut updates = vec![];

    fn find_median_tile(cells: Vec<Tile>) -> Tile {
        //computation of the geometric center
        let mut median_x = 0;
        let mut median_y = 0;

        for cell in cells.iter() {
            median_x += cell.x;
            median_y += cell.y;
        }

        median_x /= cells.len() as i64;
        median_y /= cells.len() as i64;

        //finding the closest point;
        let mut shortest_distance = None;
        let mut closest_cell = None;
        for cell in cells.iter() {
            let d: i64 = (cell.x - median_x).pow(2) + (cell.y - median_y).pow(2);
            if closest_cell.is_none() {
                closest_cell = Some(cell);
                shortest_distance = Some(d);
            } else if let Some(sd) = shortest_distance {
                if sd > d {
                    closest_cell = Some(cell);
                    shortest_distance = Some(d);
                }
            }
        }
        *closest_cell.unwrap()
    }

    match r#type {
        StateVariableType::Static => {
            if let Some(f) = state.get(&LValueS::from(vec!["globals.robot_standard_velocity"])) {
                //println!("ok");
                global.def_velocity = Some((&f.value).try_into().unwrap())
            }
            //Store velocity
        }
        StateVariableType::Dynamic => {
            //Update the location of robot: Belts and Parking Areas
        }
    }

    let mut state_variables = vec![];
    for (k, f) in &state.inner {
        match k {
            LValueS::List(list) => {
                let mut sv = list.clone();
                let state_function = sv.remove(0);
                let mut parameters: Vec<Atom> = vec![];
                for e in sv.drain(..) {
                    parameters.push(e.try_into().unwrap())
                }

                let state_function = state_function.to_string();
                if state_function.contains(".instance") {
                    let obj_label = parameters[0].to_string();
                    let instance = Instance {
                        r#type: f.value.to_string(),
                        object: obj_label.to_string(),
                    };

                    //Creation of a new instance
                    updates.push(instance.into());
                    let resource: Resource = Resource {
                        label: obj_label.to_string(),
                        resource_kind: ResourceKind::Unary.into(),
                        quantity: 0,
                    };

                    //Creation of a unary resource corresponding to the instance.
                    updates.push(resource.into());

                    if f.value.to_string() == "belt" || f.value.to_string() == "parking_area" {
                        let cells = state
                            .get(&LValueS::from(vec![
                                format!("{}.cells", f.value).into(),
                                list[1].clone(),
                            ]))
                            .unwrap();
                        let mut cells: Vec<LValueS> = (&cells.value).try_into().unwrap();
                        let tiles: Vec<Tile> = cells
                            .drain(..)
                            .map(|val| {
                                let coordinates: Vec<LValueS> = (&val).try_into().unwrap();
                                Tile {
                                    x: (&coordinates[0]).try_into().unwrap(),
                                    y: (&coordinates[1]).try_into().unwrap(),
                                }
                            })
                            .collect();
                        let tile = find_median_tile(tiles);
                        global.locations.insert(obj_label, tile);
                    }
                }

                state_variables.push(StateVariable {
                    r#type: r#type.into(),
                    state_function,
                    parameters,
                    value: Some(f.value.clone().try_into().unwrap()),
                });
            }
            _ => {
                todo!()
            }
        }
    }

    if let Some(velocity) = global.def_velocity {
        if !global.exported {
            global.exported = true;
            //Post processing to return travel-time between two positions
            for (l1, t1) in &global.locations {
                for (l2, t2) in &global.locations {
                    if l1 != l2 {
                        let time: f64 = ((t1.x - t2.x).pow(2) as f64 + (t1.y - t2.y).pow(2) as f64)
                            .sqrt()
                            / velocity;

                        state_variables.push(StateVariable {
                            r#type: StateVariableType::Static.into(),
                            state_function: TRAVEL_TIME.to_string(),
                            parameters: vec![l1.to_string().into(), l2.to_string().into()],
                            value: Some(Atom::from(time).into()),
                        });

                        state_variables.push(StateVariable {
                            r#type: StateVariableType::Static.into(),
                            state_function: TRAVEL_TIME.to_string(),
                            parameters: vec![l2.to_string().into(), l1.to_string().into()],
                            value: Some(Atom::from(time).into()),
                        });
                    }
                }
            }
        }
    }

    updates.push(StateUpdate { state_variables }.into());

    updates
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

/*

//const LOCATION_TILE: &str = "location_tile";
//const TRAVEL_DISTANCE: &str = "travel_distance";

#[derive(Default)]
struct Global {
    tiles: HashSet<Tile>,
}


fn post_process_state(
    state: PartialState,
    r#type: StateVariableType,
    global: &mut Global,
) -> Vec<PlatformUpdate> {
    let mut updates = vec![];

    fn update_tiles(
        new: Tile,
        tiles: &mut HashSet<Tile>,
        updates: &mut Vec<PlatformUpdate>,
    ) -> Vec<StateVariable> {
        let mut state_variables = vec![];

        if !tiles.contains(&new) {
            for tile in tiles.iter() {
                let distance: f64 =
                    ((new.x - tile.x).pow(2) as f64 + (new.y - tile.y).pow(2) as f64).sqrt();

                state_variables.push(StateVariable {
                    r#type: StateVariableType::Static.into(),
                    state_function: TRAVEL_DISTANCE.to_string(),
                    parameters: vec![tile.to_string().into(), new.to_string().clone().into()],
                    value: Some(Atom::from(distance).into()),
                });
                state_variables.push(StateVariable {
                    r#type: StateVariableType::Static.into(),
                    state_function: TRAVEL_DISTANCE.to_string(),
                    parameters: vec![new.to_string().into(), tile.to_string().into()],
                    value: Some(Atom::from(distance).into()),
                });
            }
            updates.push(
                Instance {
                    r#type: TILE_PREFIX.to_string(),
                    object: new.to_string(),
                }
                .into(),
            );
            tiles.insert(new);
        }

        state_variables
    }

    let mut state_variables = vec![];
    for (k, v) in &state.inner {
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
                    let obj_label = parameters[0].to_string();
                    let instance = Instance {
                        r#type: v.to_string(),
                        object: obj_label.to_string(),
                    };

                    //Creation of a new instance
                    updates.push(instance.into());
                    let resource: Resource = Resource {
                        label: obj_label.to_string(),
                        resource_kind: ResourceKind::Unary.into(),
                        quantity: 0,
                    };

                    //Creation of a unary resource corresponding to the instance.
                    updates.push(resource.into());
                };

                //Create synthetic task to represent tile position of belts, robots and machines
                if state_function.contains(".coordinates_tile") {
                    //coordinates_tile is a pair of coordinates
                    let coordinates: Vec<LValueS> = v.try_into().unwrap();
                    let tile = Tile {
                        x: (&coordinates[0]).try_into().unwrap(),
                        y: (&coordinates[1]).try_into().unwrap(),
                    };

                    let value = tile.to_string();

                    state_variables.append(&mut update_tiles(
                        tile,
                        &mut global.tiles,
                        &mut updates,
                    ));

                    state_variables.push(StateVariable {
                        r#type: r#type.into(),
                        state_function: LOCATION_TILE.to_string(),
                        parameters: parameters.clone(),
                        value: Some(LValueS::from(value).try_into().unwrap()),
                    })
                }

                //Computation of a median tile for an area
                if state_function.contains(".cells") {
                    let mut cells: Vec<LValueS> = v.try_into().unwrap();
                    let cells: Vec<(i64, i64)> = cells
                        .drain(..)
                        .map(|val| {
                            let coordinates: Vec<LValueS> = (&val).try_into().unwrap();
                            (
                                (&coordinates[0]).try_into().unwrap(),
                                (&coordinates[1]).try_into().unwrap(),
                            )
                        })
                        .collect();

                    //computation of the geometric center
                    let mut median_x = 0;
                    let mut median_y = 0;

                    for cell in cells.iter() {
                        median_x += cell.0;
                        median_y += cell.1;
                    }

                    median_x /= cells.len() as i64;
                    median_y /= cells.len() as i64;

                    //finding the closest point;
                    let mut shortest_distance = None;
                    let mut closest_cell = None;
                    for cell in cells.iter() {
                        let d: i64 = (cell.0 - median_x).pow(2) + (cell.1 - median_y).pow(2);
                        if closest_cell.is_none() {
                            closest_cell = Some(cell);
                            shortest_distance = Some(d);
                        } else if let Some(sd) = shortest_distance {
                            if sd > d {
                                closest_cell = Some(cell);
                                shortest_distance = Some(d);
                            }
                        }
                    }

                    let cell = closest_cell.unwrap();

                    let tile = Tile {
                        x: cell.0,
                        y: cell.1,
                    };

                    let value = tile.to_string();

                    state_variables.append(&mut update_tiles(
                        tile,
                        &mut global.tiles,
                        &mut updates,
                    ));

                    state_variables.push(StateVariable {
                        r#type: r#type.into(),
                        state_function: LOCATION_TILE.to_string(),
                        parameters: parameters.clone(),
                        value: Some(LValueS::from(value).try_into().unwrap()),
                    })
                }

                //Post processing to create synthetic function that return the travel-time between two positions.

                state_variables.push(StateVariable {
                    r#type: r#type.into(),
                    state_function,
                    parameters,
                    value: Some(v.clone().try_into().unwrap()),
                })
            }
            _ => {
                todo!()
            }
        }
    }

    updates.push(StateUpdate { state_variables }.into());

    updates
}*/
