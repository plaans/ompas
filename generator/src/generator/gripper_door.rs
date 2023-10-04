use crate::config::Recipe;
use crate::generator::gripper::GripperTask::Place;
use crate::generator::gripper::Object::{Ball, Robby};
use crate::generator::gripper::{GripperTask, Room, AT_ROBBY, BALL, POS, ROOM};
use crate::{Generator, Problem, Task};
use petgraph::graph::NodeIndex;
use petgraph::Graph;
use rand::prelude::IteratorRandom;
use rand::Rng;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};

const DOOR: &str = "door";

const CONNECTS: &str = "connects";
const DOOR_STATUS: &str = "door_status";

#[derive(Default)]
pub struct GripperDoorGenerator {}

impl Generator for GripperDoorGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(GripperDoorProblem::generate(recipe).map(|p| Box::new(p))?)
    }
}

#[derive(Default)]
pub struct GripperDoorProblem {
    tasks: Vec<GripperTask>,
    //Topology
    graph: Graph<Room, Door>,
}

pub struct Door {
    id: u32,
    status: DoorStatus,
}

impl Door {
    pub fn get_label(&self) -> String {
        format!("{}_{}", DOOR, self.id)
    }
}

pub enum DoorStatus {
    Opened,
    Closed,
}

impl Display for DoorStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                DoorStatus::Opened => "opened",
                DoorStatus::Closed => "closed",
            }
        )
    }
}

pub struct GripperDoorConfig {
    pub n_ball: u32,
    pub n_room: u32,
    pub n_task: u32,
    pub max_distance: u32,
    pub max_connected: u32,
}

impl TryFrom<Recipe> for GripperDoorConfig {
    type Error = String;

    fn try_from(value: Recipe) -> Result<Self, Self::Error> {
        let n_ball = *value
            .get("ball")
            .ok_or_else(|| "ball is undefined".to_string())?;
        let n_room = *value
            .get("room")
            .ok_or_else(|| "room is undefined".to_string())?;
        let n_task = *value
            .get("task")
            .ok_or_else(|| "task is undefined".to_string())?;

        let max_distance = *value
            .get("max-distance")
            .ok_or_else(|| "max-distance is undefined".to_string())?;
        let max_connected = *value
            .get("max-connected")
            .ok_or_else(|| "max-connected is undefined".to_string())?;

        Ok(Self::new(
            n_ball,
            n_room,
            n_task,
            max_distance,
            max_connected,
        ))
    }
}

impl GripperDoorConfig {
    pub fn new(
        n_ball: u32,
        n_room: u32,
        n_task: u32,
        max_distance: u32,
        max_connected: u32,
    ) -> Self {
        Self {
            n_ball,
            n_room,
            n_task,
            max_distance,
            max_connected,
        }
    }
}

impl Problem for GripperDoorProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String> {
        let mut next_door_id = 0;
        let config: GripperDoorConfig = recipe.clone().try_into()?;

        let mut pb = Self::default();
        let rg = &mut rand::thread_rng();
        // Declaration of the rooms
        for i in 0..config.n_room {
            pb.graph.add_node(Room::new(i));
        }

        let indices: Vec<NodeIndex> = pb.graph.node_indices().into_iter().collect();

        for r in pb.graph.node_indices() {
            let other = indices.iter().filter(|&&i| i != r).choose(rg).unwrap();
            let id = next_door_id;
            next_door_id += 1;
            pb.graph.add_edge(
                r,
                *other,
                Door {
                    id,
                    status: match rg.gen_range(0..=1) {
                        0 => DoorStatus::Opened,
                        1 => DoorStatus::Closed,
                        _ => panic!(),
                    },
                },
            );
        }

        // Check that constraints are satisfied
        /*loop {
            match floyd_warshall(&pb.graph, 1) {
                Ok(map) => {

                }
                Err(e) => panic!("{}", e),
            }
        }*/

        pb.graph
            .node_weights_mut()
            .choose(rg)
            .unwrap()
            .contains
            .push(Robby);

        // Declaration of the balls
        for i in 0..config.n_ball {
            pb.graph
                .node_weights_mut()
                .choose(rg)
                .unwrap()
                .contains
                .push(Ball(i));
        }

        let mut available: HashSet<u32> = (0..config.n_ball).collect();
        // Declaration of the
        for _ in 0..config.n_task {
            let ball = available.iter().choose(rg);
            if let Some(&ball) = ball {
                available.remove(&ball);
                let room = pb.graph.node_weights().choose(rg).unwrap().id;
                pb.tasks.push(Place(ball, room))
            } else {
                break;
            }
        }
        Ok(pb)
    }

    fn get_objects(&self) -> Vec<(String, Vec<String>)> {
        let mut balls = vec![];
        let mut rooms = vec![];
        let mut doors = vec![];

        for door in self.graph.edge_weights() {
            doors.push(door.get_label())
        }

        for node in self.graph.node_weights() {
            rooms.push(node.get_label());
            for o in &node.contains {
                if let Ball(_) = o {
                    balls.push(o.get_label())
                }
            }
        }

        vec![
            (BALL.to_string(), balls),
            (ROOM.to_string(), rooms),
            (DOOR.to_string(), doors),
        ]
    }

    fn get_tasks(&self) -> Vec<Task> {
        self.tasks
            .iter()
            .map(|t| match t {
                Place(b, r) => {
                    vec![
                        "place".to_string(),
                        Ball(*b).get_label(),
                        Room::new(*r).get_label(),
                    ]
                }
            })
            .collect()
    }

    fn get_dynamic_facts(&self) -> Vec<(LValue, LValue)> {
        let mut facts = vec![];
        for node in self.graph.node_weights() {
            let room_lv: LValue = node.get_label().into();
            for o in &node.contains {
                match o {
                    Robby => facts.push((list![AT_ROBBY.into()], room_lv.clone())),
                    Ball(_) => {
                        facts.push((list!(POS.into(), o.get_label().into()), room_lv.clone()))
                    }
                }
            }
        }

        for door in self.graph.edge_weights() {
            facts.push((
                list!(DOOR_STATUS.into(), door.get_label().into()),
                door.status.to_string().into(),
            ));
        }

        facts
    }

    fn get_static_facts(&self) -> Vec<(LValue, LValue)> {
        let mut facts = vec![];
        for id in self.graph.edge_indices() {
            let (start, end) = self.graph.edge_endpoints(id).unwrap();
            let door = self.graph.edge_weight(id).unwrap();
            facts.push((
                list!(
                    CONNECTS.into(),
                    self.graph.node_weight(start).unwrap().get_label().into(),
                    self.graph.node_weight(end).unwrap().get_label().into()
                ),
                door.get_label().into(),
            ));
        }
        facts
    }
}
