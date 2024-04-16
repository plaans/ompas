use crate::config::{GetElement, Recipe};
use crate::generator::gripper::GripperTask::Place;
use crate::generator::gripper::Object::{Ball, Robby};
use crate::generator::gripper::{
    GripperConfig, GripperTask, Object, Room, AT_ROBBY, BALL, CARRY, EMPTY, LEFT, POS, RIGHT, ROOM,
};
use crate::generator::{populate_topology, write_dot_to_file};
use crate::{Generator, Problem, Task};
use petgraph::algo::floyd_warshall;
use petgraph::dot::Dot;
use petgraph::{Graph, Undirected};
use rand::prelude::{IteratorRandom, SliceRandom};
use rand::Rng;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

//Recipe
pub const MAX_EDGE: &str = "max-edge";
pub const MAX_DISTANCE: &str = "max-distance";

pub const DOOR: &str = "door";

pub const CONNECTS: &str = "connects";
pub const OPENED: &str = "opened";
pub const MIN_DISTANCE: &str = "min-distance";

#[derive(Default)]
pub struct GripperDoorGenerator {}

impl Generator for GripperDoorGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(GripperDoorProblem::generate(recipe).map(Box::new)?)
    }
}

#[derive(Default)]
pub struct GripperDoorProblem {
    tasks: Vec<GripperTask>,
    //Topology
    graph: Graph<Room<Object>, Door, Undirected>,
}

#[derive(Debug)]
pub struct Door {
    pub id: u32,
    pub opened: bool,
}

impl Display for Door {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}", DOOR, self.id)
    }
}

pub struct GripperDoorConfig {
    pub gripper_config: GripperConfig,
    pub max_distance: u32,
    pub max_edge: u32,
}

impl TryFrom<&Recipe> for GripperDoorConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let gripper_config: GripperConfig = value.try_into()?;

        let max_distance = value.get_element(MAX_DISTANCE)?;
        let max_connected = value.get_element(MAX_EDGE)?;

        Ok(Self::new(gripper_config, max_distance, max_connected))
    }
}

impl GripperDoorConfig {
    pub fn new(gripper_config: GripperConfig, max_distance: u32, max_edge: u32) -> Self {
        Self {
            gripper_config,
            max_distance,
            max_edge,
        }
    }
}

impl Problem for GripperDoorProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String> {
        let GripperDoorConfig {
            gripper_config:
                GripperConfig {
                    n_ball,
                    n_room,
                    n_task,
                },
            max_distance,
            max_edge,
        }: GripperDoorConfig = recipe.try_into()?;

        let mut pb = Self::default();
        let rg = &mut rand::thread_rng();
        // Declaration of the rooms
        for i in 0..n_room {
            pb.graph.add_node(Room::new(i));
        }

        populate_topology(&mut pb.graph, max_distance, max_edge, &|id: u32| -> Door {
            let rg = &mut rand::thread_rng();
            Door {
                id,
                opened: rg.gen_bool(0.5),
            }
        });

        pb.graph
            .node_weights_mut()
            .choose(rg)
            .unwrap()
            .contains
            .push(Robby);

        let mut possible_tasks = vec![];

        // Declaration of the balls
        for b in 0..n_ball {
            let room = pb.graph.node_weights_mut().choose(rg).unwrap();
            room.contains.push(Ball(b));
            possible_tasks.push(Place(
                b,
                (0..n_room).filter(|i| i != &room.id).choose(rg).unwrap(),
            ));
        }

        possible_tasks.shuffle(rg);
        let _ = possible_tasks.split_off(n_task as usize);
        pb.tasks = possible_tasks;
        Ok(pb)
    }

    fn get_objects(&self) -> Vec<(String, Vec<String>)> {
        let mut balls = vec![];
        let mut rooms = vec![];
        let mut doors = vec![];

        for door in self.graph.edge_weights() {
            doors.push(door.to_string())
        }

        for node in self.graph.node_weights() {
            rooms.push(node.to_string());
            for o in &node.contains {
                if let Ball(_) = o {
                    balls.push(o.to_string())
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
        self.tasks.iter().map(|t| t.into()).collect()
    }

    fn get_dynamic_facts(&self) -> Vec<(LValue, LValue)> {
        let mut facts = vec![];
        for node in self.graph.node_weights() {
            let room_lv: LValue = node.to_string().into();
            for o in &node.contains {
                match o {
                    Robby => {
                        facts.push((AT_ROBBY.into(), room_lv.clone()));
                        facts.push((list!(CARRY.into(), LEFT.into()), EMPTY.into()));
                        facts.push((list!(CARRY.into(), RIGHT.into()), EMPTY.into()));
                    }
                    Ball(_) => {
                        facts.push((list!(POS.into(), o.to_string().into()), room_lv.clone()))
                    }
                }
            }
        }

        for door in self.graph.edge_weights() {
            facts.push((
                list!(OPENED.into(), door.to_string().into()),
                door.opened.into(),
            ));
        }

        facts
    }

    fn get_static_facts(&self) -> Vec<(LValue, LValue)> {
        let mut vec = export_connects(&self.graph);
        vec.append(&mut export_min_distance(&self.graph));
        vec
    }

    fn report(&self, path: PathBuf) -> PathBuf {
        let dot = Dot::with_attr_getters(
            &self.graph,
            &[],
            &|_, e| -> String {
                format!(
                    "label = \"{}({})={}\"",
                    OPENED,
                    e.weight(),
                    e.weight().opened
                )
            },
            &|_, (_, nr)| -> String {
                let mut label = nr.to_string();
                write!(label, "\n\t-contains:").unwrap();
                for o in &nr.contains {
                    write!(label, "\n\t\t-{}", o).unwrap();
                }
                format!("label=\"{}\", shape=rectangle, style=rounded", label)
            },
        );

        write_dot_to_file(self, path, format!("{:?}", dot))
    }
}

pub fn export_min_distance<T: ToString>(
    graph: &Graph<T, Door, Undirected>,
) -> Vec<(LValue, LValue)> {
    let mut facts = vec![];
    let map = floyd_warshall(graph, |_| 1).unwrap();
    for ((n1, n2), cost) in map {
        let n1: LValue = graph.node_weight(n1).unwrap().to_string().into();
        let n2: LValue = graph.node_weight(n2).unwrap().to_string().into();
        facts.push((vec![MIN_DISTANCE.into(), n1, n2].into(), cost.into()))
    }
    facts
}

pub fn export_connects<T: ToString>(graph: &Graph<T, Door, Undirected>) -> Vec<(LValue, LValue)> {
    let mut facts = vec![];
    for id in graph.edge_indices() {
        let (start, end) = graph.edge_endpoints(id).unwrap();
        let door: LValue = graph.edge_weight(id).unwrap().to_string().into();
        let n1: LValue = graph.node_weight(start).unwrap().to_string().into();
        let n2: LValue = graph.node_weight(end).unwrap().to_string().into();
        facts.push((
            list!(CONNECTS.into(), n1.clone(), door.clone(), n2.clone()),
            LValue::True,
        ));
        facts.push((
            list!(CONNECTS.into(), n2.clone(), door.clone(), n1.clone()),
            LValue::True,
        ));
        for index_1 in graph.node_indices() {
            let n1: LValue = graph.node_weight(index_1).unwrap().to_string().into();
            facts.push((
                list!(CONNECTS.into(), n1.clone(), door.clone(), n1.clone()),
                LValue::Nil,
            ));
            for index_2 in graph.node_indices().filter(|ni| {
                ni > &index_1
                    && !(ni == &start && index_1 == end)
                    && !(ni == &end && index_1 == start)
            }) {
                let n2: LValue = graph.node_weight(index_2).unwrap().to_string().into();
                facts.push((
                    list!(CONNECTS.into(), n1.clone(), door.clone(), n2.clone()),
                    LValue::Nil,
                ));
                facts.push((
                    list!(CONNECTS.into(), n2, door.clone(), n1.clone()),
                    LValue::Nil,
                ));
            }
        }
    }

    facts
}
