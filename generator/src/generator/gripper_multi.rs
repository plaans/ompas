use crate::config::{GetElement, Recipe};
use crate::generator::gripper::GripperTask::Place;
use crate::generator::gripper::{
    GripperConfig, GripperTask, Room, BALL, CARRY, EMPTY, LEFT, POS, RIGHT, ROOM,
};
use crate::generator::gripper_door::{export_connects, Door, GripperDoorConfig, DOOR, OPENED};
use crate::generator::gripper_multi::Object::{Ball, Robot};
use crate::generator::{populate_topology, write_dot_to_file};
use crate::{Generator, Problem, Task};
use petgraph::dot::Dot;
use petgraph::{Graph, Undirected};
use rand::prelude::{IteratorRandom, SliceRandom};
use rand::Rng;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

pub const ROBOT: &str = "robot";

pub const AT_ROB: &str = "at-rob";
#[derive(Default)]
pub struct GripperMultiGenerator {}

impl Generator for GripperMultiGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(GripperMultiProblem::generate(recipe).map(Box::new)?)
    }
}

#[derive(Default)]
pub struct GripperMultiProblem {
    tasks: Vec<GripperTask>,
    //Topology
    graph: Graph<Room<Object>, Door, Undirected>,
}

pub struct GripperMultiConfig {
    pub gripper_door_config: GripperDoorConfig,
    pub n_robot: u32,
}

impl TryFrom<&Recipe> for GripperMultiConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let gripper_door_config = value.try_into()?;

        let n_robot = value.get_element(ROBOT)?;

        Ok(Self::new(gripper_door_config, n_robot))
    }
}

impl GripperMultiConfig {
    pub fn new(gripper_door_config: GripperDoorConfig, n_robot: u32) -> Self {
        Self {
            gripper_door_config,
            n_robot,
        }
    }
}

#[derive(Debug)]
pub enum Object {
    Robot(u32),
    Ball(u32),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Robot(i) => format!("{}_{}", ROBOT, i),
                Ball(i) => format!("{}_{}", BALL, i),
            }
        )
    }
}

impl Problem for GripperMultiProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String> {
        let GripperMultiConfig {
            gripper_door_config:
                GripperDoorConfig {
                    gripper_config:
                        GripperConfig {
                            n_ball,
                            n_room,
                            n_task,
                        },
                    max_distance,
                    max_edge,
                },
            n_robot,
        }: GripperMultiConfig = recipe.try_into()?;

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

        for i in 0..n_robot {
            pb.graph
                .node_weights_mut()
                .choose(rg)
                .unwrap()
                .contains
                .push(Robot(i));
        }

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
        let mut robots = vec![];

        for door in self.graph.edge_weights() {
            doors.push(door.to_string())
        }

        for node in self.graph.node_weights() {
            rooms.push(node.to_string());
            for o in &node.contains {
                let label = o.to_string();
                match o {
                    Robot(_) => robots.push(label),
                    Ball(_) => balls.push(label),
                }
            }
        }

        vec![
            (BALL.to_string(), balls),
            (ROOM.to_string(), rooms),
            (DOOR.to_string(), doors),
            (ROBOT.to_string(), robots),
        ]
    }

    fn get_tasks(&self) -> Vec<Task> {
        self.tasks
            .iter()
            .map(|t| match t {
                Place(b, r) => {
                    vec![
                        "place".to_string(),
                        Ball(*b).to_string(),
                        Room::<Object>::new(*r).to_string(),
                    ]
                }
            })
            .collect()
    }

    fn get_dynamic_facts(&self) -> Vec<(LValue, LValue)> {
        let mut facts = vec![];
        for node in self.graph.node_weights() {
            let room_lv: LValue = node.to_string().into();
            for o in &node.contains {
                let label: LValue = o.to_string().into();
                match o {
                    Robot(_) => {
                        facts.push((list![AT_ROB.into(), label.clone()], room_lv.clone()));
                        facts.push((
                            list![CARRY.into(), label.clone(), LEFT.into()],
                            EMPTY.into(),
                        ));
                        facts.push((
                            list![CARRY.into(), label.clone(), RIGHT.into()],
                            EMPTY.into(),
                        ));
                    }
                    _ => facts.push((list!(POS.into(), label), room_lv.clone())),
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
        export_connects(&self.graph)
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
