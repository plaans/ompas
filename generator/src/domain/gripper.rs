use crate::config::Recipe;
use crate::domain::gripper::GripperTask::Place;
use crate::domain::gripper::Object::{Ball, Robby};
use crate::{Generator, Problem, Task};
use petgraph::Graph;
use rand::prelude::IteratorRandom;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::collections::HashSet;

const BALL: &str = "ball";
const ROOM: &str = "room";

#[derive(Default)]
pub struct GripperGenerator {}

impl Generator for GripperGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(GripperProblem::generate(&recipe).map(|p| Box::new(p))?)
    }
}

#[derive(Default)]
pub struct GripperProblem {
    tasks: Vec<GripperTask>,
    //Topology
    graph: Graph<Room, Connected>,
}

pub enum GripperTask {
    Place(u32, u32),
}

#[derive(Default)]
pub struct Connected {}

pub enum Object {
    Robby,
    Ball(u32),
}

impl Object {
    pub fn get_label(&self) -> String {
        match self {
            Self::Robby => "robby".to_string(),
            Self::Ball(i) => format!("{BALL}_{i}"),
        }
    }
}

pub struct Room {
    id: u32,
    contains: Vec<Object>,
}

impl Room {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            contains: vec![],
        }
    }

    pub fn get_label(&self) -> String {
        format!("{ROOM}_{}", self.id)
    }
}

pub struct GripperConfig {
    pub n_ball: u32,
    pub n_room: u32,
    pub n_task: u32,
}

impl TryFrom<Recipe> for GripperConfig {
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

        Ok(Self {
            n_ball,
            n_room,
            n_task,
        })
    }
}

impl GripperConfig {
    pub fn new(n_ball: u32, n_room: u32, n_task: u32) -> Self {
        Self {
            n_ball,
            n_room,
            n_task,
        }
    }
}

impl Problem for GripperProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String> {
        let config: GripperConfig = recipe.clone().try_into()?;

        let mut pb = Self::default();
        let rg = &mut rand::thread_rng();
        // Declaration of the rooms
        for i in 0..config.n_room {
            pb.graph.add_node(Room::new(i));
        }

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

        for node in self.graph.node_weights() {
            rooms.push(node.get_label());
            for o in &node.contains {
                if let Ball(_) = o {
                    balls.push(o.get_label())
                }
            }
        }

        vec![(BALL.to_string(), balls), (ROOM.to_string(), rooms)]
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
                    Robby => facts.push((list!["at-robby".into()], room_lv.clone())),
                    Ball(_) => {
                        facts.push((list!("pos".into(), o.get_label().into()), room_lv.clone()))
                    }
                }
            }
        }
        facts
    }

    fn get_static_facts(&self) -> Vec<(LValue, LValue)> {
        vec![]
    }
}
