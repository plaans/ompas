use crate::config::{GetElement, Recipe};
use crate::generator::gripper::GripperTask::Place;
use crate::generator::gripper::Object::{Ball, Robby};
use crate::generator::write_dot_to_file;
use crate::{Generator, Problem, Task};
use petgraph::dot::Dot;
use petgraph::Graph;
use rand::prelude::{IteratorRandom, SliceRandom};
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

//Types
pub const BALL: &str = "ball";
pub const ROOM: &str = "room";
pub const TASK: &str = "task";
pub const EMPTY: &str ="empty";
pub const LEFT: &str= "left";
pub const RIGHT: &str= "right";
//State functions
pub const POS: &str = "pos";
pub const AT_ROBBY: &str = "at-robby";
pub const ROBBY: &str = "robby";
pub const CARRY: &str= "carry";


//Tasks
pub const PLACE: &str = "place";
#[derive(Default)]
pub struct GripperGenerator {}

impl Generator for GripperGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(GripperProblem::generate(recipe).map(Box::new)?)
    }
}

#[derive(Default)]
pub struct GripperProblem {
    tasks: Vec<GripperTask>,
    //Topology
    graph: Graph<Room<Object>, Connected>,
}

pub enum GripperTask {
    Place(u32, u32),
}

impl From<&GripperTask> for Task {
    fn from(value: &GripperTask) -> Self {
        match value {
            Place(b, r) => {
                vec![
                    PLACE.to_string(),
                    Ball(*b).to_string(),
                    Room::<Object>::new(*r).to_string(),
                ]
            }
        }
    }
}

impl Display for GripperTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Place(b, r) => write!(f, "{}({},{})", PLACE, Ball(*b), Room::<Object>::new(*r)),
        }
    }
}

#[derive(Debug)]
pub enum Connected {}

#[derive(Debug)]
pub enum Object {
    Robby,
    Ball(u32),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Robby => write!(f, "{}", ROBBY),
            Ball(i) => write!(f, "{}_{}", BALL, i),
        }
    }
}

#[derive(Debug)]
pub struct Room<T> {
    pub id: u32,
    pub contains: Vec<T>,
}

impl<T> Display for Room<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}", ROOM, self.id)
    }
}

impl<T> Room<T> {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            contains: vec![],
        }
    }
}

pub struct GripperConfig {
    pub n_ball: u32,
    pub n_room: u32,
    pub n_task: u32,
}

impl TryFrom<&Recipe> for GripperConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let n_ball = value.get_element(BALL)?;
        let n_room = value.get_element(ROOM)?;
        let n_task = value.get_element(TASK)?;

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
        let GripperConfig {
            n_ball,
            n_room,
            n_task,
        }: GripperConfig = recipe.try_into()?;

        let mut pb = Self::default();
        let rg = &mut rand::thread_rng();
        // Declaration of the rooms
        for i in 0..n_room {
            pb.graph.add_node(Room::new(i));
        }

        pb.graph
            .node_weights_mut()
            .choose(rg)
            .unwrap()
            .contains
            .push(Robby);

        // Declaration of the balls
        for i in 0..n_ball {
            pb.graph
                .node_weights_mut()
                .choose(rg)
                .unwrap()
                .contains
                .push(Ball(i));
        }

        let mut available: Vec<_> = (0..n_ball)
            .map(|id| Place(id, (0..n_room).choose(rg).unwrap()))
            .collect();
        available.shuffle(rg);
        let _ = available.split_off(n_task as usize);
        pb.tasks = available;
        Ok(pb)
    }

    fn get_objects(&self) -> Vec<(String, Vec<String>)> {
        let mut balls = vec![];
        let mut rooms = vec![];

        for node in self.graph.node_weights() {
            rooms.push(node.to_string());
            for o in &node.contains {
                if let Ball(_) = o {
                    balls.push(o.to_string())
                }
            }
        }

        vec![(BALL.to_string(), balls), (ROOM.to_string(), rooms)]
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
                    },
                    Ball(_) => {
                        facts.push((list!(POS.into(), o.to_string().into()), room_lv.clone()))
                    }
                }
            }
        }
        facts
    }

    fn get_static_facts(&self) -> Vec<(LValue, LValue)> {
        vec![]
    }

    fn report(&self, path: PathBuf) -> PathBuf {
        let dot = Dot::with_attr_getters(
            &self.graph,
            &[],
            &|_, _| -> String { "".to_string() },
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
