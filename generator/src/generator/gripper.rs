use crate::config::Recipe;
use crate::generator::gripper::GripperTask::Place;
use crate::generator::gripper::Object::{Ball, Robby};
use crate::{Generator, Problem, Task};
use petgraph::dot::Dot;
use petgraph::Graph;
use rand::prelude::IteratorRandom;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::collections::HashSet;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Write as OtherWrite;
use std::path::PathBuf;
use std::process::Command;

//Types
pub const BALL: &str = "ball";
pub const ROOM: &str = "room";

//State functions
pub const POS: &str = "pos";
pub const AT_ROBBY: &str = "at-robby";

//Tasks
pub const PLACE: &str = "place";
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

impl From<&GripperTask> for Task {
    fn from(value: &GripperTask) -> Self {
        match value {
            Place(b, r) => {
                vec![
                    PLACE.to_string(),
                    Ball(*b).get_label(),
                    Room::new(*r).get_label(),
                ]
            }
        }
    }
}

impl Display for GripperTask {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Place(b, r) => write!(
                f,
                "{}({},{})",
                PLACE,
                Ball(*b).get_label(),
                Room::new(*r).get_label(),
            ),
        }
    }
}

#[derive(Debug)]
pub enum Connected {
    None,
    Door(DoorStatus),
}

#[derive(Debug)]
pub enum DoorStatus {
    Opened,
    Closed,
}

#[derive(Debug)]
pub enum Object {
    Robby,
    Ball(u32),
}

impl Object {
    pub fn get_label(&self) -> String {
        match self {
            Robby => "robby".to_string(),
            Ball(i) => format!("{BALL}_{i}"),
        }
    }
}

#[derive(Debug)]
pub struct Room {
    pub id: u32,
    pub contains: Vec<Object>,
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
        self.tasks.iter().map(|t| t.into()).collect()
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
                let mut label = nr.get_label();
                write!(label, "\n\t-contains:").unwrap();
                for o in &nr.contains {
                    write!(label, "\n\t\t-{}", o.get_label()).unwrap();
                }
                format!("label=\"{}\", shape=rectangle, style=rounded", label)
            },
        );

        let mut dot_file_name = path.clone();
        dot_file_name.push("topology.dot");
        let mut dot_file = File::create(dot_file_name.clone()).unwrap();

        dot_file.write_all(format!("{:?}", dot).as_bytes()).unwrap();

        let mut img_file_name = path.clone();
        let name = "topology.png";
        img_file_name.push(name);

        let mut content = "## Tasks\n".to_string();
        for t in &self.tasks {
            write!(content, "-{}", t.to_string()).unwrap();
        }

        write!(content, "\n## TOPOLOGY\n ![]({})", name).unwrap();
        write!(
            content,
            "\n## PROBLEM FILE \n ```lisp\n\
        {}\n\
        ```",
            self.to_sompas()
        )
        .unwrap();

        Command::new("dot")
            .args([
                "-Tpng",
                dot_file_name.to_str().unwrap(),
                "-o",
                img_file_name.to_str().unwrap(),
            ])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut report_file_name = path.clone();
        report_file_name.push("report.md");
        let mut md_file = File::create(&report_file_name).unwrap();

        md_file.write_all(content.as_bytes()).unwrap();

        report_file_name
    }
}
