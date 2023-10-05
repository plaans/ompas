use crate::config::{GetElement, Recipe};
use crate::generator::gripper::{GripperConfig, GripperTask, Room, BALL, POS, ROOM};
use crate::generator::gripper_build::ToyPart::{Head, LeftArm, LeftLeg, RightArm, RightLeg, Torso};
use crate::generator::gripper_door::{Door, GripperDoorConfig, CONNECTS, DOOR, OPENED};
use crate::generator::gripper_multi::{GripperMultiConfig, AT_ROB};
use crate::generator::gripper_multi::{Object as OtherObject, ROBOT};
use crate::generator::{populate_topology, write_dot_to_file};
use crate::{Generator, Problem, Task};
use petgraph::dot::Dot;
use petgraph::Graph;
use rand::prelude::{IteratorRandom, SliceRandom};
use rand::Rng;
use sompas_structs::list;
use sompas_structs::lvalue::LValue;
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::path::PathBuf;

pub const TORSO: &str = "torso";
pub const ARM: &str = "arm";
pub const RIGHT_ARM: &str = "right_arm";
pub const LEFT_ARM: &str = "left_arm";
pub const HEAD: &str = "head";
pub const LEG: &str = "leg";
pub const RIGHT_LEG: &str = "right_leg";
pub const LEFT_LEG: &str = "left_leg";
pub const TOY: &str = "toy";

pub const ETHER: &str = "ether";

//State functions:
pub const LEFT_LEG_OF: &str = "left_leg_of";
pub const RIGHT_LEG_OF: &str = "left_arm_of";
pub const TORSO_OF: &str = "torso_of";
pub const LEFT_ARM_OF: &str = "left_arm_of";
pub const RIGHT_ARM_OF: &str = "right_arm_of";
pub const HEAD_OF: &str = "head_of";
pub const BUILT: &str = "built";

//Task
pub const BUILD_TOY: &str = "build_toy";
#[derive(Default)]
pub struct GripperBuildGenerator {}

impl Generator for GripperBuildGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(GripperBuildProblem::generate(recipe).map(Box::new)?)
    }
}

#[derive(Debug)]
pub struct Toy {
    pub id: u32,
    pub built: bool,
}

impl Toy {
    pub fn get_parts(&self) -> [ToyPart; 6] {
        let id = self.id;
        [
            LeftArm(id),
            RightArm(id),
            LeftLeg(id),
            RightLeg(id),
            Torso(id),
            Head(id),
        ]
    }
}

#[derive(Debug)]
pub enum Object {
    OtherObject(crate::generator::gripper_multi::Object),
    ToyPart(ToyPart),
    Toy(Toy),
}

impl Display for Toy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_{}", TOY, self.id)
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Object::OtherObject(o) => {
                    o.to_string()
                }
                Object::ToyPart(tp) => {
                    tp.to_string()
                }
                Object::Toy(t) => {
                    t.to_string()
                }
            }
        )
    }
}

#[derive(Debug)]
pub enum ToyPart {
    LeftLeg(u32),
    RightLeg(u32),
    LeftArm(u32),
    RightArm(u32),
    Head(u32),
    Torso(u32),
}

impl Display for ToyPart {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ToyPart::LeftLeg(i) => {
                write!(f, "{}_{}", LEFT_LEG, i)
            }
            ToyPart::RightLeg(i) => {
                write!(f, "{}_{}", RIGHT_LEG, i)
            }
            ToyPart::LeftArm(i) => {
                write!(f, "{}_{}", LEFT_ARM, i)
            }
            ToyPart::RightArm(i) => {
                write!(f, "{}_{}", RIGHT_ARM, i)
            }
            ToyPart::Head(i) => {
                write!(f, "{}_{}", HEAD, i)
            }
            ToyPart::Torso(i) => {
                write!(f, "{}_{}", TORSO, i)
            }
        }
    }
}

pub enum GripperBuildTask {
    OtherTask(GripperTask),
    BuildToy(u32),
}

impl From<&GripperBuildTask> for Task {
    fn from(value: &GripperBuildTask) -> Self {
        match value {
            GripperBuildTask::OtherTask(t) => t.into(),
            GripperBuildTask::BuildToy(id) => {
                vec![
                    BUILD_TOY.to_string(),
                    Toy {
                        id: *id,
                        built: false,
                    }
                    .to_string(),
                ]
            }
        }
    }
}

#[derive(Default)]
pub struct GripperBuildProblem {
    tasks: Vec<GripperBuildTask>,
    //Topology
    ether: Vec<Object>,
    graph: Graph<Room<Object>, Door>,
}

pub struct GripperBuildConfig {
    pub gripper_multi_config: GripperMultiConfig,
    pub n_toy: u32,
}

impl TryFrom<&Recipe> for GripperBuildConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let gripper_multi_config = value.try_into()?;

        let n_toy = value.get_element(TOY)?;

        Ok(Self::new(gripper_multi_config, n_toy))
    }
}

impl GripperBuildConfig {
    pub fn new(gripper_multi_config: GripperMultiConfig, n_toy: u32) -> Self {
        Self {
            gripper_multi_config,
            n_toy,
        }
    }
}

impl Problem for GripperBuildProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String> {
        let GripperBuildConfig {
            gripper_multi_config:
                GripperMultiConfig {
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
                },
            n_toy,
        }: GripperBuildConfig = recipe.try_into()?;

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

        // Declaration of the balls
        for i in 0..n_ball {
            pb.graph
                .node_weights_mut()
                .choose(rg)
                .unwrap()
                .contains
                .push(Object::OtherObject(OtherObject::Ball(i)));
        }

        for i in 0..n_robot {
            pb.graph
                .node_weights_mut()
                .choose(rg)
                .unwrap()
                .contains
                .push(Object::OtherObject(OtherObject::Robot(i)));
        }

        for i in 0..n_toy {
            let built = rg.gen_bool(0.5);
            match built {
                true => {
                    pb.graph
                        .node_weights_mut()
                        .choose(rg)
                        .unwrap()
                        .contains
                        .push(Object::Toy(Toy { id: i, built }));
                }
                false => {
                    let toy = Toy { id: i, built };
                    for tp in toy.get_parts() {
                        pb.graph
                            .node_weights_mut()
                            .choose(rg)
                            .unwrap()
                            .contains
                            .push(Object::ToyPart(tp));
                    }
                    pb.ether.push(Object::Toy(Toy { id: i, built }));
                }
            }
        }

        let mut available: Vec<_> = (0..n_ball)
            .map(|id| {
                GripperBuildTask::OtherTask(GripperTask::Place(id, (0..n_room).choose(rg).unwrap()))
            })
            .collect();
        available.append(&mut (0..n_toy).map(GripperBuildTask::BuildToy).collect());
        available.shuffle(rg);
        pb.tasks = available.split_off(n_task as usize);

        Ok(pb)
    }

    fn get_objects(&self) -> Vec<(String, Vec<String>)> {
        let mut balls = vec![];
        let mut rooms = vec![];
        let mut doors = vec![];
        let mut robots = vec![];
        let mut legs = vec![];
        let mut arms = vec![];
        let mut torsos = vec![];
        let mut heads = vec![];
        let mut toys = vec![];

        let mut add_object = |o: &Object| {
            let label = o.to_string();
            match o {
                Object::OtherObject(oo) => match oo {
                    OtherObject::Robot(_) => robots.push(label),
                    OtherObject::Ball(_) => balls.push(label),
                },
                Object::ToyPart(tp) => match tp {
                    ToyPart::LeftLeg(_) | ToyPart::RightLeg(_) => legs.push(label),
                    ToyPart::LeftArm(_) | ToyPart::RightArm(_) => arms.push(label),
                    ToyPart::Head(_) => heads.push(label),
                    ToyPart::Torso(_) => torsos.push(label),
                },
                Object::Toy(_) => toys.push(label),
            }
        };

        for door in self.graph.edge_weights() {
            doors.push(door.to_string())
        }

        for node in self.graph.node_weights() {
            rooms.push(node.to_string());
            for o in &node.contains {
                add_object(o)
            }
        }

        for o in &self.ether {
            add_object(o)
        }

        vec![
            (BALL.to_string(), balls),
            (ROOM.to_string(), rooms),
            (DOOR.to_string(), doors),
            (ROBOT.to_string(), robots),
            (ARM.to_string(), arms),
            (LEG.to_string(), legs),
            (TORSO.to_string(), torsos),
            (HEAD.to_string(), heads),
            (TOY.to_string(), toys),
        ]
    }

    fn get_tasks(&self) -> Vec<Task> {
        self.tasks.iter().map(|t| t.into()).collect()
    }

    fn get_dynamic_facts(&self) -> Vec<(LValue, LValue)> {
        let mut facts = vec![];

        self.ether.iter().for_each(|o| {
            let label: LValue = o.to_string().into();
            if let Object::Toy(t) = o {
                facts.push((list!(BUILT.into(), label.clone()), t.built.into()))
            }
            facts.push((list!(POS.into(), label), ETHER.into()))
        });

        for node in self.graph.node_weights() {
            let room_lv: LValue = node.to_string().into();
            for o in &node.contains {
                let label: LValue = o.to_string().into();
                match o {
                    Object::OtherObject(OtherObject::Robot(_)) => {
                        facts.push((list![AT_ROB.into(), label], room_lv.clone()))
                    }
                    other => {
                        if let Object::Toy(t) = other {
                            let label: LValue = t.to_string().into();
                            facts.push((list!(BUILT.into(), label.clone()), t.built.into()));
                            for part in t.get_parts() {
                                facts.push((
                                    list!(POS.into(), part.to_string().into()),
                                    label.clone(),
                                ))
                            }
                        }
                        facts.push((list!(POS.into(), label), room_lv.clone()))
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
        let mut facts = vec![];

        let mut add_composition_of_toy = |t: &Toy| {
            let id = t.id;
            let label: LValue = t.to_string().into();
            let vec = vec![
                (LEFT_ARM_OF, ToyPart::LeftArm(id)),
                (RIGHT_ARM_OF, ToyPart::RightArm(id)),
                (TORSO_OF, ToyPart::Torso(id)),
                (RIGHT_LEG_OF, ToyPart::RightLeg(id)),
                (LEFT_LEG_OF, ToyPart::LeftLeg(id)),
                (HEAD_OF, ToyPart::Head(id)),
            ];

            for (sf, o) in vec {
                facts.push((list!(sf.into(), label.clone()), o.to_string().into()));
            }
        };

        self.ether.iter().for_each(|o| {
            if let Object::Toy(t) = o {
                add_composition_of_toy(t)
            }
        });

        self.graph.node_weights().for_each(|n| {
            n.contains.iter().for_each(|o| {
                if let Object::Toy(t) = o {
                    add_composition_of_toy(t)
                }
            })
        });

        for id in self.graph.edge_indices() {
            let (start, end) = self.graph.edge_endpoints(id).unwrap();
            let door = self.graph.edge_weight(id).unwrap();
            facts.push((
                list!(
                    CONNECTS.into(),
                    self.graph.node_weight(start).unwrap().to_string().into(),
                    self.graph.node_weight(end).unwrap().to_string().into()
                ),
                door.to_string().into(),
            ));
        }
        facts
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
