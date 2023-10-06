use crate::config::{GetElement, Recipe};
use crate::Problem;
use map_macro::hash_set;
use ompas_core::OMPAS_PATH;
use rand::prelude::IteratorRandom;
use rand::prelude::SliceRandom;
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use serde_repr::{Deserialize_repr, Serialize_repr};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::ops::Range;
use std::path::PathBuf;

pub const MACHINE: &str = "machine";
pub const PROCESS: &str = "process";
pub const T_JOBSHOP: &str = "t_jobshop";
pub const PACKAGE: &str = "package";
pub const MIN_TIME: &str = "min_time";
pub const MAX_TIME: &str = "max_time";
pub const TIME_SCALE: &str = "time_scale";
pub const ROBOT: &str = "robot";
pub const CONTROLLER: &str = "controller";

pub const CONTROLLER_PF: &str = "pf";
pub const CONTROLLER_TELEPORT: &str = "teleport";

pub const DEFAULT_CONTROLLER: &str = CONTROLLER_PF;
pub const DEFAULT_TIME_SCALE: u32 = 1;
pub const DEFAULT_ROBOT_NUMBER: u32 = 2;
pub const DEFAULT_N_MACHINE: u32 = 6;

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Machine {
    pub position: [u32; 2],
    pub possible_processes: Vec<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    input_belt_size: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    output_belt_size: Option<u32>,
}
#[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
#[repr(u8)]
pub enum CreateOrder {
    Normal = 0,
    Reversed = 1,
    Random = 2,
}

#[derive(Serialize_repr, Deserialize_repr, PartialEq, Debug)]
#[repr(u8)]
pub enum CreateTime {
    Fixed = 0,
    Random = 1,
}

fn is_false(b: &bool) -> bool {
    !(*b)
}

pub type Position<T> = [T; 2];

#[derive(Debug, Serialize, Deserialize)]
pub struct InputMachine {
    position: Position<u32>,
    packages: Vec<Package>,
    #[serde(skip_serializing_if = "is_false")]
    infinite: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    create_order: Option<CreateOrder>,
    #[serde(skip_serializing_if = "Option::is_none")]
    create_time: Option<CreateTime>,
    #[serde(skip_serializing_if = "Option::is_none")]
    time_step: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    output_belt_size: Option<u32>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OutputMachine {
    position: Position<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    time_step: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    input_belt_size: Option<u32>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Robot {
    pub position: Position<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_battery: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    battery_drain_rate: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    battery_charge_rate: Option<f32>,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Scenario {
    #[serde(skip)]
    pub path: Option<PathBuf>,
    #[serde(skip)]
    pub n_process: u32,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub robots: Vec<Robot>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub machines: Vec<Machine>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub input_machines: Vec<InputMachine>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub output_machines: Vec<OutputMachine>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub packages: Vec<Package>,
    #[serde(flatten)]
    pub environment: Environment,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub jobshop: Option<PathBuf>,
}

impl Scenario {
    fn get_environment(n_machine: u32) -> Environment {
        match n_machine {
            6 => {
                let mut env_6_path: PathBuf = OMPAS_PATH.get_ref().clone().into();
                env_6_path.push("ompas-gobot-sim/gobot-sim/simu/environments/env_6_machines.json");
                let content = fs::read_to_string(&env_6_path).unwrap();
                let mut environment: Environment = serde_json::from_str(&content).unwrap();
                environment.path = env_6_path;
                environment
            }
            _ => panic!(""),
        }
    }

    pub fn new(n_machine: u32, n_process: u32) -> Self {
        let environment = Self::get_environment(n_machine);
        let machines = environment.get_machines();
        let input_machines = environment.get_input_machines();
        let output_machines = environment.get_output_machines();
        Self {
            path: None,
            n_process,
            robots: vec![],
            machines,
            input_machines,
            output_machines,
            packages: vec![],
            environment,
            jobshop: None,
        }
    }

    pub fn add_robots(&mut self, n: u32) {
        for _ in 0..n {
            let avoid: Vec<_> = self
                .robots
                .iter()
                .map(|r| r.position.map(|f| f as u32))
                .collect();
            self.robots.push(Robot {
                position: self
                    .environment
                    .new_robot_position(Some(vec![Tile::ParkingArea]), avoid)
                    .map(|u| u as f32),
                max_battery: None,
                battery_drain_rate: None,
                battery_charge_rate: None,
            })
        }
    }
    // "packages":[
    // [[0,10],[1,5]],
    // [[0,1],[1,8],[2,6]],
    // [[2,3],[1,9]],
    // [[2,7],[0,12],
    // [5,4]]],
    fn random_package(n_process: u32, time: Range<u32>, process: Range<u32>) -> Package {
        let rg = &mut thread_rng();
        let amount_process = process.clone().choose(rg).unwrap_or(process.start) as usize;
        let time = time.clone().choose(rg).unwrap_or(1);
        (0..n_process)
            .choose_multiple(rg, amount_process)
            .iter()
            .map(|p| [*p, time])
            .collect()
    }

    pub fn add_packages(&mut self, n: u32, time: Range<u32>, process: Range<u32>) {
        let n_process = self.n_process;
        for _ in 0..n {
            self.packages.push(Self::random_package(
                n_process,
                time.clone(),
                process.clone(),
            ))
        }
    }

    pub fn add_process_to_machines(&mut self, machine_per_process: u32) {
        let n_process = self.n_process;
        let mut processes: HashMap<u32, u32> =
            (0..n_process).map(|i| (i, machine_per_process)).collect();

        let rg = &mut thread_rng();

        let mut random_process =
            |processes: &mut HashMap<u32, u32>, avoid: &[u32]| -> Option<u32> {
                match processes
                    .iter()
                    .filter_map(|(id, _)| if !avoid.contains(id) { Some(id) } else { None })
                    .choose(rg)
                    .cloned()
                {
                    Some(p) => {
                        let n = processes.get_mut(&p).unwrap();
                        if *n == 1 {
                            processes.remove(&p);
                        } else {
                            *n -= 1;
                        }
                        Some(p)
                    }
                    None => None,
                }
            };

        let rg2 = &mut thread_rng();
        while !processes.is_empty() {
            let machine = self.machines.iter_mut().choose(rg2).unwrap();
            if let Some(process) =
                random_process(&mut processes, machine.possible_processes.as_slice())
            {
                machine.possible_processes.push(process);
            }
        }
    }
}

pub type Package = Vec<Process>;

pub type Process = [u32; 2];

#[derive(Debug, Copy, Clone, Serialize_repr, Deserialize_repr, PartialEq, Eq, Hash)]
#[repr(i32)]
pub enum Tile {
    Air = -1,
    Floor = 3,
    ParkingArea = 4,
    InteractArea = 5,
    Wall = 8,
    Machine = 9,
    InputBelt = 10,
    OutputBelt = 11,
    InputMachine = 12,
    OutputMachine = 13,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Environment {
    #[serde(skip_serializing)]
    pub data: Vec<Vec<Tile>>,
    #[serde(skip_serializing)]
    pub offset: Vec<i64>,
    #[serde(skip_deserializing, rename(serialize = "environment"))]
    pub path: PathBuf,
}

impl Environment {
    pub fn get_machines(&self) -> Vec<Machine> {
        let mut machines = vec![];
        for (y, line) in self.data.iter().enumerate() {
            for (x, tile) in line.iter().enumerate() {
                if let Tile::Machine = tile {
                    machines.push(Machine {
                        position: [y as u32, x as u32],
                        possible_processes: vec![],
                        input_belt_size: None,
                        output_belt_size: None,
                    })
                }
            }
        }
        machines
    }

    pub fn get_input_machines(&self) -> Vec<InputMachine> {
        vec![]
    }

    pub fn get_output_machines(&self) -> Vec<OutputMachine> {
        vec![]
    }

    fn new_robot_position(
        &self,
        filter: Option<Vec<Tile>>,
        avoid: Vec<Position<u32>>,
    ) -> Position<u32> {
        *self
            .get_possible_position_for_robots(filter, avoid)
            .choose(&mut thread_rng())
            .unwrap()
    }

    fn get_possible_position_for_robots(
        &self,
        filter: Option<Vec<Tile>>,
        avoid: Vec<Position<u32>>,
    ) -> Vec<Position<u32>> {
        let possible = hash_set! {
            Tile::Air,
            Tile::Floor,
            Tile::ParkingArea,
            Tile::InteractArea,
        };

        let possible = if let Some(filter) = filter {
            possible
                .intersection(&filter.iter().cloned().collect())
                .cloned()
                .collect()
        } else {
            possible
        };

        let mut positions = vec![];

        for (y, line) in self.data.iter().enumerate() {
            for (x, tile) in line.iter().enumerate() {
                let position = [y as u32, x as u32];
                if possible.contains(tile) && !avoid.contains(&position) {
                    positions.push(position)
                }
            }
        }
        positions
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub enum Controller {
    None = 0,
    #[default]
    PF = 1,
    Teleport = 2,
}
impl TryFrom<u32> for Controller {
    type Error = String;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Self::None,
            1 => Self::PF,
            2 => Self::Teleport,
            _ => return Err("Controller is either 0, 1 or 2.".to_string()),
        })
    }
}

impl Display for Controller {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Controller::None => "none",
                Controller::PF => CONTROLLER_PF,
                Controller::Teleport => CONTROLLER_TELEPORT,
            }
        )
    }
}

#[derive(Debug, Default)]
pub struct GobotProblem {
    pub time_scale: u32,
    pub controller: Controller,
    pub scenario: Scenario,
}

impl Problem for GobotProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String>
    where
        Self: Sized,
    {
        let GobotConfig {
            n_machine,
            time_scale,
            robot,
            controller,
            n_process,
            ..
        } = recipe.try_into()?;

        let mut scenario = Scenario::new(n_machine, n_process);
        scenario.add_robots(robot);

        Ok(Self {
            time_scale,
            controller,
            scenario,
        })
    }

    fn store(&mut self, path: &PathBuf) {
        {
            let mut path_scenario = path.clone();
            let file_name = path_scenario.file_name().unwrap();
            path_scenario.set_file_name(
                file_name
                    .to_str()
                    .unwrap()
                    .replace(".lisp", "_scenario.json"),
            );
            let mut file_jobshop = File::create(&path_scenario).unwrap();
            file_jobshop
                .write_all(
                    serde_json::to_string_pretty(&self.scenario)
                        .unwrap()
                        .as_bytes(),
                )
                .unwrap();

            self.scenario.path = Some(path_scenario);
        }

        let mut file = File::create(path).unwrap();
        file.write_all(self.to_sompas().as_bytes()).unwrap();
    }

    fn to_sompas(&self) -> String {
        let scenario = &self.scenario;

        let str = format!(
            "(begin
    (define path (get-env-var \"OMPAS_PATH\"))
    (define gobot-sim-path (concatenate path \"/ompas-gobot-sim/gobot-sim/simu/\"))
    (set-config-platform
         --path gobot-sim-path
        --scenario {}
        --robot_controller {}
        --time_scale {})
    (exec-task t_jobshop))",
            match &scenario.path {
                None => "(concatenate gobot-sim-path \"/scenarios/new_scenario_multirobots.json\")"
                    .to_string(),
                Some(p) => format!("\"{}\"", p.to_str().unwrap()),
            },
            self.controller,
            self.time_scale
        );
        str
    }
    fn report(&self, mut p: PathBuf) -> PathBuf {
        p.push("report.md");
        let content = format!(
            "
## Scenario
```
{}
```
## Scheme
```lisp
{}
```
",
            serde_json::to_string_pretty(&self.scenario).unwrap(),
            self.to_sompas(),
        );

        let mut file = File::create(&p).unwrap();
        file.write_all(content.as_bytes()).unwrap();
        p
    }
}

pub struct GobotConfig {
    pub n_package: u32,
    pub n_machine: u32,
    pub n_process: u32,
    pub min_time: u32,
    pub max_time: u32,
    pub time_scale: u32,
    pub robot: u32,
    pub controller: Controller,
}

impl TryFrom<&Recipe> for GobotConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let n_package = value.get_element(PACKAGE)?;
        let n_machine = value.get_element(MACHINE).unwrap_or(DEFAULT_N_MACHINE);
        let n_process = value.get_element(PROCESS)?;
        let min_time = value.get_element(MIN_TIME)?;
        let max_time = value.get_element(MAX_TIME)?;
        assert!(
            min_time < max_time,
            "{}",
            format!("{}({}) >= {}({})", MIN_TIME, min_time, MAX_TIME, max_time)
        );
        let time_scale = value.get_element(TIME_SCALE).unwrap_or(DEFAULT_TIME_SCALE);
        let robot = value.get_element(ROBOT).unwrap_or(1);
        let controller: Controller = value.get_element(CONTROLLER).unwrap_or(2).try_into()?;

        Ok(Self {
            n_package,
            n_machine,
            n_process,
            min_time,
            max_time,
            time_scale,
            robot,
            controller,
        })
    }
}
