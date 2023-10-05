use crate::config::{GetElement, Recipe};
use crate::{Generator, Problem, Task};
use rand::prelude::SliceRandom;
use rand::{thread_rng, Rng};
use serde::{Deserialize, Serialize};
use sompas_structs::lvalue::LValue;
use std::fmt::Write as OtherWrite;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub const MACHINE: &str = "machine";
pub const T_JOBSHOP: &str = "t_jobshop";
pub const JOB: &str = "job";
pub const MIN_TIME: &str = "min_time";
pub const MAX_TIME: &str = "max_time";
pub const TIME_SCALE: &str = "time_scale";
pub const ROBOT: &str = "robot";
pub const CONTROLLER: &str = "controller";
pub const CONTROLLER_PF: &str = "pf";
pub const TELEPORT: &str = "teleport";

pub const DEFAULT_CONTROLLER: &str = CONTROLLER_PF;
pub const DEFAULT_TIME_SCALE: u32 = 1;
pub const DEFAULT_ROBOT_NUMBER: u32 = 2;

pub struct JobshopGenerator {
    n_machine: u32,
}

impl JobshopGenerator {
    pub fn new(n_machine: u32) -> Self {
        Self { n_machine }
    }
}

impl Generator for JobshopGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        let mut recipe = recipe.clone();
        recipe.insert(MACHINE.to_string(), self.n_machine);
        Ok(JobshopProblem::generate(&recipe).map(Box::new)?)
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Machine {
    pub position: Vec<i64>,
    pub possible_processes: Vec<i64>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Robot {
    pub position: Vec<f64>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct Scenario {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub robots: Vec<Robot>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub machines: Vec<Machine>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub packages: Vec<Vec<Vec<i64>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    environments: Option<String>,
}

impl Scenario {
    pub fn add_robots(&mut self, n: u32) {
        for _ in 0..n {
            self.robots.push(Robot {
                position: vec![7.8, 16.5],
            })
        }
    }
}

pub type Job = Vec<Process>;

#[derive(Debug)]
pub struct Process {
    machine: u32,
    time: u32,
}

#[derive(Debug, Default)]
pub struct JobshopProblem {
    jobs: Vec<Job>,
    n_machine: u32,
    time_scale: u32,
    teleport: bool,
    controller: &'static str,
    path_jobshop: Option<PathBuf>,
    path_scenario: Option<PathBuf>,
    scenario: Scenario,
}

// nb_jobs nb_machines
// 6 6 0 0 0 0
// Times
// 1 3 6 7 3 6
// 8 5 10 10 10 4
// 5 4 8 9 1 7
// 5 5 5 3 8 9
// 9 3 5 4 3 1
// 3 3 9 10 4 1
// Machines
// 3 1 2 4 6 5
// 2 3 5 6 1 4
// 3 4 6 1 2 5
// 2 1 3 4 5 6
// 3 2 5 6 1 4
// 2 4 6 1 5 3
impl JobshopProblem {
    pub fn jobshop_string(&self) -> String {
        let mut str = "nb_jobs nb_machines\n".to_string();
        writeln!(str, "{} {} {}", self.jobs.len(), self.n_machine, {
            let n = self.n_machine as i32 - 2;
            if n > 0 {
                "0 ".repeat(n as usize)
            } else {
                "".to_string()
            }
        })
        .unwrap();
        let mut times = "".to_string();
        let mut machines = "".to_string();
        for j in &self.jobs {
            writeln!(
                times,
                "{}",
                j.iter()
                    .map(|p| p.time.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
            .unwrap();
            writeln!(
                machines,
                "{}",
                j.iter()
                    .map(|p| p.machine.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
            .unwrap()
        }
        writeln!(str, "Times").unwrap();
        str.push_str(times.as_str());
        writeln!(str, "Machines").unwrap();
        str.push_str(machines.as_str());

        str
    }
}

pub struct JobShopConfig {
    n_job: u32,
    n_machine: u32,
    min_time: u32,
    max_time: u32,
    time_scale: u32,
    teleport: bool,
    robot: u32,
    controller: &'static str,
}

impl TryFrom<&Recipe> for JobShopConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let n_job = value.get_element(JOB)?;
        let n_machine = value.get_element(MACHINE)?;
        let min_time = value.get_element(MIN_TIME)?;
        let max_time = value.get_element(MAX_TIME)?;
        assert!(
            min_time < max_time,
            "{}",
            format!("{}({}) >= {}({})", MIN_TIME, min_time, MAX_TIME, max_time)
        );
        let time_scale = value.get_element(TIME_SCALE).unwrap_or(DEFAULT_TIME_SCALE);
        let teleport = match value.get_element(TELEPORT).unwrap_or(0) {
            0 => false,
            1 => true,
            _ => return Err("{} should be either \"0\" or \"1\"".to_string()),
        };
        let robot = value.get_element(ROBOT).unwrap_or(1);
        let controller = match value.get_element(CONTROLLER).unwrap_or(0) {
            1 => CONTROLLER_PF,
            _ => DEFAULT_CONTROLLER,
        };

        Ok(Self {
            n_job,
            n_machine,
            min_time,
            max_time,
            time_scale,
            teleport,
            robot,
            controller,
        })
    }
}

impl Problem for JobshopProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String>
    where
        Self: Sized,
    {
        let JobShopConfig {
            n_job,
            n_machine,
            min_time,
            max_time,
            time_scale,
            teleport,
            robot,
            controller,
        } = recipe.try_into()?;

        let mut scenario = Scenario::default();
        scenario.add_robots(robot);

        let mut pb = Self {
            jobs: vec![],
            n_machine,
            time_scale,
            teleport,
            controller,
            path_jobshop: None,
            path_scenario: None,
            scenario,
        };

        let rg = &mut thread_rng();
        for _ in 0..n_job {
            let mut job = Job::default();
            let mut machines: Vec<u32> = (1..n_machine + 1).collect();
            machines.shuffle(rg);
            for m in machines {
                job.push(Process {
                    machine: m,
                    time: rg.gen_range(min_time..max_time),
                })
            }
            pb.jobs.push(job)
        }
        Ok(pb)
    }

    fn get_objects(&self) -> Vec<(String, Vec<String>)> {
        todo!()
    }

    fn get_tasks(&self) -> Vec<Task> {
        vec![vec![T_JOBSHOP.to_string()]]
    }

    fn get_dynamic_facts(&self) -> Vec<(LValue, LValue)> {
        todo!()
    }

    fn get_static_facts(&self) -> Vec<(LValue, LValue)> {
        todo!()
    }

    fn store(&mut self, path: &PathBuf) {
        {
            let mut path_jobhsop = path.clone();
            let file_name = path_jobhsop.file_name().unwrap();
            path_jobhsop.set_file_name(file_name.to_str().unwrap().replace(".lisp", "_jf.txt"));
            let mut file_jobshop = File::create(&path_jobhsop).unwrap();
            file_jobshop
                .write_all(self.jobshop_string().as_bytes())
                .unwrap();

            self.path_jobshop = Some(path_jobhsop);
        }
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
                .write_all(serde_json::to_string(&self.scenario).unwrap().as_bytes())
                .unwrap();

            self.path_scenario = Some(path_scenario);
        }

        let mut file = File::create(path).unwrap();
        file.write_all(self.to_sompas().as_bytes()).unwrap();
    }

    fn to_sompas(&self) -> String {
        let str = format!(
            "(begin
    (define path (get-env-var \"OMPAS_PATH\"))
    (define gobot-sim-path (concatenate path \"/ompas-gobot-sim/gobot-sim/simu/\"))
    (set-config-platform
         --path gobot-sim-path
         --scenario {}
         --environment (concatenate gobot-sim-path \"/environments/env_6_machines.json\")
         --jobshop {}
        {}
        --robot_controller {}
        --time_scale {})
    (exec-task t_jobshop))",
            match &self.path_scenario {
                None => "(concatenate gobot-sim-path \"/scenarios/new_scenario_multirobots.json\")"
                    .to_string(),
                Some(p) => format!("\"{}\"", p.to_str().unwrap()),
            },
            match &self.path_jobshop {
                None => {
                    "(concatenate gobot-sim-path \"/jobshop/instances/ft06.txt\")".to_string()
                }
                Some(p) => {
                    format!("\"{}\"", p.to_str().unwrap())
                }
            },
            match self.teleport {
                true => "--robot_controller teleport",
                false => "\n",
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
## Jobshop
```
{}
```
## Scenario
```
{}
```
## Scheme
```lisp
{}
```
",
            self.jobshop_string(),
            serde_json::to_string(&self.scenario).unwrap(),
            self.to_sompas(),
        );

        let mut file = File::create(&p).unwrap();
        file.write_all(content.as_bytes()).unwrap();
        p
    }
}
