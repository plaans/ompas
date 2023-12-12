use crate::config::Recipe;
use crate::generator::gobot::{GobotConfig, GobotProblem, Package};
use crate::{Generator, Problem};
use rand::prelude::SliceRandom;
use rand::{thread_rng, Rng};
use std::fmt::Write as OtherWrite;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;

#[derive(Default)]
pub struct JobshopGenerator {}

impl Generator for JobshopGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(JobshopProblem::generate(recipe).map(Box::new)?)
    }
}

pub struct JobshopProblem {
    pub inner: GobotProblem,
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
        let inner = &self.inner;
        let mut str = "nb_jobs nb_machines\n".to_string();
        writeln!(
            str,
            "{} {} {}",
            inner.scenario.packages.len(),
            inner.scenario.machines.len(),
            {
                let n = inner.scenario.machines.len() as i32 - 2;
                if n > 0 {
                    "0 ".repeat(n as usize)
                } else {
                    "".to_string()
                }
            }
        )
        .unwrap();
        let mut times = "".to_string();
        let mut machines = "".to_string();
        for p in &inner.scenario.packages {
            writeln!(
                times,
                "{}",
                p.iter()
                    .map(|p| p[1].to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
            .unwrap();
            writeln!(
                machines,
                "{}",
                p.iter()
                    .map(|p| p[0].to_string())
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
    pub gobot_config: GobotConfig,
}

impl TryFrom<&Recipe> for JobShopConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        Ok(Self {
            gobot_config: value.try_into()?,
        })
    }
}

impl Problem for JobshopProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String>
    where
        Self: Sized,
    {
        let JobShopConfig {
            gobot_config:
                GobotConfig {
                    n_package,
                    n_machine,
                    n_process,
                    min_time,
                    max_time,
                    ..
                },
        } = recipe.try_into()?;

        let mut gobot_problem = GobotProblem::generate(recipe)?;

        let rg = &mut thread_rng();
        for _ in 0..n_package {
            let mut package = Package::default();
            let mut machines: Vec<u32> = (1..n_machine + 1).collect();
            machines.shuffle(rg);
            for m in &machines[0..n_process as usize] {
                package.push([*m, rg.gen_range(min_time..max_time)])
            }
            gobot_problem.scenario.packages.push(package)
        }

        Ok(Self {
            inner: gobot_problem,
        })
    }

    fn to_sompas(&self) -> String {
        self.inner.to_sompas()
    }

    fn store(&mut self, path: &PathBuf) {
        {
            let mut path_scenario = path.clone();
            let file_name = path_scenario.file_name().unwrap();
            path_scenario.set_file_name(file_name.to_str().unwrap().replace(".scm", "_jf.txt"));
            let mut file_jobshop = File::create(&path_scenario).unwrap();
            file_jobshop
                .write_all(self.jobshop_string().as_bytes())
                .unwrap();

            self.inner.scenario.jobshop = Some(path_scenario);
        }
        self.inner.store(path)
    }

    fn report(&self, p: PathBuf) -> PathBuf {
        let path = self.inner.report(p);
        let mut file = OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(&path)
            .expect("error opening report.md");
        let content = format!("\n## Jobshop\n```\n{}\n```", self.jobshop_string());

        file.write_all(content.as_bytes()).unwrap();

        path
    }
}
