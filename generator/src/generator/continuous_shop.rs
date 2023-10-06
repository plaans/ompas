use crate::config::{GetElement, Recipe};
use crate::generator::gobot::{GobotConfig, GobotProblem, PROCESS};
use crate::{Generator, Problem};
use std::path::PathBuf;

pub const PACKAGE_MIN_PROCESS: &str = "package_min_process";
pub const PACKAGE_MAX_PROCESS: &str = "package_max_process";
pub const MACHINE_PER_PROCESS: &str = "machine_per_process";

#[derive(Default)]
pub struct ContinuousShopGenerator {}

impl Generator for ContinuousShopGenerator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String> {
        Ok(ContinuousShopProblem::generate(recipe).map(Box::new)?)
    }
}

pub struct ContinuousShopConfig {
    gobot_config: GobotConfig,
    package_min_process: u32,
    package_max_process: u32,
    machine_per_process: u32,
}

impl TryFrom<&Recipe> for ContinuousShopConfig {
    type Error = String;

    fn try_from(value: &Recipe) -> Result<Self, Self::Error> {
        let gobot_config: GobotConfig = value.try_into()?;
        let package_min_process = value.get_element(PACKAGE_MIN_PROCESS).unwrap_or(1);
        let package_max_process = value.get_element(PACKAGE_MAX_PROCESS).unwrap_or(1);
        assert!(
            package_max_process <= gobot_config.n_process,
            "{}",
            format!(
                "{}({}) > {}({})",
                PACKAGE_MAX_PROCESS, package_max_process, PROCESS, gobot_config.n_process,
            )
        );
        assert!(
            package_min_process <= package_max_process,
            "{}",
            format!(
                "{}({}) > {}({})",
                PACKAGE_MIN_PROCESS, package_min_process, PACKAGE_MAX_PROCESS, package_max_process
            )
        );
        let machine_per_process = value.get_element(MACHINE_PER_PROCESS).unwrap_or(1);
        Ok(Self {
            gobot_config,
            package_min_process,
            package_max_process,
            machine_per_process,
        })
    }
}

pub struct ContinuousShopProblem {
    inner: GobotProblem,
}

impl Problem for ContinuousShopProblem {
    fn generate(recipe: &Recipe) -> Result<Self, String>
    where
        Self: Sized,
    {
        let ContinuousShopConfig {
            gobot_config:
                GobotConfig {
                    n_package,
                    min_time,
                    max_time,
                    ..
                },
            package_min_process,
            package_max_process,
            machine_per_process,
        } = recipe.try_into()?;

        let mut inner = GobotProblem::generate(recipe)?;
        inner.scenario.add_process_to_machines(machine_per_process);
        inner.scenario.add_packages(
            n_package,
            min_time..max_time + 1,
            package_min_process..package_max_process + 1,
        );

        Ok(Self { inner })
    }

    fn to_sompas(&self) -> String {
        self.inner.to_sompas()
    }

    fn store(&mut self, path: &PathBuf) {
        self.inner.store(path)
    }

    fn report(&self, p: PathBuf) -> PathBuf {
        self.inner.report(p)
    }
}
