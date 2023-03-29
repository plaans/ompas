use crate::config::gobot_config::GobotConfig;
use crate::config::sim_config::SimConfig;
use crate::MailConfig;
use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};
use std::path::PathBuf;
use yaml_rust::Yaml;

pub mod gobot_config;
pub mod mail_config;
pub mod sim_config;

const BENCH_SIM_BIN_NAME: &str = "bench_sim";
const BENCH_GOBOT_BIN_NAME: &str = "bench_gobot";

pub const TYPE_GOBOT: &str = "gobot";
pub const TYPE_SIM: &str = "sim";

pub type ConfigError = String;

pub struct BenchConfig {
    pub mail: MailConfig,
    pub number: i64,
    pub max_time: i64,
    pub domain_path: PathBuf,
    pub log_path: PathBuf,
    pub bin_path: PathBuf,
    pub problems: Vec<PathBuf>,
    pub specific: SpecificConfig,
}

pub trait GetSpecific {
    fn get_specific(&self) -> Vec<String>;
}

impl BenchConfig {
    pub fn get_specific(&self) -> Vec<String> {
        self.specific.get_specific()
    }

    pub fn get_bin_name(&self) -> &str {
        match self.specific {
            SpecificConfig::Sim(_) => BENCH_SIM_BIN_NAME,
            SpecificConfig::Gobot(_) => BENCH_GOBOT_BIN_NAME,
        }
    }
}

pub enum SpecificConfig {
    Sim(SimConfig),
    Gobot(GobotConfig),
}

impl GetSpecific for SpecificConfig {
    fn get_specific(&self) -> Vec<String> {
        match &self {
            SpecificConfig::Sim(s) => s.get_specific(),
            SpecificConfig::Gobot(s) => s.get_specific(),
        }
    }
}

impl TryFrom<&Yaml> for BenchConfig {
    type Error = ConfigError;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        let problems = value["problems"]
            .as_vec()
            .ok_or("problems is not an array.")?;

        let mut p2 = vec![];
        for p in problems {
            p2.push(p.as_str().ok_or("problem should be a str")?.into());
        }

        let t = value["type"].as_str().unwrap();
        let specific = match t {
            TYPE_GOBOT => SpecificConfig::Gobot(value.try_into()?),
            TYPE_SIM => SpecificConfig::Sim(value.try_into()?),
            _ => {
                panic!("does not understand the type of benchmark")
            }
        };

        Ok(Self {
            mail: value["mail"].borrow().try_into()?,
            number: value["number"].as_i64().ok_or("number should be a i64")?,
            max_time: value["max-time"]
                .as_i64()
                .ok_or("max-time should be a i64")?,
            domain_path: value["domain_path"]
                .as_str()
                .ok_or("domain is not valid")?
                .into(),
            log_path: value["log_path"]
                .as_str()
                .ok_or("log_path invalid.")?
                .into(),
            bin_path: value["bin_path"]
                .as_str()
                .ok_or("bin_path invalid.")?
                .into(),
            problems: p2,
            specific,
        })
    }
}
