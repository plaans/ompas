use crate::config::{ConfigError, GetSpecific};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use yaml_rust::Yaml;

pub struct SimConfig {
    pub techniques: Vec<PlanningTechnique>,
}

impl GetSpecific for SimConfig {
    fn get_specific(&self) -> Vec<String> {
        self.get_techniques()
    }
}

impl SimConfig {
    pub fn get_techniques(&self) -> Vec<String> {
        self.techniques
            .iter()
            .map(|t| {
                match t {
                    PlanningTechnique::Greedy => "",
                    PlanningTechnique::Aries => "-a",
                    PlanningTechnique::AriesOpt => "-o",
                    PlanningTechnique::RAEPlan => "-r",
                    PlanningTechnique::UPOM => "-u",
                }
                .to_string()
            })
            .collect()
    }
}

impl Display for SimConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "
        - techniques: {:#?}",
            self.techniques
        )
    }
}

const GREEDY: &str = "greedy";
const ARIES: &str = "aries";
const ARIES_OPT: &str = "aries-opt";
const RAE_PLAN: &str = "raeplan";
const UPOM: &str = "upom";

#[derive(Copy, Clone)]
pub enum PlanningTechnique {
    Greedy,
    Aries,
    AriesOpt,
    RAEPlan,
    UPOM,
}

impl TryFrom<&str> for PlanningTechnique {
    type Error = ConfigError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            GREEDY => Ok(Self::Greedy),
            ARIES => Ok(Self::Aries),
            ARIES_OPT => Ok(Self::AriesOpt),
            RAE_PLAN => Ok(Self::RAEPlan),
            UPOM => Ok(Self::UPOM),
            t => Err(format!("{} is not a valid technique", t)),
        }
    }
}

impl Display for PlanningTechnique {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}",
            match self {
                Self::Greedy => GREEDY,
                Self::Aries => ARIES,
                Self::AriesOpt => ARIES_OPT,
                Self::UPOM => UPOM,
                Self::RAEPlan => RAE_PLAN,
            }
        )
    }
}

impl Debug for PlanningTechnique {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl TryFrom<&Yaml> for SimConfig {
    type Error = ConfigError;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        let techniques = value["techniques"]
            .as_vec()
            .ok_or("techniques is not an array.")?;
        let mut t2 = vec![];
        for t in techniques {
            t2.push(t.as_str().ok_or("problem should be a str")?.try_into()?);
        }

        Ok(Self { techniques: t2 })
    }
}
