use crate::config::{ConfigError, GetSpecific};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use yaml_rust::Yaml;

pub struct GobotConfig {
    pub techniques: Vec<GobotTechnique>,
    pub view: bool,
}

impl GetSpecific for GobotConfig {
    fn get_specific(&self) -> Vec<String> {
        let mut techniques = self.get_techniques();
        if self.view {
            let _ = techniques.iter_mut().map(|t| t.push_str(" -v"));
        }
        techniques
    }
}

impl Display for GobotConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "
        - techniques: {:#?}",
            self.techniques
        )
    }
}

impl GobotConfig {
    pub fn get_techniques(&self) -> Vec<String> {
        self.techniques
            .iter()
            .map(|t| {
                match t {
                    GobotTechnique::Greedy => "",
                    GobotTechnique::FA => "-f",
                    GobotTechnique::Lrptf => "-L",
                    GobotTechnique::Aries => "-a",
                    GobotTechnique::AriesOpt => "-o",
                }
                .to_string()
            })
            .collect()
    }
}

const GREEDY: &str = "greedy";
const FA: &str = "fa";
const LRPTF: &str = "lrptf";
const ARIES: &str = "aries";
const ARIES_OPT: &str = "aries-opt";

pub enum GobotTechnique {
    Greedy,
    FA,
    Lrptf,
    Aries,
    AriesOpt,
}

impl TryFrom<&str> for GobotTechnique {
    type Error = ConfigError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            GREEDY => Ok(Self::Greedy),
            FA => Ok(Self::FA),
            LRPTF => Ok(Self::Lrptf),
            ARIES => Ok(Self::Aries),
            ARIES_OPT => Ok(Self::AriesOpt),
            t => Err(format!("{} is not a valid technique", t)),
        }
    }
}

impl Display for GobotTechnique {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}",
            match self {
                GobotTechnique::Greedy => GREEDY,
                GobotTechnique::FA => FA,
                GobotTechnique::Lrptf => LRPTF,
                GobotTechnique::Aries => ARIES,
                GobotTechnique::AriesOpt => ARIES_OPT,
            }
        )
    }
}

impl Debug for GobotTechnique {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl TryFrom<&Yaml> for GobotConfig {
    type Error = ConfigError;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        let techniques = value["techniques"]
            .as_vec()
            .ok_or("techniques is not an array.")?;
        let mut t2 = vec![];
        for t in techniques {
            t2.push(t.as_str().ok_or("problem should be a str")?.try_into()?);
        }
        let view = value["view"].as_bool().unwrap_or(false);

        Ok(Self {
            techniques: t2,
            view,
        })
    }
}
