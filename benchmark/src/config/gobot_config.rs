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
                    GobotTechnique::Advanced => "-a",
                    GobotTechnique::Lrptf => "-l",
                    GobotTechnique::Lptf => "",
                }
                .to_string()
            })
            .collect()
    }
}

const GREEDY: &str = "greedy";
const ADVANCED: &str = "advanced";
const LRPTF: &str = "lrptf";
const LPTF: &str = "lptf";

pub enum GobotTechnique {
    Greedy,
    Advanced,
    Lrptf,
    Lptf,
}

impl TryFrom<&str> for GobotTechnique {
    type Error = ConfigError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            GREEDY => Ok(Self::Greedy),
            ADVANCED => Ok(Self::Advanced),
            LRPTF => Ok(Self::Lrptf),
            LPTF => Ok(Self::Lptf),
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
                GobotTechnique::Advanced => ADVANCED,
                GobotTechnique::Lrptf => LRPTF,
                GobotTechnique::Lptf => LPTF,
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
