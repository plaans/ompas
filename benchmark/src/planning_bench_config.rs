use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;
use yaml_rust::Yaml;

pub struct PlanningBenchConfig {
    mail: MailConfig,
    number: i64,
    max_time: i64,
    domain_path: PathBuf,
    log_path: PathBuf,
    bin_path: PathBuf,
    problems: Vec<PathBuf>,
    techniques: Vec<PlanningTechnique>,
}

impl Display for PlanningBenchConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "config:\n
        {}\n\
        - number: {}\n\
        - max_time: {}\n\
        - domain_path: {:#?}\n\
        - log_path: {:#?}\n\
        - bin_path: {:#?} \n\
        - problems: {:#?}\n\
        - techniques: {:#?}",
            self.mail,
            self.number,
            self.max_time,
            self.domain_path,
            self.log_path,
            self.bin_path,
            self.problems,
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

pub type ConfigError = String;

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

impl TryFrom<&Yaml> for PlanningBenchConfig {
    type Error = ConfigError;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        let problems = value["problems"]
            .as_vec()
            .ok_or("problems is not an array.")?;

        let mut p2 = vec![];
        for p in problems {
            p2.push(p.as_str().ok_or("problem should be a str")?.into());
        }

        let techniques = value["techniques"]
            .as_vec()
            .ok_or("techniques is not an array.")?;
        let mut t2 = vec![];
        for t in techniques {
            t2.push(t.as_str().ok_or("problem should be a str")?.try_into()?);
        }

        Ok(Self {
            mail: value["mail"].borrow().try_into()?,
            number: value["number"].as_i64().ok_or("number should be a i64")?,
            max_time: value["max-time"]
                .as_i64()
                .ok_or("max-time should be a i64")?,
            domain_path: value["domain"]
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
            techniques: t2,
        })
    }
}

pub struct MailConfig {
    pub from: String,
    pub to: String,
    pub smtp: String,
    #[allow(dead_code)]
    password: String,
}

impl Display for MailConfig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "mail:\n\
           \t-from: {}\n\
            \t-to: {}\n\
            \t-smtp: {}\n\
            \t-password: *****
        ",
            self.from, self.to, self.smtp
        )
    }
}

impl TryFrom<&Yaml> for MailConfig {
    type Error = ConfigError;

    fn try_from(value: &Yaml) -> Result<Self, Self::Error> {
        Ok(Self {
            from: value["from"]
                .as_str()
                .ok_or("from should be a string")?
                .to_string(),
            to: value["to"]
                .as_str()
                .ok_or("to should be a string")?
                .to_string(),
            smtp: value["smtp"]
                .as_str()
                .ok_or("smtp should be a string")?
                .to_string(),
            password: value["password"]
                .as_str()
                .ok_or("password should be a string")?
                .to_string(),
        })
    }
}
