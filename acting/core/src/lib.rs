use env_param::EnvParam;
use std::str::FromStr;

pub mod model;
pub mod ompas;
pub mod planning;

pub const TOKIO_CHANNEL_SIZE: usize = 100;

static OMPAS_CHRONICLE_DEBUG: EnvParam<ChronicleDebug> =
    EnvParam::new("OMPAS_CHRONICLE_DEBUG", "off");
pub static OMPAS_DEBUG: EnvParam<bool> = EnvParam::new("OMPAS_DEBUG", "false");
pub static OMPAS_LOG: EnvParam<bool> = EnvParam::new("OMPAS_LOG", "false");
static OMPAS_PLAN_OUTPUT: EnvParam<bool> = EnvParam::new("OMPAS_PLAN_OUTPUT", "false");
static OMPAS_PLAN_ENCODING_OPTIMIZATION: EnvParam<bool> =
    EnvParam::new("OMPAS_PLAN_ENCODING_OPTIMIZATION", "true");
static OMPAS_DELIBERATION_FREQUENCY: EnvParam<u64> =
    EnvParam::new("OMPAS_DELIBERATION_FREQUENCY", "1");
static OMPAS_DEBUG_CONTINUOUS_PLANNING: EnvParam<bool> =
    EnvParam::new("OMPAS_DEBUG_CONTINUOUS_PLANNING", "false");

static OMPAS_PLANNER_OUTPUT: EnvParam<bool> = EnvParam::new("OMPAS_PLANNER_OUTPUT", "false");

pub static OMPAS_PATH: EnvParam<String> = EnvParam::new("OMPAS_PATH", "~/ompas");

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum ChronicleDebug {
    Off = 0,
    On = 1,
    Full = 2,
}

impl FromStr for ChronicleDebug {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "off" => Ok(Self::Off),
            "on" => Ok(Self::On),
            "full" => Ok(Self::Full),
            _ => Err(()),
        }
    }
}
