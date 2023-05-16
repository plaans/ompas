use env_param::EnvParam;
use std::str::FromStr;

pub mod model;
pub mod ompas;
pub mod planning;

pub const TOKIO_CHANNEL_SIZE: usize = 100;

pub const OMPAS_CHRONICLE_DEBUG: &str = "OMPAS_CHRONICLE_DEBUG";
pub const OMPAS_PLAN_OUTPUT: &str = "OMPAS_PLAN_OUTPUT";
pub const OMPAS_DEBUG: &str = "OMPAS_DEBUG";
pub const OMPAS_PATH: &str = "OMPAS_PATH";
pub const OMPAS_LOG: &str = "OMPAS_LOG";
pub const OMPAS_PLAN_ENCODING_OPTIMIZATION: &str = "OMPAS_PLAN_ENCODING_OPTIMIZATION";

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

static OMPAS_CHRONICLE_DEBUG_ON: EnvParam<ChronicleDebug> =
    EnvParam::new(OMPAS_CHRONICLE_DEBUG, "off");
pub static OMPAS_DEBUG_ON: EnvParam<bool> = EnvParam::new(OMPAS_DEBUG, "false");
pub static OMPAS_LOG_ON: EnvParam<bool> = EnvParam::new(OMPAS_LOG, "false");
static OMPAS_PLAN_OUTPUT_ON: EnvParam<bool> = EnvParam::new(OMPAS_PLAN_OUTPUT, "false");
static OMPAS_PLAN_ENCODING_OPTIMIZATION_ON: EnvParam<bool> =
    EnvParam::new(OMPAS_PLAN_ENCODING_OPTIMIZATION, "true");

pub fn ompas_path() -> String {
    if let Ok(s) = std::env::var(OMPAS_PATH) {
        s
    } else {
        format!("{}/ompas", std::env::var("HOME").unwrap())
    }
}
