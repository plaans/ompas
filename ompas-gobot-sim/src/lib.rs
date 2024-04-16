//modules of the crate
pub mod language;
pub mod platform;
mod platform_server;
pub mod serde;
pub mod tcp;
use ompas_core::OMPAS_PATH;

/// Const defining the size of the channels.
const TOKIO_CHANNEL_SIZE: usize = 100;
//const PROCESS_TOPIC_GOBOT_SIM: &str = "__PROCESS_TOPIC_GOBOT_SIM__";

pub const GODOT_PATH: &str = "GODOT3_PATH";
pub const GODOT_HEADLESS_PATH: &str = "GODOT3_HEADLESS_PATH";
pub const GODOT_BINARY_PATH: &str = "GODOT_BINARY_PATH";

pub fn default_gobot_sim_path() -> String {
    format!("{}/ompas-gobot-sim/gobot-sim/simu", OMPAS_PATH.get_ref())
}

pub fn default_gobot_sim_domain() -> String {
    format!(
        "{}/ompas-gobot-sim/test_domain/domain.scm",
        OMPAS_PATH.get_ref()
    )
}

pub fn default_gobot_sim_plan_domain() -> String {
    format!(
        "{}/ompas-gobot-sim/planning_domain/domain_plan.scm",
        OMPAS_PATH.get_ref()
    )
}

pub fn default_gobot_sim_plan_domain_e() -> String {
    format!(
        "{}/ompas-gobot-sim/planning_domain/domain_plan_extended.scm",
        OMPAS_PATH.get_ref()
    )
}

pub fn default_gobot_sim_plan_exec_domain() -> String {
    format!(
        "{}/ompas-gobot-sim/planning_domain/domain.scm",
        OMPAS_PATH.get_ref()
    )
}
