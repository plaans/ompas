//modules of the crate
pub mod language;
pub mod mod_godot;
pub mod platform;
mod platform_server;
pub mod serde;
pub mod tcp;

/// Const defining the size of the channels.
const TOKIO_CHANNEL_SIZE: usize = 100;
const DEFAULT_PATH_PROJECT_GODOT: &str = "/home/jeremy/godot/simulation-factory-godot/simu";
