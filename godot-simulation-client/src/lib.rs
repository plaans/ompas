//modules of the crate
pub mod mod_godot;
mod rae_domain;
pub mod rae_interface;
pub mod serde;
pub mod tcp;

const TOKIO_CHANNEL_SIZE: usize = 65_536;
