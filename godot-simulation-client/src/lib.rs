//modules of the crate
pub mod mod_godot;
mod rae_domain;
pub mod rae_interface;
pub mod serde;
pub mod tcp;

/// Const defining the size of the channels.
const TOKIO_CHANNEL_SIZE: usize = 65_536;
