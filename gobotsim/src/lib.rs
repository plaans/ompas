//modules of the crate
pub mod language;
pub mod mod_godot;
pub mod rae_interface;
pub mod serde;
pub mod tcp;
/// Const defining the size of the channels.
const TOKIO_CHANNEL_SIZE: usize = 100;
