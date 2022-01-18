#[macro_use]
extern crate lazy_static;
pub mod async_await;
pub mod core;
pub mod functions;
pub mod language;
pub mod lisp_interpreter;
pub mod modules;
pub mod repl;
pub mod static_eval;
pub mod structs;
pub mod test_utils;

const TOKIO_CHANNEL_SIZE: usize = 16_384;
