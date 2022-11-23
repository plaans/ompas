pub mod lisp_interpreter;
pub mod repl;

const TOKIO_CHANNEL_SIZE: usize = 100;
const PROCESS_TOPIC_INTERPRETER: &str = "__PROCESS_TOPIC_INTERPRETER__";
const LOG_TOPIC_INTERPRETER: &str = "__LOG_TOPIC_INTERPRETER__";
