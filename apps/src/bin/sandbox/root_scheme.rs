use std::path::PathBuf;

use structopt::StructOpt;

//use ompas_modules::robot::CtxRobot;
use ompas_lisp::core::*;
use ompas_lisp::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(name = "Scheme", about = "A Scheme REPL")]
struct Opt {
    #[structopt(short = "d", long = "debug")]
    debug: bool,

    #[structopt(short = "p", long = "log-path")]
    log: Option<PathBuf>,
}

#[tokio::main]
async fn main() {
    println!("Scheme console v0.1");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    if opt.debug {
        activate_debug();
    }

    //test_lib_model(&opt);
    lisp_interpreter(opt.log).await;
}

pub async fn lisp_interpreter(log: Option<PathBuf>) {
    let mut li = LispInterpreter::new().await;

    li.set_config(LispInterpreterConfig::new(true));

    li.run(log).await;
}
