use chrono::{DateTime, Utc};
use std::path::PathBuf;

use sompas_core::activate_debug;
use sompas_modules::advanced_math::CtxMath;
use sompas_modules::io::{CtxIo, LogOutput};
use sompas_modules::static_eval::CtxStaticEval;
use sompas_modules::string::CtxString;
use sompas_modules::utils::CtxUtils;
use sompas_repl::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(name = "Scheme", about = "A Scheme REPL")]
struct Opt {
    #[structopt(short = "d", long = "debug")]
    debug: bool,

    #[structopt(short = "p", long = "log-path")]
    log: Option<PathBuf>,

    #[structopt(short = "r", long = "root")]
    root: bool,
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
    lisp_interpreter(opt.log, opt.root).await;
}

pub async fn lisp_interpreter(log: Option<PathBuf>, root: bool) {
    let mut li = LispInterpreter::new().await;

    if !root {
        let mut ctx_io = CtxIo::default();

        let ctx_math = CtxMath::default();
        let ctx_utils = CtxUtils::default();
        let ctx_string = CtxString::default();

        let ctx_eval_static: CtxStaticEval = CtxStaticEval::new().await;

        //Add the sender of the channel.
        if let Some(pb) = &log {
            let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
            let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
            let mut file_pb = pb.clone();
            file_pb.push(format!("log_{}.txt", string_date));
            ctx_io.set_log_output(LogOutput::File(file_pb));
        }

        li.import_namespace(ctx_utils);
        li.import_namespace(ctx_io);
        li.import_namespace(ctx_math);

        li.import(ctx_string);
        li.import_namespace(ctx_eval_static);
    }

    li.set_config(LispInterpreterConfig::new(true));

    li.run(log).await;
}