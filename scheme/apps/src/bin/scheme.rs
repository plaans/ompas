use chrono::{DateTime, Utc};
use std::fs;
use std::path::PathBuf;

use ompas_middleware::logger::FileDescriptor;
use ompas_middleware::{LogLevel, Master};
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::{LogOutput, ModIO};
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 100;

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
        Master::set_log_level(LogLevel::Debug).await
    }

    //test_lib_model(&opt);
    lisp_interpreter(opt.log, opt.root).await;
}

pub async fn lisp_interpreter(log: Option<PathBuf>, root: bool) {
    let mut li = LispInterpreter::new().await;

    if !root {
        let mut ctx_io = ModIO::default();

        let ctx_math = ModAdvancedMath::default();
        let ctx_utils = ModUtils::default();
        let ctx_string = ModString::default();

        let ctx_time: ModTime = ModTime::new(2);

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
        li.import(ctx_time);
    }

    li.set_config(LispInterpreterConfig::new(true));

    li.run(log.map(|p| FileDescriptor::AbsolutePath(fs::canonicalize(p).unwrap())))
        .await;
    Master::end().await;
}
