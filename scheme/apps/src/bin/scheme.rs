use ompas_middleware::logger::FileDescriptor;
use ompas_middleware::{LogLevel, Master};
use sompas_modules::io::LogOutput;
use sompas_modules::ModExtendedStd;
use sompas_repl::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

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
        Master::set_log_level(LogLevel::Trace).await
    }

    //test_lib_model(&opt);
    lisp_interpreter(opt.log, opt.root).await;
}

pub async fn lisp_interpreter(log: Option<PathBuf>, root: bool) {
    let mut li = LispInterpreter::new().await;

    if !root {
        let mut mod_extended_std = ModExtendedStd::default();
        //Add the sender of the channel.
        if let Some(pb) = &log {
            let mut file_pb = pb.clone();
            file_pb.push(format!("log_{}.txt", Master::get_string_date()));
            mod_extended_std.set_log_output(LogOutput::File(file_pb));
        }

        li.import_namespace(mod_extended_std);
    }

    li.set_config(LispInterpreterConfig::new(true));

    li.run(log.map(|p| FileDescriptor::AbsolutePath(fs::canonicalize(p).unwrap())))
        .await;
    Master::wait_end().await;
}
