use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use std::fs;
use std::path::PathBuf;

use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_core::OMPAS_LOG;
use ompas_language::process::LOG_TOPIC_OMPAS;
use ompas_middleware::logger::FileDescriptor;
use ompas_middleware::Master;
use sompas_modules::time::ModTime;
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
struct Opt {
    #[structopt(short = "l", long = "log-path")]
    log: Option<PathBuf>,

    #[structopt(short = "p", long = "problem")]
    problem: Option<PathBuf>,

    #[structopt(short = "d", long = "domain")]
    domain: PathBuf,
}

#[tokio::main]
async fn main() {
    println!("OMPAS v0.1");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    //test_lib_model(&opt);
    lisp_interpreter(&opt).await;
}

async fn lisp_interpreter(opt: &Opt) {
    let mut li = LispInterpreter::new().await;

    let mut ctx_io = ModIO::default();
    let ctx_math = ModAdvancedMath::default();
    let ctx_utils = ModUtils::default();
    let ctx_string = ModString::default();
    let ctx_time = ModTime::new(2);

    //Insert the doc for the different contexts.

    //Add the sender of the channel.
    if let Some(pb) = &opt.log {
        ctx_io.set_log_output(pb.clone().into());
    }

    li.import_namespace(ctx_utils);
    li.import_namespace(ctx_io);
    li.import_namespace(ctx_math);
    li.import_namespace(ctx_string);
    li.import_namespace(ctx_time);

    let mut com = li.subscribe();
    let str = fs::read_to_string(&opt.domain).expect("Something went wrong reading the file");
    //println!("string in file: {}", str);
    com.send(str).await.expect("could not send to LI");
    if let Some(p) = &opt.problem {
        let str = fs::read_to_string(p).unwrap_or_else(|_| {
            panic!("Something went wrong reading the file {:?}", p.as_os_str())
        });
        //println!("string in file: {}", str);
        com.send(str).await.expect("could not send to LI");
    }
    tokio::spawn(async move {
        loop {
            if let Err(e) = com
                .recv()
                .await
                .expect("error receiving result of initialisation of domain")
            {
                panic!("error initialising the domain: {}", e)
            }
        }
    });

    let ctx_rae = ModMonitor::new("nil", opt.log.clone()).await;

    if OMPAS_LOG.get() {
        Master::start_display_log_topic(LOG_TOPIC_OMPAS).await;
    }

    li.import_namespace(ctx_rae);

    li.set_config(LispInterpreterConfig::new(true));

    li.run(
        opt.log
            .as_ref()
            .map(|p| FileDescriptor::AbsolutePath(fs::canonicalize(p).unwrap())),
    )
    .await;
    Master::wait_end().await;
}
