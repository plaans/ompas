use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_core::OMPAS_LOG;
use ompas_language::process::LOG_TOPIC_OMPAS;
use ompas_middleware::logger::FileDescriptor;
use ompas_middleware::Master;
use sompas_modules::ModExtendedStd;
use sompas_repl::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use std::fs;
use std::path::PathBuf;
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
    lisp_interpreter(&opt).await;
}

async fn lisp_interpreter(opt: &Opt) {
    // let subscriber = tracing_subscriber::fmt()
    //     .with_timer(tracing_subscriber::fmt::time::Uptime::from(
    //         std::time::Instant::now(),
    //     ))
    //     // .without_time() // if activated, no time will be printed on logs (useful for counting events with `counts`)
    //     .with_thread_ids(true)
    //     .with_max_level(tracing::Level::TRACE)
    //     .finish();
    // tracing::subscriber::set_global_default(subscriber).unwrap();

    let mut li = LispInterpreter::new().await;

    let mut mod_extended_std = ModExtendedStd::default();

    mod_extended_std.set_time_zone(2);
    //Insert the doc for the different contexts.

    //Add the sender of the channel.
    if let Some(pb) = &opt.log {
        mod_extended_std.set_log_output(pb.clone().into());
    }

    li.import_namespace(mod_extended_std);

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
