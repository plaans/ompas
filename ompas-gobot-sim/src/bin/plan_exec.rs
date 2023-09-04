use ompas_core::ompas::manager::platform::lisp_domain::LispDomain;
use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_core::{OMPAS_DEBUG, OMPAS_LOG};
use ompas_gobotsim::default_gobot_sim_plan_exec_domain;
use ompas_gobotsim::platform::PlatformGobotSim;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PLATFORM_CLIENT};
use ompas_language::process::LOG_TOPIC_OMPAS;
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::{LogLevel, Master};
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 100;
pub const LOG_LEVEL: LogLevel = LogLevel::Debug;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
struct Opt {
    #[structopt(short = "p", long = "log-path")]
    log: Option<PathBuf>,

    #[structopt(short = "D", long = "domain")]
    domain: Option<PathBuf>,
}

#[tokio::main]
async fn main() {
    println!("OMPAS v0.1");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    Master::set_log_level(LOG_LEVEL).await;

    if OMPAS_DEBUG.get() {
        Master::set_log_level(LogLevel::Trace).await;
    }

    //test_lib_model(&opt);
    lisp_interpreter(opt).await;
}

async fn lisp_interpreter(opt: Opt) {
    let mut li = LispInterpreter::new().await;

    let mut ctx_io = ModIO::default();
    let ctx_math = ModAdvancedMath::default();
    let ctx_utils = ModUtils::default();
    let ctx_string = ModString::default();

    //Insert the doc for the different contexts.

    //Add the sender of the channel.
    if let Some(pb) = &opt.log {
        ctx_io.set_log_output(pb.clone().into());
    }

    li.import_namespace(ctx_utils);
    li.import_namespace(ctx_io);
    li.import_namespace(ctx_math);
    li.import_namespace(ctx_string);
    li.import_namespace(ModTime::new(2));

    let ctx_rae = ModMonitor::new(
        PlatformGobotSim::new(
            LispDomain::File(
                opt.domain
                    .unwrap_or(default_gobot_sim_plan_exec_domain().into()),
            ),
            false,
            LogClient::new(PLATFORM_CLIENT, LOG_TOPIC_PLATFORM).await,
        ),
        opt.log.clone(),
    )
    .await;
    li.import_namespace(ctx_rae);

    if OMPAS_LOG.get() {
        Master::start_display_log_topic(LOG_TOPIC_OMPAS).await;
    }

    li.set_config(LispInterpreterConfig::new(true));

    li.run(
        opt.log
            .map(|p| FileDescriptor::AbsolutePath(fs::canonicalize(p).unwrap())),
    )
    .await;

    Master::wait_end().await;
}
