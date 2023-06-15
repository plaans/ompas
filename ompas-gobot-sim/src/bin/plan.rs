//use ompas_gobotsim::mod_godot::CtxGodot;
use ompas_core::ompas::scheme::exec::platform::lisp_domain::LispDomain;
use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_gobotsim::default_gobot_sim_plan_domain;
use ompas_gobotsim::platform::PlatformGobotSim;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PLATFORM_CLIENT};
use ompas_middleware::logger::LogClient;
use ompas_middleware::{LogLevel, Master};
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::LispInterpreter;
use sompas_structs::lruntimeerror;
use std::path::PathBuf;
use structopt::StructOpt;

pub const LOG_LEVEL: LogLevel = LogLevel::Debug;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
struct Opt {
    #[structopt(short = "d", long = "domain")]
    domain: Option<PathBuf>,

    #[structopt(short = "o", long = "optimal")]
    _opt: bool,

    #[structopt(short = "v", long = "view-simulator")]
    view: bool,
}

#[tokio::main]
async fn main() -> lruntimeerror::Result<()> {
    println!("OMPAS v0.1");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    Master::set_log_level(LOG_LEVEL).await;

    //test_lib_model(&opt);
    lisp_interpreter(opt).await
}

async fn lisp_interpreter(opt: Opt) -> lruntimeerror::Result<()> {
    let mut li = LispInterpreter::new().await;

    let ctx_io = ModIO::default();
    let ctx_math = ModAdvancedMath::default();
    let ctx_utils = ModUtils::default();
    let ctx_string = ModString::default();

    //Insert the doc for the different contexts.

    li.import_namespace(ctx_utils);
    li.import_namespace(ctx_io);
    li.import_namespace(ctx_math);
    li.import_namespace(ctx_string);
    li.import_namespace(ModTime::new(2));

    let ctx_rae = ModMonitor::new(
        PlatformGobotSim::new(
            LispDomain::File(opt.domain.unwrap_or(default_gobot_sim_plan_domain().into())),
            !opt.view,
            LogClient::new(PLATFORM_CLIENT, LOG_TOPIC_PLATFORM).await,
        ),
        None,
    )
    .await;
    li.import_namespace(ctx_rae);

    tokio::spawn(li.run(None));
    Master::wait_end().await;

    Ok(())
}
