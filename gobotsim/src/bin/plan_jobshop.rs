//use ompas_gobotsim::mod_godot::CtxGodot;
use ompas_core::ompas::scheme::exec::platform::lisp_domain::LispDomain;
use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_gobotsim::platform::PlatformGobotSim;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PLATFORM_CLIENT};
use ompas_middleware::logger::LogClient;
use ompas_middleware::{LogLevel, Master};
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::{ChannelToLispInterpreter, LispInterpreter};
use sompas_structs::lruntimeerror;
use std::path::PathBuf;
use structopt::StructOpt;
pub const LOG_LEVEL: LogLevel = LogLevel::Debug;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
struct Opt {
    #[structopt(short = "f", long = "file")]
    file: Option<PathBuf>,

    #[structopt(short = "o", long = "opt")]
    opt: bool,
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
    li.import(ctx_string);

    let ctx_rae = ModMonitor::new(
        PlatformGobotSim::new(
            LispDomain::File(opt.file.unwrap_or(
                "/home/jeremy/CLionProjects/ompas/gobotsim/planning_domain/domain.lisp".into(),
            )),
            false,
            LogClient::new(PLATFORM_CLIENT, LOG_TOPIC_PLATFORM).await,
        ),
        None,
    )
    .await;
    li.import_namespace(ctx_rae);

    let mut channel: ChannelToLispInterpreter = li.subscribe();

    //li.set_config(LispInterpreterConfig::new(true));

    tokio::spawn(li.run(None));

    let command = match opt.opt {
        false => "(plan-task t_jobshop)".to_string(),
        true => "(plan-task-opt t_jobshop)".to_string(),
    };

    channel.send(command).await.unwrap();
    channel.recv().await.unwrap()?;
    channel.send("(dump_trace)".to_string()).await.unwrap();
    channel.recv().await.unwrap()?;

    Master::end().await;

    Ok(())
}
