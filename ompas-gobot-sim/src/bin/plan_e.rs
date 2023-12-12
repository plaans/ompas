use ompas_core::ompas::manager::platform::scheme_domain::SchemeDomain;
use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_gobotsim::default_gobot_sim_plan_domain_e;
use ompas_gobotsim::platform::PlatformGobotSim;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PLATFORM_CLIENT};
use ompas_middleware::logger::LogClient;
use ompas_middleware::{LogLevel, Master};
use sompas_modules::ModExtendedStd;
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

    li.import_namespace(ModExtendedStd::default());

    let ctx_rae = ModMonitor::new(
        PlatformGobotSim::new(
            SchemeDomain::File(
                opt.domain
                    .unwrap_or(default_gobot_sim_plan_domain_e().into()),
            ),
            false,
            LogClient::new(PLATFORM_CLIENT, LOG_TOPIC_PLATFORM).await,
        ),
        None,
    )
    .await;
    li.import_namespace(ctx_rae);

    //let mut channel: ChannelToLispInterpreter = li.subscribe();

    //li.set_config(LispInterpreterConfig::new(true));

    tokio::spawn(li.run(None));
    /*let command = format!(
        "(begin (start) (sleep 2)\n
         {}\n
         )",
        match opt.opt {
            false => "(plan-task t_jobshop)",
            true => "(plan-task-opt t_jobshop)",
        }
    );

    channel.send(command).await.unwrap();
    let result: LValue = channel.recv().await.unwrap()?;
    println!("finished: {}", result);

    Master::end().await;*/
    Master::wait_end().await;

    Ok(())
}
