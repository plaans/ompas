use ompas_gobotsim::platform::PlatformGobotSim;
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_rae_core::monitor::ModMonitor;
use ompas_rae_interface::lisp_domain::LispDomain;
use ompas_rae_language::interface::{LOG_TOPIC_PLATFORM, PLATFORM_CLIENT};
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::{
    ChannelToLispInterpreter, LispInterpreter, LispInterpreterConfig,
};
use std::path::PathBuf;
use std::{env, fs};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
pub struct Opt {
    #[structopt(short = "l", long = "log-path")]
    log: Option<PathBuf>,

    #[structopt(short = "d", long = "domain")]
    domain: PathBuf,

    #[structopt(short = "p", long = "problem")]
    problem: Option<PathBuf>,

    #[structopt(short = "a", long = "advanced")]
    advanced: bool,

    #[structopt(short = "L", long = "lrpt")]
    lrpt: bool,

    #[structopt(short = "v", long = "view")]
    view: bool,

    #[structopt(short = "t", long = "time")]
    time: Option<usize>,
}

#[tokio::main]
async fn main() {
    println!("OMPAS v0.1");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    //test_lib_model(&opt);
    lisp_interpreter(opt).await;
}

pub async fn lisp_interpreter(opt: Opt) {
    let path = fs::canonicalize(opt.domain.clone()).expect("path to domain is unvalid");

    println!("domain: {:?}", path);
    let problem = match &opt.problem {
        Some(p) => p.clone(),
        None => {
            println!("Searching for problem files...");
            let mut problem_path = path.clone();
            problem_path.push("problems");
            println!("problems path: {:?}", problem_path);
            let mut paths = fs::read_dir(problem_path).unwrap();
            if let Some(path) = paths.next() {
                path.unwrap().path()
            } else {
                panic!("no problem in subdirectory. Please specify a file path for benchmark.")
            }
        }
    };
    println!("problem : {:?}", problem);

    let log = if let Some(pb) = &opt.log {
        pb.clone()
    } else {
        let home = env::var("HOME").unwrap();
        PathBuf::from(format!("{}/ompas/benchmark", home))
    };

    //println!("log path: {:?}", log);

    let mut li = LispInterpreter::new().await;

    let mut ctx_io = ModIO::default();
    let ctx_math = ModAdvancedMath::default();
    let ctx_utils = ModUtils::default();
    let ctx_string = ModString::default();

    //Insert the doc for the different contexts.

    //Add the sender of the channel.

    ctx_io.set_log_output(log.clone().into());

    li.import_namespace(ctx_utils);
    li.import_namespace(ctx_io);
    li.import_namespace(ctx_math);

    li.import(ctx_string);

    let log = if let Some(pb) = &opt.log {
        pb.clone()
    } else {
        let home = env::var("HOME").unwrap();
        PathBuf::from(format!("{}/ompas/benchmark", home))
    };

    let ctx_rae = ModMonitor::new(
        PlatformGobotSim::new(
            domain(&opt),
            !opt.view,
            LogClient::new(PLATFORM_CLIENT, LOG_TOPIC_PLATFORM).await,
        ),
        Some(log.clone()),
    )
    .await;
    li.import_namespace(ctx_rae);

    let mut com: ChannelToLispInterpreter = li.subscribe();

    let problem_lisp =
        fs::read_to_string(problem.clone()).expect("Something went wrong reading the file");

    li.set_config(LispInterpreterConfig::new(false));
    tokio::spawn(async move {
        li.run(Some(FileDescriptor::AbsolutePath(
            log.canonicalize().unwrap(),
        )))
        .await;
    });

    //tokio::time::sleep(Duration::from_secs(time)).await;
    //com.send(domain_lisp).await.expect("could not send to LI");
    com.send(problem_lisp).await.expect("could not send to LI");
    com.recv().await;
    com.send("(launch)".to_string()).await.expect("error on LI");
    com.recv().await;
    println!("bench: task starts");
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
    /*com.send("(await (trigger-task t_jobshop))".to_string())
    .await
    .expect("error on LI");*/

    com.send(format!(
        "(await (race (await (trigger-task t_jobshop)) (await (sleep {}))))",
        opt.time.unwrap_or(1)
    ))
    .await
    .expect("error on LI");

    /*select! {
        _ = com.recv() => {
            println!("task finished")
        }
        _ = tokio::time::sleep(Duration::from_secs()=> {
            println!("Benchmark stopped before completion")
        }
    }*/
    com.recv().await;

    println!("bench: task ends");
    let problem_name = problem.file_name().unwrap().to_str().unwrap();
    let problem_name = problem_name.replace(".lisp", "");
    com.send(format!(
        "(export-stats gobot-sim_{}_{})",
        if opt.advanced {
            "advanced"
        } else if opt.lrpt {
            "lrpt"
        } else {
            "greedy"
        },
        problem_name
    ))
    .await
    .expect("could not send to LI");

    com.recv().await;
    com.send("exit".to_string())
        .await
        .expect("could not send to LI");

    com.recv().await;
    /*while com.recv().await.is_some() {
        //println!("{}", m)
    }*/

    println!(
        "end of the benchmark :  {} for domain gobot-sim",
        problem_name
    );
}

pub fn domain(opt: &Opt) -> LispDomain {
    let domain = opt.domain.clone();
    let mut commands = domain.clone();
    commands.push("commands.lisp");
    let mut state_functions = domain.clone();
    state_functions.push("state_functions.lisp");
    let mut om = domain.clone();
    om.push(if opt.advanced {
        "jobshop_advanced.lisp"
    } else if opt.lrpt {
        "jobshop_advanced_lrpt.lisp"
    } else {
        "jobshop_greedy.lisp"
    });
    let mut common = domain.clone();
    common.push("jobshop_common.lisp");

    let mut lambdas = domain;
    lambdas.push("lambdas.lisp");

    format!(
        "(begin
        (read \"{}\")
        (read \"{}\")
        (read \"{}\")
        (read \"{}\")
        (read \"{}\"))",
        commands.to_str().unwrap(),
        state_functions.to_str().unwrap(),
        lambdas.to_str().unwrap(),
        common.to_str().unwrap(),
        om.to_str().unwrap()
    )
    .into()
}
