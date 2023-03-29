use chrono::{DateTime, Utc};
use ompas_core::ompas::scheme::exec::platform::lisp_domain::LispDomain;
use ompas_core::ompas::scheme::monitor::ModMonitor;
use ompas_gobotsim::platform::PlatformGobotSim;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PLATFORM_CLIENT};
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::Master;
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
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

    #[structopt(short = "f", long = "fa")]
    fa: bool,

    #[structopt(short = "L", long = "lrpt")]
    lrpt: bool,

    #[structopt(short = "a", long = "aries")]
    aries: bool,

    #[structopt(short = "o", long = "aries-opt")]
    aries_opt: bool,

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
        PathBuf::from(format!("{}/ompas_benchmark", home))
    };

    //println!("log path: {:?}", log);

    let mut li = LispInterpreter::new().await;

    let mut ctx_io = ModIO::default();
    let ctx_math = ModAdvancedMath::default();
    let ctx_utils = ModUtils::default();
    let ctx_string = ModString::default();
    let mod_time = ModTime::new(2);

    //Insert the doc for the different contexts.

    //Add the sender of the channel.

    ctx_io.set_log_output(log.clone().into());

    li.import_namespace(ctx_utils);
    li.import_namespace(ctx_io);
    li.import_namespace(ctx_math);
    li.import_namespace(mod_time);

    li.import(ctx_string);

    let mut log = if let Some(pb) = &opt.log {
        pb.clone()
    } else {
        let home = env::var("HOME").unwrap();
        PathBuf::from(format!("{}/ompas_benchmark", home))
    };

    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();

    log.push(string_date);
    fs::create_dir_all(log.clone()).unwrap();

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
        li.run(Some(FileDescriptor::Directory(log.canonicalize().unwrap())))
            .await;
    });

    com.send(problem_lisp).await.expect("could not send to LI");
    let r = com.recv().await.unwrap();
    println!(
        "{}",
        match r {
            Ok(lv) => lv.to_string(),
            Err(e) => e.to_string(),
        }
    );
    let start = if opt.aries {
        "(start-with-planner false)"
    } else if opt.aries_opt {
        "(start-with-planner true)"
    } else {
        "(start)"
    };
    com.send(start.to_string()).await.expect("error on LI");
    com.recv().await;
    println!("bench: task starts");
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

    //let trigger = "(trigger-task t_jobshop)".to_string();
    let trigger = format!(
        "(await (race (wait-task (trigger-task t_jobshop)) (sleep {})))",
        opt.time.unwrap_or(1)
    );
    //println!("trigger = {}", trigger);

    com.send(trigger).await.expect("error on LI");
    com.recv().await;

    println!("bench: task ends");
    let problem_name = problem.file_name().unwrap().to_str().unwrap();
    let problem_name = problem_name.replace(".lisp", "");
    com.send(format!(
        "(export-stats gobot-sim_{}_{})",
        if opt.fa {
            "advanced"
        } else if opt.lrpt {
            "lrpt"
        } else if opt.aries {
            "aries"
        } else if opt.aries_opt {
            "aries_opt"
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

    Master::end().await
}

pub fn domain(opt: &Opt) -> LispDomain {
    let domain = opt.domain.clone();
    let mut om = domain.clone();
    om.push(if opt.fa {
        "fa.lisp"
    } else if opt.lrpt {
        "falrpt.lisp"
    } else {
        "greedy.lisp"
    });
    let mut base = domain.clone();
    base.push("base.lisp");

    let mut jobshop = domain.clone();
    jobshop.push("jobshop.lisp");

    let domain = format!(
        "(begin
        (read \"{}\")
        (read \"{}\")
        (read \"{}\"))",
        base.to_str().unwrap(),
        jobshop.to_str().unwrap(),
        om.to_str().unwrap(),
    );

    //println!("{domain}");
    domain.into()
}
