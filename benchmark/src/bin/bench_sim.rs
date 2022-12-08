use ompas_rae_core::monitor::ModMonitor;
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::ModIO;
use sompas_modules::string::ModString;
use sompas_modules::utils::ModUtils;
use sompas_repl::lisp_interpreter::{
    ChannelToLispInterpreter, LispInterpreter, LispInterpreterConfig,
};
use std::path::PathBuf;
use std::time::Duration;
use std::{env, fs};
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
pub struct Opt {
    #[structopt(short = "l", long = "log-path")]
    log: Option<PathBuf>,

    #[structopt(short = "d", long = "domain")]
    domain: PathBuf,

    #[structopt(short = "p", long = "problem")]
    problem: Option<PathBuf>,

    #[structopt(short = "t", long = "time")]
    time: Option<u64>,

    #[structopt(short = "a", long = "aries")]
    aries: bool,

    #[structopt(short = "o", long = "aries-opt")]
    aries_opt: bool,

    #[structopt(short = "v", long = "view")]
    view: bool,
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
    let mut domain_file_path = path.clone();
    domain_file_path.push("domain.lisp");
    println!("domain: {:?}", path);
    let problem = match opt.problem {
        Some(p) => p,
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

    let time = opt.time.unwrap_or(60);

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
    let ctx_rae = ModMonitor::new(None, Some(log.clone()), opt.view).await;
    li.import_namespace(ctx_rae);

    let mut com: ChannelToLispInterpreter = li.subscribe();
    let domain_lisp = fs::read_to_string(domain_file_path.clone())
        .expect("Something went wrong reading the file");
    let problem_lisp =
        fs::read_to_string(problem.clone()).expect("Something went wrong reading the file");

    if opt.aries {
        com.send("(configure-select aries)".to_string())
            .await
            .expect("error on LI");
    } else if opt.aries_opt {
        com.send("(configure-select aries-opt)".to_string())
            .await
            .expect("error on LI");
    }

    li.set_config(LispInterpreterConfig::new(false));

    tokio::spawn(async move {
        li.run(Some(log)).await;
    });

    com.send(domain_lisp).await.expect("could not send to LI");
    com.recv().await.expect("").expect("");
    com.send(problem_lisp).await.expect("could not send to LI");
    com.recv().await.expect("").expect("");
    com.send("(launch)".to_string()).await.expect("error on LI");
    com.recv().await.expect("").expect("");

    tokio::time::sleep(Duration::from_secs(time)).await;
    let problem_name = problem.file_name().unwrap().to_str().unwrap();
    let domain_name = opt.domain.file_name().unwrap().to_str().unwrap();
    let problem_name = problem_name.replace(".lisp", "");
    let domain_name = domain_name.replace(".lisp", "");
    com.send(format!(
        "(export-stats {}_{}_{}s)",
        domain_name, problem_name, time
    ))
    .await
    .expect("could not send to LI");

    com.send("exit".to_string())
        .await
        .expect("could not send to LI");

    println!(
        "end of the benchmark of li for {} of domain {}",
        problem_name, domain_name
    );
}
