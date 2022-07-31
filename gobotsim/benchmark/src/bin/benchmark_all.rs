use colored::Colorize;
use gobot_sim_benchmark::{BenchmarkData, RunData};
use indicatif::{ProgressBar, ProgressStyle};
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};
use std::fmt::Debug;
use std::fs;
use std::fs::File;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, SystemTime};
use structopt::StructOpt;
use yaml_rust::YamlLoader;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
pub struct Opt {
    //Number of greedy that will be runned as greedy choose randomly on a set of equal scored methods
    #[structopt(short = "n", long = "number")]
    number: Option<usize>,

    #[structopt(short = "d", long = "domain")]
    domain: PathBuf,

    #[structopt(short = "c", long = "config")]
    config: PathBuf,

    #[structopt(short = "v", long = "view")]
    view: bool,

    #[structopt(short = "t", long = "time")]
    time: Option<usize>,
}

fn main() {
    let mut benchmark_data = BenchmarkData::default();
    let bar = ProgressBar::new(0);
    bar.set_style(
        ProgressStyle::with_template(
            "[{elapsed_precise}/{eta_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}",
        )
        .unwrap()
        .progress_chars("##-"),
    );
    bar.enable_steady_tick(Duration::from_secs(1));
    bar.reset();
    let opt: Opt = Opt::from_args();
    println!("GOBOT-SIM+RAE BENCHMARK v0.1");
    //Installation of the last version of benchmark

    println!("Installation of the last version of OMPAS' benchmark...");
    let child = Command::new("cargo")
        .args(&[
            "install",
            "--force",
            "--bin",
            "gobot-benchmark",
            "--path",
            ".",
        ])
        .spawn();

    if let Ok(mut c) = child {
        bar.println("Spawned successfully");
        bar.println(format!("Exit with: {:?}", c.wait()));
    } else {
        panic!("panic");
    }
    bar.println("Benchmark installed !");

    //let time = opt.time.unwrap_or(DEFAULT_TIME);

    //println!("Benchmark for domain: {}", domain.to_str().unwrap());
    let domain_path: PathBuf = opt.domain;
    bar.println("Searching for problem files...");
    let mut problem_path = domain_path.clone();
    problem_path.push("problems");
    let paths = fs::read_dir(problem_path).expect("directory of domain not found...");
    let mut problems = vec![];
    for path in paths {
        problems.push(path.unwrap().path())
    }
    if problems.is_empty() {
        bar.println("No problem found...");
    } else {
        bar.println("Problems found:");
        for p in &problems {
            bar.println(format!("- {}", p.to_str().unwrap()));
        }
    }

    let mut first = true;

    let modes = ["-a", "-L", ""];
    let number = opt.number.unwrap_or(1);
    let n_problem = modes.len() * problems.len() * number;
    bar.set_length(n_problem.try_into().unwrap());

    for problem in problems {
        let mut runs = vec![];
        for mode in modes {
            runs.append(&mut vec![mode; number]);
        }

        for select in runs {
            let start = SystemTime::now();
            if !first {
                std::thread::sleep(Duration::from_secs(5));
            } else {
                first = false
            }
            let mut command = Command::new("gobot-benchmark");
            command.args(&[
                "-d",
                domain_path.to_str().unwrap(),
                "-p",
                problem.to_str().unwrap(),
            ]);

            if opt.view {
                command.arg("-v");
            }
            command.args(["-t", format!("{}", opt.time.unwrap_or(1)).as_str()]);
            let f1 = File::create("benchmark.log").expect("couldn't create file");
            let f2 = File::create("benchmark.log").expect("couldn't create file");
            command
                .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) });
            if select != "" {
                command.arg(select);
            }
            let debug = format!("{:?}", command);
            if let Ok(mut c) = command.spawn() {
                bar.println(format!(
                    "{} {} {}, config: {}",
                    "[benchmark]".bold().green(),
                    "gobot-sim",
                    problem.file_name().unwrap().to_str().unwrap(),
                    select
                ));
                let result = c.wait();
                //println!("Exit with: {:?}", c.wait());
            } else {
                panic!("panic");
            }
            let run = RunData {
                duration: start.elapsed().unwrap(),
            };
            benchmark_data.runs.push(run);
            bar.inc(1);
        }
    }
    bar.finish();
    send_email(&opt.config, benchmark_data.format_data())
}

fn send_email(config: &PathBuf, message: String) {
    println!("sending email");
    let config = fs::read_to_string(config).expect("Something went wrong reading the config file");
    let configs = YamlLoader::load_from_str(&config).unwrap();
    let config = &configs[0];

    let email = Message::builder()
        .from(config["from"].as_str().unwrap().parse().unwrap())
        .to(config["to"].as_str().unwrap().parse().unwrap())
        .subject("GobotSim Benchmark")
        .body(message)
        .unwrap();

    let creds = Credentials::new(
        config["from"].as_str().unwrap().to_string(),
        config["password"].as_str().unwrap().to_string(),
    );

    // Open a remote connection to gmail
    let mailer = SmtpTransport::relay(config["server"].as_str().unwrap())
        .unwrap()
        .credentials(creds)
        .build();

    // Send the email
    match mailer.send(&email) {
        Ok(_) => println!("Email sent successfully!"),
        Err(e) => panic!("Could not send email: {:?}", e),
    }
}
