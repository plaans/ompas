use gobot_sim_benchmark::{BenchmarkData, RunData};
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};
use std::fs;
use std::fs::File;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::SystemTime;
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
}

fn main() {
    let mut benchmark_data = BenchmarkData::default();

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
        println!("Spawned successfully");
        println!("Exit with: {:?}", c.wait());
    } else {
        panic!("panic");
    }
    println!("Benchmark installed !");

    //let time = opt.time.unwrap_or(DEFAULT_TIME);

    //println!("Benchmark for domain: {}", domain.to_str().unwrap());
    let domain_path: PathBuf = opt.domain;
    println!("Searching for problem files...");
    let mut problem_path = domain_path.clone();
    problem_path.push("problems");
    let paths = fs::read_dir(problem_path).expect("directory of domain not found...");
    let mut problems = vec![];
    for path in paths {
        problems.push(path.unwrap().path())
    }
    println!("Problems found : {:#?}", problems);
    let mut first = true;

    for problem in problems {
        let start = SystemTime::now();

        let mut vec: Vec<&str> = vec!["-a"; opt.number.unwrap_or(1)];
        //vec.append(&mut vec!["-L"; opt.number.unwrap_or(1)]);
        //vec.append(&mut vec![""; opt.number.unwrap_or(1)]);
        //vec.push("-o");

        for select in &vec {
            if !first {
                std::thread::sleep(std::time::Duration::from_secs(5));
            } else {
                first = false
            }
            let mut command = Command::new("gobot-benchmark");
            command.args(&[
                "-d",
                domain_path.to_str().unwrap(),
                "-p",
                problem.to_str().unwrap(),
                //"-t",
                //time.to_string().as_str(),
            ]);
            let f1 = File::create("benchmark.log").expect("couldn't create file");
            let f2 = File::create("benchmark.log").expect("couldn't create file");
            command
                .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) });
            if select != &"" {
                command.arg(select);
            }
            if let Ok(mut c) = command.spawn() {
                println!("Spawned successfully");
                println!("Exit with: {:?}", c.wait());
            } else {
                panic!("panic");
            }
        }

        let run = RunData {
            duration: start.elapsed().unwrap(),
        };
        benchmark_data.runs.push(run)
    }
    send_email(&opt.config, benchmark_data.format_data())
}

fn format_data() -> String {
    "Benchmark terminated!".to_string()
}

fn send_email(config: &PathBuf, message: String) {
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
