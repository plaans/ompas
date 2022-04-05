use std::fs;
use std::path::PathBuf;
use std::process::Command;
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
pub struct Opt {
    #[structopt(short = "d", long = "domains")]
    domains: Vec<PathBuf>,

    #[structopt(short = "t", long = "time")]
    time: Option<u64>,
}

const DEFAULT_TIME: u64 = 60;

fn main() {
    let opt: Opt = Opt::from_args();
    println!("OMPAS BENCHMARK v0.1");
    println!("Domains to benchmark: {:#?}", opt.domains);

    let time = opt.time.unwrap_or(DEFAULT_TIME);

    for domain in &opt.domains {
        println!("Benchmark for domain: {}", domain.to_str().unwrap());
        let domain_path: PathBuf = format!("instances/{}", domain.to_str().unwrap()).into();
        println!("Searching for problem files...");
        let mut problem_path = domain_path.clone();
        problem_path.push("problems");
        let paths = fs::read_dir(problem_path).expect("directory of domain not found...");
        let mut problems = vec![];
        for path in paths {
            problems.push(path.unwrap().path())
        }
        println!("Problems found : {:#?}", problems);
        for problem in problems {
            for select in &["", "-a", "-o"] {
                let mut command = Command::new("cargo");
                command.args(&[
                    "run",
                    "--release",
                    "--bin",
                    "benchmark",
                    "--",
                    "-d",
                    domain_path.to_str().unwrap(),
                    "-p",
                    problem.to_str().unwrap(),
                    "-t",
                    time.to_string().as_str(),
                ]);
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
        }
    }

    /*
    if let Ok(mut c) = Command::new("cargo")
        .args(&[
            "run",
            "--release",
            "--bin",
            "benchmark",
            "--",
            "-d",
            "instances/gripper",
            "-t",
            "1",
            "-a",
        ])
        .spawn()
    {
        println!("Spawned successfully");
        println!("Exit with: {:?}", c.wait());
    } else {
        panic!("panic");
    }*/
}
