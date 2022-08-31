use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
pub struct Opt {
    #[structopt(short = "d", long = "domains")]
    domains: Vec<PathBuf>,

    #[structopt(short = "t", long = "time")]
    time: Option<u64>,

    //Number of greedy that will be runned as greedy choose randomly on a set of equal scored methods
    #[structopt(short = "g", long = "greedy")]
    greedy: Option<usize>,

    #[structopt(short = "a", long = "aries")]
    aries: Option<usize>,
}

const DEFAULT_TIME: u64 = 1;

fn main() {
    let opt: Opt = Opt::from_args();
    println!("OMPAS BENCHMARK v0.1");
    //Installation of the last version of benchmark

    let mut current_dir = env::current_dir().unwrap();
    current_dir.push("../rae/apps");
    assert!(env::set_current_dir(&current_dir).is_ok());
    println!(
        "Successfully changed working directory to {} !",
        current_dir.display()
    );

    println!("Installation of the last version of OMPAS' benchmark...");
    let child = Command::new("cargo")
        .args(&["install", "--bin", "benchmark", "--path", "."])
        .spawn();

    if let Ok(mut c) = child {
        println!("Spawned successfully");
        println!("Exit with: {:?}", c.wait());
    } else {
        panic!("panic");
    }
    println!("Benchmark installed !");

    let mut current_dir = env::current_dir().unwrap();
    current_dir.pop();
    assert!(env::set_current_dir(&current_dir).is_ok());
    println!(
        "Successfully changed working directory to {} !",
        current_dir.display()
    );

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
            let mut vec: Vec<&str> = vec![""; opt.greedy.unwrap_or(1)];
            vec.append(&mut vec!["-a"; opt.aries.unwrap_or(1)]);
            vec.push("-o");

            for select in &vec {
                let mut command = Command::new("benchmark");
                command.args(&[
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
}
