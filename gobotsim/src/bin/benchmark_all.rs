use std::path::PathBuf;
use std::process::Command;
use std::{env, fs};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
pub struct Opt {
    //Number of greedy that will be runned as greedy choose randomly on a set of equal scored methods
    #[structopt(short = "n", long = "number")]
    number: Option<usize>,
}

const DEFAULT_TIME: u64 = 1;

fn main() {
    let opt: Opt = Opt::from_args();
    println!("GOBOT-SIM+RAE BENCHMARK v0.1");
    //Installation of the last version of benchmark

    println!("Installation of the last version of OMPAS' benchmark...");
    let child = Command::new("cargo")
        .args(&["install", "--bin", "gobot-benchmark", "--path", "."])
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
    let domain_path: PathBuf = "/home/jeremy/CLionProjects/ompas/gobotsim/godot_domain".into();
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
        let mut vec: Vec<&str> = vec!["-a"; opt.number.unwrap_or(1)];
        vec.append(&mut vec![""; opt.number.unwrap_or(1)]);
        //vec.push("-o");

        for select in &vec {
            let mut command = Command::new("gobot-benchmark");
            command.args(&[
                "-d",
                domain_path.to_str().unwrap(),
                "-p",
                problem.to_str().unwrap(),
                //"-t",
                //time.to_string().as_str(),
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
            std::thread::sleep(std::time::Duration::from_secs(5));
        }
    }
}
