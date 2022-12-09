use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};
use ompas_benchmark::config::BenchConfig;
use ompas_benchmark::{install_binary, send_email, BenchmarkData, RunData};
use std::convert::TryInto;
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
    #[structopt(short = "c", long = "config")]
    config: PathBuf,
}

fn main() {
    let opt: Opt = Opt::from_args();
    let config =
        fs::read_to_string(&opt.config).expect("Something went wrong reading the config file");
    let configs = YamlLoader::load_from_str(&config).unwrap();
    let config = &configs[0];
    let config: BenchConfig = config.try_into().expect("could not read config");

    let mut benchmark_data = BenchmarkData::default();
    let bar = ProgressBar::new(0);
    bar.set_style(
        ProgressStyle::with_template(
            "[{elapsed_precise}/{eta_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}",
        )
        .unwrap()
        .progress_chars("##-"),
    );

    bar.println("GOBOT-SIM+RAE BENCHMARK v0.1");
    //Installation of the last version of benchmark
    install_binary(config.get_bin_name(), config.bin_path.clone());

    bar.println("Benchmark installed !");

    //let time = opt.time.unwrap_or(DEFAULT_TIME);

    //println!("Benchmark for domain: {}", domain.to_str().unwrap());
    let domain_path: PathBuf = config.domain_path.clone();
    let mut problems = vec![];
    if config.problems.is_empty() {
        bar.println("Searching for problem files...");
        let mut problem_path = domain_path.clone();
        problem_path.push("problems");
        let paths = fs::read_dir(problem_path).expect("directory of domain not found...");
        for path in paths {
            problems.push(path.unwrap().path())
        }
    } else {
        problems = config.problems.clone();
        let mut problem_path = domain_path.clone();
        problem_path.push("problems");
        for p in &mut problems {
            //println!("problem: {:#?}", p);
            //println!("parent:{:#?}", p.parent());
            if p.parent().is_none() {
                println!("gonna transform into a complete path");
                let mut new_path = problem_path.clone();
                new_path.push(p.clone());
                *p = new_path;
            }
        }
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

    let specific = config.get_specific();
    let number: usize = config.number as usize;
    let n_problem = specific.len() * problems.len() * number;
    bar.enable_steady_tick(Duration::from_secs(1));
    bar.reset();
    bar.set_length(n_problem.try_into().unwrap());

    for problem in problems {
        let mut runs = vec![];
        for spec in &specific {
            runs.append(&mut vec![spec; number]);
        }

        for select in runs {
            let start = SystemTime::now();
            if !first {
                std::thread::sleep(Duration::from_secs(5));
            } else {
                first = false
            }
            let mut command = Command::new(config.get_bin_name());
            command.args([
                "-d",
                domain_path.to_str().unwrap(),
                "-p",
                problem.to_str().unwrap(),
            ]);
            command.args(["-t", format!("{}", config.max_time).as_str()]);
            let f1 = File::create("benchmark.log").expect("couldn't create file");
            let f2 = File::create("benchmark.log").expect("couldn't create file");
            command
                .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) });
            if !select.is_empty() {
                command.arg(select);
            }
            //let debug = format!("{:?}", command);
            if let Ok(mut c) = command.spawn() {
                bar.println(format!(
                    "{} {} {}, config: {}",
                    "[benchmark]".bold().green(),
                    config.get_bin_name(),
                    problem.file_name().unwrap().to_str().unwrap(),
                    select
                ));
                let _r = c.wait();
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
    send_email(
        &config.mail,
        benchmark_data.format_data("GOBOT-SIM BENCHMARK".to_string()),
    )
}
