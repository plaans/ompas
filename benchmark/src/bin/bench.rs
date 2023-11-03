use colored::Colorize;
use indicatif::{ProgressBar, ProgressStyle};
use ompas_benchmark::config::job_config::{
    ContinuousPlanningConfig, HeuristicConfig, JobConfig, SelectConfig,
};
use ompas_benchmark::config::BenchConfig;
use ompas_benchmark::{install_binary, send_email, BenchmarkData, RunData};
use ompas_core::OMPAS_PATH;
use ompas_middleware::OMPAS_WORKING_DIR;
use std::convert::TryInto;
use std::fmt::Debug;
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::{Path, PathBuf};
use tokio::process::{Command};
use std::process::Stdio;
use std::time::{Duration, SystemTime};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "bench", about = "Program that benchmarks the OMPAS system.")]
pub struct Opt {
    #[structopt(short = "c", long = "config")]
    config: PathBuf,
}

pub fn benchmark_working_dir() -> PathBuf {
    let path: PathBuf = OMPAS_WORKING_DIR.get_ref().into();
    let mut path = path.canonicalize().unwrap();
    path.push("benchmark");
    fs::create_dir_all(&path).unwrap();
    path
}

pub struct RunConfig {
    label: String,
    start: String,
    path: PathBuf,
}

#[derive(Clone)]
pub struct Problem {
    label: String,
    path: PathBuf,
}

#[tokio::main]
async fn main() {
    let opt: Opt = Opt::from_args();
    let str =
        fs::read_to_string(&opt.config).expect("Something went wrong reading the config file");
    let config: BenchConfig = serde_yaml::from_str(&str).expect("Could not parse yaml config");

    let mut benchmark_data = BenchmarkData::default();
    let bar = ProgressBar::new(0);
    bar.set_style(
        ProgressStyle::with_template(
            "[{elapsed_precise}/{eta_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}",
        )
        .unwrap()
        .progress_chars("##-"),
    );

    bar.println("ompas-benchmark");
    {
        bar.println("Installing OMPAS binary...");
        let mut path: PathBuf = OMPAS_PATH.get_ref().into();
        path.push("acting");
        path.push("apps");
        install_binary("ompas".to_string(), path);
        bar.println("OMPAS installed !");
    }

    bar.println(format!("{} jobs to benchmark.", config.jobs.len()));
    'loop_job: for JobConfig {
        domain,
        problem_config,
        heuristics,
        timeout,
        n_run,
    } in &config.jobs
    {

        let mut job_dir =  benchmark_working_dir();
        job_dir.push(&domain.label);

        let mut benchmark_log_dir = job_dir.clone();
        benchmark_log_dir.push("benchmark_log");
        fs::create_dir_all(&benchmark_log_dir).unwrap();
        let domain_path = match domain.domain_path.canonicalize() {
            Ok(p) => p,
            Err(e) => {
                bar.println(format!("Error with domain_path of {}: {}",domain.label ,e));
                bar.println(format!("Skipping job {}", domain.label));
                continue 'loop_job;
            }
        };

        bar.println(format!("Benchmarking {} ", domain.label));
        if let Some(binary) = &domain.binary {
            bar.println("Specific ompas binary to install...");
            install_binary(&binary.label, binary.path.clone());
            bar.println("Binary installed !");
        }

        // Getting all the problem files
        let problem_dir_path = match problem_config.problem_dir_path.canonicalize() {
            Ok(p) => p,
            Err(e) => {
                bar.println(format!("Error with problem_dir_path of {}: {}",domain.label ,e));
                bar.println(format!("Skipping job {}", domain.label));
                continue 'loop_job;
            }
        };

        bar.println("Searching for problem files...");
        let mut problems = get_all_problems_files(&problem_dir_path);
        if let Some(specific_problems) = &problem_config.specific_problems {
            bar.println("Getting specific problems");
            let mut filtered = vec![];
            for problem in specific_problems {
                match problems.iter().find(|p| &p.label == problem) {
                    Some(p) => filtered.push(p.clone()),
                    None => bar.println(format!("Could not find problem {}", problem)),
                }
            }
            problems = filtered
        }

        if problems.is_empty() {
            continue 'loop_job;
        } else {
            bar.println(format!("Found following problems for job {}", domain.label));
            for p in &problems {
                bar.println(format!("- {}", p.path.display()));
            }
        }

        //Generate lisp code for all possible configs
        bar.println("Generating files for heuristics...");
        let mut configs: Vec<RunConfig> = vec![];
        let mut run_config_path = job_dir.clone();
        run_config_path.push("run_config");
        fs::create_dir_all(&run_config_path).unwrap();
        for heuristic in heuristics {
            let (label, code, start) = generate_config(heuristic, &domain_path);
            //Store in file
            let mut config_path = run_config_path.clone();
            config_path.push(format!("{}.lisp", label));
            let mut file = OpenOptions::new()
                .create(true)
                .truncate(true)
                .write(true)
                .open(config_path.clone())
                .unwrap();
            file.write_all(code.as_bytes()).unwrap();

            //Store file name in run_configs
            configs.push(RunConfig {
                label,
                start,
                path: config_path,
            })
        }
        bar.println("Done!");

        let n_problem = configs.len() * problems.len() * (*n_run as usize);
        bar.println(format!("{} run to do...", n_problem));
        bar.enable_steady_tick(Duration::from_secs(1));
        bar.reset();
        bar.set_length(n_problem.try_into().unwrap());

        let mut first = true;
        let mut i = 1;
        let mut problems_path = job_dir.clone();
        problems_path.push("problems");
        fs::create_dir_all(&problems_path).unwrap();

        for problem in &problems {
            let problem_path = problem.path.to_str().unwrap();
            for config in &configs {
                let config_path = config.path.to_str().unwrap();

                let mut problem_config_path: PathBuf = problems_path.clone();
                let problem_config_name = format!("{}_{}", problem.label.replace(".lisp", ""), config.label);
                problem_config_path.push(format!("{}.lisp", problem_config_name));
                let mut file = OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(problem_config_path.clone())
                    .unwrap();
                let content = format!(
"(begin
    (read \"{}\")
    (read \"{}\")
    {}
    (wait-end-all {})
    (export-report {})
    (exit 0))",
                    config_path, problem_path, config.start, timeout, problem_config_name,
                );
                file.write_all(content.as_bytes()).unwrap();

                let config_path = problem_config_path.to_str().unwrap();

                for _ in 0..*n_run {
                    if !first {
                        std::thread::sleep(Duration::from_secs(2));
                    } else {
                        first = false
                    }
                    let start = SystemTime::now();


                    let mut command = Command::new(domain.get_binary());
                    command.args(["-d", config_path]);
                    command.env("OMPAS_WORKING_DIR", job_dir.to_str().unwrap());

                    bar.println(format!("command: {:?}", command));

                    let mut benchmark_log_file = benchmark_log_dir.clone();
                    benchmark_log_file.push(format!("benchmark_{}.log", i));

                    //println!("log file: {}", benchmark_log_file.to_str().unwrap());

                    let f1 = File::create(&benchmark_log_file).expect("couldn't create file");
                    let f2 = File::create(&benchmark_log_file).expect("couldn't create file");
                    command
                        .stdout(unsafe { Stdio::from_raw_fd(f1.into_raw_fd()) })
                        .stderr(unsafe { Stdio::from_raw_fd(f2.into_raw_fd()) });

                    if let Ok(mut c) = command.spawn() {
                        bar.println(format!(
                            "[{} {}]: problem = {}, config = {}",
                            "run".bold().green(),
                            i,
                            problem_path,
                            config_path,
                        ));
                        tokio::select! {
                            _ = tokio::time::sleep(Duration::from_secs((timeout+10) as u64)) => {
                                let _ = c.kill().await;
                                bar.println("command killed!")
                            }
                            _ = c.wait() => {
                                //bar.println!("command ok!")
                            }
                        }
                        let _r = c.wait();
                    } else {
                        panic!("panic");
                    }
                    let run = RunData {
                        duration: start.elapsed().unwrap(),
                    };
                    benchmark_data.runs.push(run);
                    bar.inc(1);
                    i += 1;
                }
            }
        }
    }

    bar.finish();

    /*send_email(
        &config.mail,
        benchmark_data.format_data("ompas-bench".to_string()),
    )*/
}

fn generate_config(heuristic: &HeuristicConfig, domain: &Path) -> (String, String, String) {
    let select = match heuristic.select {
        SelectConfig::Greedy => "greedy",
        SelectConfig::Random => "random",
        SelectConfig::UPOM => "upom",
    };

    let mut config_name = select.to_string();

    let start = match heuristic.continuous_planning {
        ContinuousPlanningConfig::No => {
            config_name.push_str("_reactive");
            "(start)"
        }
        ContinuousPlanningConfig::Satisfactory => {
            config_name.push_str("_satisfactory");

            "(start-with-planner false)"
        }
        ContinuousPlanningConfig::Optimality => {
            config_name.push_str("_optimality");
            "(start-with-planner true)"
        }
    };

    let domain = match &heuristic.specific_domain {
        None => domain.canonicalize().unwrap().to_str().unwrap().to_string(),
        Some(s) => {
            config_name.push_str(s.label.as_str());
            s.domain
                .canonicalize()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string()
        }
    };

    (
        config_name,
        format!(
"(begin
    (read \"{}\") ; loading domain
    (set-log-level debug) ;setting log-level
    (set-select {})) ; define the algorithm of select",
            domain, select,
        ),
        start.to_string()
    )
}

fn get_all_problems_files(path: &Path) -> Vec<Problem> {
    let mut problem_files = vec![];

    let mut queue = vec![path.to_path_buf()];

    while let Some(path) = queue.pop() {
        let path = path.canonicalize().unwrap();
        let dir = fs::read_dir(path).unwrap();

        for entry in dir {
            let entry = entry.unwrap();

            let path = entry.path();
            if path.is_dir() {
                queue.push(path);
            } else if path.is_file() {
                let file_name = path.file_name().unwrap().to_str().unwrap();
                if file_name.contains(".lisp") {
                    problem_files.push(Problem {
                        label: file_name.to_string(),
                        path,
                    });
                }
            }
        }
    }
    problem_files
}
