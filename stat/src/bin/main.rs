use ompas_stat::config::StatosConfig;
use ompas_stat::stat::system::{SystemRunData, SystemStatFormatter};
use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::time::SystemTime;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(name = "Statos", about = "Generation of problems for gripper domain")]
struct Opt {
    #[structopt(short = "c", long = "config")]
    config: PathBuf,
}

pub fn main() {
    println!("Hello, world!");

    let opt = Opt::from_args();

    let str = fs::read_to_string(opt.config).expect("Could not read config file");

    let config: StatosConfig =
        serde_yaml::from_str(&str).expect("Could not deserialize content of config");

    println!("config: {:?}", config);

    for config in config.configs {
        let system_run = SystemRunData::new(&config.input_dir);

        let time = SystemTime::now();
        let stat = system_run.compute_stat();
        let formatter = SystemStatFormatter::from(&stat);

        println!(
            "time to compute stat : {} s",
            time.elapsed().unwrap().as_secs_f32()
        );

        println!("{}", formatter);

        let csv = formatter.to_csv();

        let mut file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(&config.output_file)
            .unwrap();
        file.write_all(csv.as_bytes()).unwrap();
    }
}
