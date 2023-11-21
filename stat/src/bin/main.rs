use ompas_stat::config::StatosConfig;
use ompas_stat::OMPASStatCollection;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "Generator",
    about = "Generation of problems for gripper domain"
)]
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

    let _ = OMPASStatCollection::new(&config.dir);
}
