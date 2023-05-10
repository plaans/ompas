use ompas_apps::config::GraphConvertConfig;
use ompas_core::planning::conversion::flow_graph::algo::annotate::annotate;
use sompas_core::{get_root_env, parse};
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fs;
use std::path::PathBuf;
use std::time::Instant;
use structopt::StructOpt;
use yaml_rust::YamlLoader;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "Graph-convert",
    about = "Conversion of Scheme expression into Graph Flow"
)]
struct Opt {
    #[structopt(short = "c", long = "config")]
    config: Option<PathBuf>,
}

#[tokio::main]
async fn main() -> Result<(), LRuntimeError> {
    println!(
        "Hello, world!\n
Graph flow converter for SOMPAS code!\n
    "
    );

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);

    let config: GraphConvertConfig = match &opt.config {
        Some(config) => {
            let config =
                fs::read_to_string(config).expect("Something went wrong reading the config file");

            let configs = YamlLoader::load_from_str(&config).unwrap();
            let config = &configs[0];
            config.try_into().expect("could not read config")
        }
        None => Default::default(),
    };

    /*let mut path = config.output_path.clone().unwrap();
    path.push("test.dot");
    let mut file = File::create(&path).unwrap();
    file.write_all(b"digraph { a -> b }").unwrap();*/

    for p in &config.problems {
        println!("problem: {:?}", p);
        let mut env: LEnv = get_root_env().await;
        let mut path = config.input_path.clone().unwrap();
        path.push(p);

        let problem =
            fs::read_to_string(path).expect("Something went wrong reading the config file");

        let lv: LValue = parse(&problem, &mut env)
            .await
            .unwrap_or_else(|r| panic!("{}", r.to_string()));
        let instant = Instant::now();
        let lv = annotate(lv);
        let elapsed = instant.elapsed().as_micros();
        println!("time: {}µs\n{}", elapsed, lv.format(0));
    }
    Ok(())
}
