use chrono::{DateTime, Utc};
use flow_graph::config::GraphConvertConfig;
use sompas_structs::lvalue::LValue;
use std::env::set_current_dir;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;
use structopt::StructOpt;
use yaml_rust::YamlLoader;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "Graph-convert",
    about = "Conversion of Scheme expression into Graph Flow"
)]
struct Opt {
    #[structopt(short = "v", long = "view")]
    view: bool,
    #[structopt(short = "c", long = "config")]
    config: Option<PathBuf>,
}

#[tokio::main]
async fn main() {
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
                fs::read_to_string(&config).expect("Something went wrong reading the config file");

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

    output_markdown(&LValue::Nil, &(), config.output_path.clone().unwrap(), true);
}

fn output_markdown(expression: &LValue, graph: &(), path: PathBuf, view: bool) {
    let mut path = path;
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    path.push(format!("graph-flow-output_{}", string_date));
    fs::create_dir(&path).unwrap();
    let mut path_dot = path.clone();
    path_dot.push("temp.dot");
    let mut file = File::create(&path_dot).unwrap();
    file.write_all(b"digraph { a -> b }").unwrap();
    set_current_dir(&path).unwrap();
    Command::new("dot")
        .args(&["-Tpng", "temp.dot", "-o", "graph.png"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    let mut md_path = path.clone();
    md_path.push("test.md");
    let mut md_file = File::create(&md_path).unwrap();
    let md: String = format!(
        "# Conversion of expression : {}\n
    \n
## Scheme code

```lisp\n
{}
```
\n
## Graph
\n
![](graph.png)
    ",
        "expression.lisp",
        expression.format(0)
    );

    md_file.write_all(md.as_bytes()).unwrap();

    if view {
        Command::new("google-chrome")
            .arg("test.md")
            .spawn()
            .unwrap()
            .wait()
            .unwrap();
    }
}
