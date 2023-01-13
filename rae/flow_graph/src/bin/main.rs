use chrono::{DateTime, Utc};
use flow_graph::config::GraphConvertConfig;
use flow_graph::conversion::convert;
use flow_graph::structs::chronicle::template::ChronicleTemplate;
use sompas_core::{get_root_env, parse};
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
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

        let lv = parse(&problem, &mut env)
            .await
            .unwrap_or_else(|r| panic!("{}", r.to_string()));

        let ch: ChronicleTemplate = convert(&lv, &env).await?;

        //println!("symbol types: {}", ch.sym_table.format_types());
        //println!("types forest: {}", ch.sym_table.format_types_forest());
        /*let lv = pre_processing(&lv, &env).await?;

        let mut graph = FlowGraph::default();

        let r = convert_into_flow_graph(&lv, &mut graph, None, &mut Default::default())?;*/

        output_markdown(
            p.to_str().unwrap(),
            &lv,
            &ch,
            config.output_path.clone().unwrap(),
            true,
        );

        /*flow_graph_post_processing(&mut ch.debug.flow_graph)?;

        output_markdown(
            format!("{}_post", p.to_str().unwrap()).as_str(),
            &lv,
            &ch,
            config.output_path.clone().unwrap(),
            true,
        );*/
    }
    Ok(())
}

fn output_markdown(
    name: &str,
    expression: &LValue,
    ch: &ChronicleTemplate,
    path: PathBuf,
    view: bool,
) {
    let mut path = path;
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    path.push(format!("graph-flow-output_{}", string_date));
    fs::create_dir_all(&path).unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = format!("{}.dot", name);
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ch.debug.flow_graph.export_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let flow_file_name = format!("{}.png", name);
    Command::new("dot")
        .args(["-Tpng", &dot_file_name, "-o", &flow_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut path_dot = path.clone();
    let dot_file_name = "lattice.dot";
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = ch.st.export_lattice_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let lattice_file_name = "lattice.png";
    Command::new("dot")
        .args(["-Tpng", &dot_file_name, "-o", &lattice_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut md_path = path.clone();
    let md_file_name = format!("{}-output.md", name);
    md_path.push(&md_file_name);
    let mut md_file = File::create(&md_path).unwrap();
    let md: String = format!(
        "# Conversion of expression : {}\n
    \n
## Scheme code

```lisp\n
{}
```
\n
## Post processed Scheme code
```lisp\n
{}
```
## Graph
\n
![]({})
\n
## Chronicles
```
{}
```

## Type Lattice
\n
![]({})
\n

## Sym Table
```
{}
```
    ",
        name,
        expression.format(0),
        ch.debug.post_processed_lvalue.format(0),
        flow_file_name,
        ch,
        lattice_file_name,
        ch.st
    );

    md_file.write_all(md.as_bytes()).unwrap();

    if view {
        Command::new("google-chrome")
            .arg(&md_file_name)
            .spawn()
            .unwrap();
    }
}
