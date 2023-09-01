use ompas_apps::config::GraphConvertConfig;
use ompas_core::model::acting_domain::model::ActingModel;
use ompas_core::model::sym_domain::basic_type::TYPE_ID_SYMBOL;
use ompas_core::model::sym_domain::type_lattice::TypeLattice;
use ompas_core::model::sym_table::r#ref::RefSymTable;
use ompas_core::model::sym_table::SymTable;
use ompas_core::planning::conversion::flow_graph::algo::annotate::annotate;
use ompas_core::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use ompas_core::planning::conversion::flow_graph::algo::pre_processing::{
    lambda_expansion, pre_processing,
};
use ompas_core::planning::conversion::{_convert, debug_with_markdown, TEST_CONVERSION};
use sompas_core::{get_root_env, parse};
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fs;
use std::path::PathBuf;
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

    TEST_CONVERSION.set(true);

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

        let ch: ActingModel = __convert(
            lv,
            PLEnv {
                env,
                unpure_bindings: Default::default(),
                pc: Default::default(),
            },
        )
        .await?;

        debug_with_markdown(
            p.to_str().unwrap(),
            &ch,
            config.output_path.clone().unwrap(),
            true,
        );
    }
    Ok(())
}

pub async fn __convert(lv: LValue, mut p_env: PLEnv) -> Result<ActingModel, LRuntimeError> {
    let mut lattice = TypeLattice::new();
    lattice.add_type("robot", vec![TYPE_ID_SYMBOL]);
    let st: RefSymTable = SymTable::new_from(lattice).into();

    //convert(None, &lv, &mut p_env, st).await

    let lv_om = annotate(lv.clone());
    //debug_println!("annotate =>\n{}", lv_om.format(0));

    let pp_lv = lambda_expansion(&lv_om, &p_env).await?;
    let pp_lv = pre_processing(&pp_lv, &p_env).await?;
    //debug_println!("pre_processing =>\n{}", pp_lv.format(0));

    let chronicle = match _convert(None, &pp_lv, &mut p_env, st).await {
        Ok(ch) => Some(ch),
        Err(e) => {
            println!("{}", e);
            None
        }
    };

    Ok(ActingModel {
        lv,
        lv_om,
        lv_expanded: Some(pp_lv),
        runtime_info: Default::default(),
        chronicle,
    })
}
