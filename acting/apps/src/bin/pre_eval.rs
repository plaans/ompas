use ompas_core::planning::conversion::flow_graph::algo::p_eval::p_eval;
use ompas_core::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use sompas_core::{eval_init, get_root_env, parse};
use sompas_modules::utils::ModUtils;
use sompas_structs::lenv::{ImportType, LEnv};

#[allow(unused)]
const EXPR: &str = "(begin \
    (define square (lambda (x) (* x x)))\
    (square a))";

const EXPR_2: &str = "(unzip a)";
#[tokio::main]
async fn main() {
    let mut env: LEnv = get_root_env().await;
    env.import_module(ModUtils::default(), ImportType::WithoutPrefix);
    eval_init(&mut env).await;

    let mut p_env = PLEnv {
        env,
        unpure_bindings: Default::default(),
        pc: Default::default(),
    };

    p_env.add_unpure("a".to_string());

    let lv = parse(EXPR_2, &mut p_env.env).await.unwrap();
    let lv = p_eval(&lv, &mut p_env).await.unwrap();
    println!("{}", lv.format(0))
}
