use std::path::PathBuf;

use structopt::StructOpt;

use ompas_acting::controller::dumber::CtxDumber;
use ompas_acting::rae::module::init_ctx_rae;
use ompas_acting::rae::module::mod_rae::CtxRae;
use ompas_acting::rae::module::mod_rae_monitor::CtxRaeMonitor;
use ompas_godot_simulation_client::mod_godot::CtxGodot;
use ompas_godot_simulation_client::rae_interface::PlatformGodot;
use ompas_lisp::core::*;
use ompas_lisp::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use ompas_lisp::modules::_type::CtxType;
use ompas_lisp::modules::counter::CtxCounter;
use ompas_lisp::modules::doc::{CtxDoc, Documentation};
use ompas_lisp::modules::error::CtxError;
use ompas_lisp::modules::io::CtxIo;
use ompas_lisp::modules::math::CtxMath;
use ompas_lisp::modules::utils::CtxUtils;

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "FactBase",
    about = "A fact and belief database inside an acting and planning engine"
)]
struct Opt {
    #[structopt(short, long)]
    log: Option<PathBuf>,

    #[structopt(short, long)]
    repl: bool,

    #[structopt(short = "f", long = "file")]
    input: Option<PathBuf>,

    #[structopt(short = "t", long = "tests")]
    test: bool,

    #[structopt(short = "d", long = "debug")]
    debug: bool,
}

#[tokio::main]
async fn main() {
    println!("uname fact base v1.0");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    if opt.debug {
        activate_debug();
    }
    //test_lib_model(&opt);
    lisp_interpreter(opt.log).await;
}

pub async fn lisp_interpreter(log: Option<PathBuf>) {
    let mut li = LispInterpreter::new().await;

    let mut ctx_doc = CtxDoc::default();
    let mut ctx_io = CtxIo::default();
    let ctx_math = CtxMath::default();
    let ctx_type = CtxType::default();
    let ctx_counter = CtxCounter::default();
    let _ctx_godot = CtxGodot::default();
    let ctx_utils = CtxUtils::default();
    let (ctx_rae, ctx_rae_monitor) =
        init_ctx_rae(Box::new(PlatformGodot::default()), log.clone()).await;
    //Insert the doc for the different contexts.
    ctx_doc.insert_doc(CtxIo::documentation());
    ctx_doc.insert_doc(CtxMath::documentation());
    ctx_doc.insert_doc(CtxType::documentation());
    ctx_doc.insert_doc(CtxDumber::documentation());
    ctx_doc.insert_doc(CtxRae::documentation());
    ctx_doc.insert_doc(CtxRaeMonitor::documentation());
    ctx_doc.insert_doc(CtxUtils::documentation());

    //Add the sender of the channel.
    ctx_io.add_communication(li.subscribe());
    if let Some(pb) = &log {
        ctx_io.set_log_output(pb.clone().into());
    }

    li.import_namespace(CtxError::default())
        .await
        .expect("error loading error");
    li.import_namespace(ctx_utils)
        .await
        .expect("error loading utils");
    li.import_namespace(ctx_doc)
        .await
        .expect("error loading doc");
    li.import_namespace(ctx_io).await.expect("error loading io");
    li.import_namespace(ctx_math)
        .await
        .expect("error loading math");
    li.import_namespace(ctx_type)
        .await
        .expect("error loading type");
    li.import_namespace(ctx_counter)
        .await
        .expect("error loading counter");
    li.import_namespace(ctx_rae)
        .await
        .expect("error loading rae");
    li.import_namespace(ctx_rae_monitor)
        .await
        .expect("error loading rae monitor");

    li.set_config(LispInterpreterConfig::new(true));

    li.run(log).await;
}
