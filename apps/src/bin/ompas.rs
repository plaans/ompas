use std::path::PathBuf;

use ompas_godot_simulation_client::mod_godot::CtxGodot;
use ompas_godot_simulation_client::rae_interface::PlatformGodot;
use ompas_lisp::core::activate_debug;
use ompas_lisp::lisp_interpreter::{LispInterpreter, LispInterpreterConfig};
use ompas_lisp::modules::_type::CtxType;
use ompas_lisp::modules::advanced_math::CtxMath;
use ompas_lisp::modules::io::CtxIo;
use ompas_lisp::modules::string::CtxString;
use ompas_lisp::modules::utils::CtxUtils;
use ompas_rae::module::rae_exec::Platform;
use ompas_rae::module::CtxRae;
use structopt::StructOpt;

pub const TOKIO_CHANNEL_SIZE: usize = 65_384;

#[derive(Debug, StructOpt)]
#[structopt(name = "OMPAS", about = "An acting engine based on RAE.")]
struct Opt {
    #[structopt(short = "d", long = "debug")]
    debug: bool,
    #[structopt(short = "p", long = "log-path")]
    log: Option<PathBuf>,

    #[structopt(short = "g", long = "gripper")]
    gripper: bool,

    #[structopt(short = "G", long = "godot")]
    godot: bool,
}

#[tokio::main]
async fn main() {
    println!("OMPAS v0.1");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    if opt.debug {
        activate_debug();
    }
    //test_lib_model(&opt);
    lisp_interpreter(opt.log, opt.gripper, opt.godot).await;
}

pub async fn lisp_interpreter(log: Option<PathBuf>, gripper: bool, godot: bool) {
    let mut li = LispInterpreter::new().await;

    let mut ctx_io = CtxIo::default();
    let ctx_math = CtxMath::default();
    let ctx_type = CtxType::default();
    let ctx_utils = CtxUtils::default();
    let ctx_string = CtxString::default();

    //Insert the doc for the different contexts.

    //Add the sender of the channel.
    if let Some(pb) = &log {
        ctx_io.set_log_output(pb.clone().into());
    }

    li.import_namespace(ctx_utils)
        .await
        .expect("error loading utils");
    li.import_namespace(ctx_io).await.expect("error loading io");
    li.import_namespace(ctx_math)
        .await
        .expect("error loading math");
    li.import_namespace(ctx_type)
        .await
        .expect("error loading type");

    li.import(ctx_string)
        .await
        .expect("error loading ctx string");

    if godot {
        li.import_namespace(CtxGodot::default())
            .await
            .expect("error loading godot")
    } else {
        let ctx_rae = CtxRae::init_ctx_rae(
            match gripper {
                true => None,
                false => Some(Platform::new(PlatformGodot::default())),
            },
            log.clone(),
        )
        .await;
        li.import_namespace(ctx_rae)
            .await
            .expect("error loading rae");
    }

    li.set_config(LispInterpreterConfig::new(true));
    let com = li.subscribe();
    if gripper {
        com.send("(read instances/gripper/init.lisp)".to_string())
            .await
            .expect("could not send to LI");
    }

    li.run(log).await;
}
