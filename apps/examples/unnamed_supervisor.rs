use std::path::PathBuf;

use structopt::StructOpt;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};

//use ompas_modules::robot::CtxRobot;
use ompas_acting::controller::dumber::CtxDumber;
use ompas_acting::rae::module::init_ctx_rae;
use ompas_acting::rae::module::mod_rae::CtxRae;
use ompas_acting::rae::module::mod_rae_monitor::CtxRaeMonitor;
use ompas_godot_simulation_client::mod_godot::CtxGodot;
use ompas_godot_simulation_client::rae_interface::PlatformGodot;
use ompas_lisp::core::ImportType::WithoutPrefix;
use ompas_lisp::core::*;
use ompas_lisp::modules::_type::CtxType;
use ompas_lisp::modules::deprecated::counter::CtxCounter;
use ompas_lisp::modules::doc::{CtxDoc, Documentation};
use ompas_lisp::modules::io::CtxIo;
use ompas_lisp::modules::math::CtxMath;
use ompas_lisp::modules::utils::CtxUtils;
use ompas_lisp::repl::{spawn_log, spawn_repl};
use ompas_utils::task_handler;

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
    let (sender_li, mut receiver_li): (Sender<String>, Receiver<String>) =
        mpsc::channel(TOKIO_CHANNEL_SIZE);

    //Spawn the stdin and stdout threads
    /*let sender_stdin = spawn_stdin(sender_li.clone())
        .await
        .expect("error while spawning stdin");
    let sender_stdout = spawn_stdout().await.expect("error while spawning stdout");*/

    let sender_repl = spawn_repl(sender_li.clone())
        .await
        .expect("error while spawning repl");

    let (sender_log, log_handle) = spawn_log(log.clone())
        .await
        .expect("error while spawning log task");

    let (mut root_env, mut ctxs) = LEnv::root().await;
    let mut ctx_doc = CtxDoc::default();
    let mut ctx_io = CtxIo::default();
    let ctx_math = CtxMath::default();
    let ctx_type = CtxType::default();
    let ctx_counter = CtxCounter::default();
    let _ctx_godot = CtxGodot::default();
    let ctx_utils = CtxUtils::default();
    let (ctx_rae, ctx_rae_monitor) =
        init_ctx_rae(Some(Box::new(PlatformGodot::default())), log.clone()).await;
    //Insert the doc for the different contexts.
    ctx_doc.insert_doc(CtxIo::documentation());
    ctx_doc.insert_doc(CtxMath::documentation());
    ctx_doc.insert_doc(CtxType::documentation());
    ctx_doc.insert_doc(CtxDumber::documentation());
    ctx_doc.insert_doc(CtxRae::documentation());
    ctx_doc.insert_doc(CtxRaeMonitor::documentation());
    ctx_doc.insert_doc(CtxUtils::documentation());

    //Add the sender of the channel.
    //ctx_io.add_sender_li(sender_li.clone());
    if let Some(pb) = log {
        ctx_io.set_log_output(pb.into());
    }

    /*let mut _ctx_rae_exec = CtxRaeExec {
        actions_progress: Default::default(),
        state: Default::default(),
        platform_interface: Box::new(PlatformGodot::default()),
        agenda: Default::default(),
    };*/

    import(&mut root_env, &mut ctxs, ctx_utils, WithoutPrefix)
        .await
        .expect("error loading utils");
    import(&mut root_env, &mut ctxs, ctx_doc, WithoutPrefix)
        .await
        .expect("error loading doc");
    import(&mut root_env, &mut ctxs, ctx_io, WithoutPrefix)
        .await
        .expect("error loading io");
    import(&mut root_env, &mut ctxs, ctx_math, WithoutPrefix)
        .await
        .expect("error loading math");
    import(&mut root_env, &mut ctxs, ctx_type, WithoutPrefix)
        .await
        .expect("error loading type");
    import(&mut root_env, &mut ctxs, ctx_counter, WithoutPrefix)
        .await
        .expect("error loading counter");
    import(&mut root_env, &mut ctxs, ctx_rae, WithoutPrefix)
        .await
        .expect("error loading rae");
    import(&mut root_env, &mut ctxs, ctx_rae_monitor, WithoutPrefix)
        .await
        .expect("error loading monitor");

    let env = &mut root_env.clone();

    loop {
        //TODO: handle response to multi user.
        let mut sender: Sender<String> = sender_log.clone();

        //let mut send_ack = false;
        let mut str_lvalue = receiver_li.recv().await.expect("bug in lisp interpretor");
        //println!("expr: {}", str_lvalue);

        if str_lvalue.contains("repl:") {
            // stdout.write_all(b"from repl\n");
            sender = sender_repl.clone();
            str_lvalue = str_lvalue.replace("repl:", "");
        }

        if str_lvalue == *"exit" {
            task_handler::end_all();
            /*sender
            .send(EXIT_CODE_STDOUT.to_string())
            .await
            .expect("error sending message to stdout");*/
            break;
        }

        //stdout.write_all(format!("receiving command: {}\n", str_lvalue).as_bytes());

        match parse(str_lvalue.as_str(), env, &mut ctxs).await {
            Ok(lv) => match eval(&lv, env, &mut ctxs).await {
                Ok(lv) => {
                    sender
                        .send(lv.pretty_print(0))
                        .await
                        .expect("error on channel to stdout");
                }
                Err(e) => {
                    //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                    sender
                        .send(format!("error: {}", e))
                        .await
                        .expect("error on channel to stdout");
                }
            },
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                sender
                    .send(format!("error: {}", e))
                    .await
                    .expect("error on channel to stdout");
            }
        };
    }

    log_handle.await.expect("Failed to kill log task");
}
