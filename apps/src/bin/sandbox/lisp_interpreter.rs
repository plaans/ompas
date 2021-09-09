use std::path::PathBuf;

use structopt::StructOpt;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};

//use ompas_modules::robot::CtxRobot;
use ompas_acting::controller::dumber::CtxDumber;
use ompas_acting::rae::module::mod_rae_exec::CtxRaeExec;
use ompas_godot_simulation_client::mod_godot::CtxGodot;
use ompas_godot_simulation_client::rae_interface::PlatformGodot;
use ompas_lisp::core::*;
use ompas_lisp::modules::_type::CtxType;
use ompas_lisp::modules::counter::CtxCounter;
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

    #[structopt(short = "d", long = "debug")]
    debug: bool,

    #[structopt(short = "t", long = "tests")]
    test: bool,
}

#[tokio::main]
async fn main() {
    println!("Scheme console v0.1");

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

    let (mut root_env, mut ctxs, mut lisp_init) = LEnv::root();
    let mut ctx_doc = CtxDoc::default();
    let mut ctx_io = CtxIo::default();
    let ctx_math = CtxMath::default();
    //let ctx_robot = CtxRobot::default();
    let ctx_type = CtxType::default();
    let ctx_counter = CtxCounter::default();
    //let ctx_dumber = CtxDumber::default();
    //let _ctx_godot = CtxGodot::default();
    let ctx_utils = CtxUtils::default();
    //let (ctx_rae, ctx_rae_monitor) = init_ctx_rae(Box::new(PlatformGodot::default()));
    //Insert the doc for the different contexts.
    ctx_doc.insert_doc(CtxIo::documentation());
    ctx_doc.insert_doc(CtxMath::documentation());
    //ctx_doc.insert_doc(CtxRobot::documentation());
    ctx_doc.insert_doc(CtxType::documentation());
    ctx_doc.insert_doc(CtxGodot::documentation());
    ctx_doc.insert_doc(CtxDumber::documentation());
    //ctx_doc.insert_doc(CtxRae::documentation());
    //ctx_doc.insert_doc(CtxRaeMonitor::documentation());

    //Add the sender of the channel.
    ctx_io.add_sender_li(sender_li.clone());
    if let Some(pb) = log {
        ctx_io.set_log_output(pb.into());
    }

    let mut _ctx_rae_exec = CtxRaeExec {
        actions_progress: Default::default(),
        state: Default::default(),
        platform_interface: Box::new(PlatformGodot::default()),
        agenda: Default::default(),
    };

    load_module(&mut root_env, &mut ctxs, ctx_doc, &mut lisp_init);
    load_module(&mut root_env, &mut ctxs, ctx_io, &mut lisp_init);
    load_module(&mut root_env, &mut ctxs, ctx_math, &mut lisp_init);
    //load_module(root_env, ctxs, ctx_robot,lisp_init);
    //load_module(&mut root_env, &mut ctxs, ctx_dumber, &mut lisp_init);
    load_module(&mut root_env, &mut ctxs, ctx_type, &mut lisp_init);
    load_module(&mut root_env, &mut ctxs, ctx_counter, &mut lisp_init);
    //load_module(&mut root_env, &mut ctxs, ctx_godot, &mut lisp_init);
    //load_module(&mut root_env, &mut ctxs, ctx_rae, &mut lisp_init);
    //load_module(&mut root_env, &mut ctxs, ctx_rae_monitor, &mut lisp_init);
    load_module(&mut root_env, &mut ctxs, ctx_utils, &mut lisp_init);
    //load_module(&mut root_env, &mut ctxs, ctx_rae_exec, &mut lisp_init);
    let env = &mut root_env.clone();
    //println!("{}", lisp_init.begin_lisp());

    for def in lisp_init.inner() {
        sender_li
            .send(def.to_string())
            .await
            .expect("error while sending message");
    }

    //println!("global ctxs: {}", ctxs);

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
                        .send(format!("{}", lv))
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
        //stdout.write_all(b"parsing done\n");
    }

    log_handle.await.expect("Failed to kill log task");
}
