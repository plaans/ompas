use ompas_godot_simulation_client::godot::CtxGodot;
use ompas_godot_simulation_client::state::CtxState;
use ompas_lisp::core::*;
use ompas_lisp::structs::{InitLisp, LValue};
use ompas_modules::_type::CtxType;
use ompas_modules::counter::CtxCounter;
use ompas_modules::doc::{CtxDoc, Documentation};
use ompas_modules::io::repl::{spawn_stdin, spawn_stdout, EXIT_CODE_STDOUT};
use ompas_modules::io::{CtxIo, TOKIO_CHANNEL_SIZE};
use ompas_modules::math::CtxMath;
//use ompas_modules::robot::CtxRobot;
use std::path::PathBuf;
use structopt::StructOpt;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};

pub const CHANNEL_SIZE: usize = 16_384;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "FactBase",
    about = "A fact and belief database inside an acting and planning engine"
)]
struct Opt {
    #[structopt(short, long)]
    repl: bool,

    #[structopt(short = "f", long = "file")]
    input: Option<PathBuf>,

    #[structopt(short = "t", long = "test")]
    test: bool,
}

#[tokio::main]
async fn main() {
    println!("uname fact base v1.0");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    //test_lib_model(&opt);
    lisp_interpreter().await;
}

pub async fn lisp_interpreter() {
    let (sender_li, mut receiver_li): (Sender<String>, Receiver<String>) =
        mpsc::channel(TOKIO_CHANNEL_SIZE);

    //Spawn the stdin and stdout threads
    let sender_stdin = spawn_stdin(sender_li.clone())
        .await
        .expect("error while spawning stdin");
    let sender_stdout = spawn_stdout().await.expect("error while spawning stdout");

    let root_env = &mut LEnv::root();
    let ctxs: &mut ContextCollection = &mut Default::default();
    let mut ctx_doc = CtxDoc::default();
    let mut ctx_io = CtxIo::default();
    let ctx_math = CtxMath::default();
    //let ctx_robot = CtxRobot::default();
    let ctx_type = CtxType::default();
    let ctx_counter = CtxCounter::default();
    let mut ctx_godot = CtxGodot::default();
    let mut ctx_state = CtxState::default();

    //Insert the doc for the different contexts.
    ctx_doc.insert_doc(CtxIo::documentation());
    ctx_doc.insert_doc(CtxMath::documentation());
    //ctx_doc.insert_doc(CtxRobot::documentation());
    ctx_doc.insert_doc(CtxType::documentation());
    ctx_doc.insert_doc(CtxGodot::documentation());
    ctx_doc.insert_doc(CtxState::documentation());

    //Add the sender of the channel.
    ctx_io.add_sender_li(sender_li.clone());
    ctx_io.add_sender_stdout(sender_stdout.clone());

    ctx_state.set_sender_stdout(sender_stdout.clone());
    ctx_godot.set_sender_li(sender_li.clone());

    let lisp_init = &mut InitLisp::core();

    load_module(root_env, ctxs, ctx_doc, lisp_init);
    load_module(root_env, ctxs, ctx_io, lisp_init);
    load_module(root_env, ctxs, ctx_math, lisp_init);
    //load_module(root_env, ctxs, ctx_robot,lisp_init);
    load_module(root_env, ctxs, ctx_type, lisp_init);
    load_module(root_env, ctxs, ctx_counter, lisp_init);
    load_module(root_env, ctxs, ctx_godot, lisp_init);
    load_module(root_env, ctxs, ctx_state, lisp_init);
    let env = &mut root_env.clone();
    //println!("{}", lisp_init.begin_lisp());

    //Add core macros
    /*sender_li
    .send(lisp_init.begin_lisp())
    .await
    .expect("error while sending message");*/

    loop {
        let mut send_ack = false;
        let mut str_lvalue = receiver_li.recv().await.expect("bug in lisp interpretor");

        if str_lvalue.contains("repl:") {
            // stdout.write_all(b"from repl\n");
            send_ack = true;
            str_lvalue = str_lvalue.replace("repl:", "");
        }

        if str_lvalue == *"exit" {
            sender_stdout
                .send(EXIT_CODE_STDOUT.to_string())
                .await
                .expect("error sending message to stdout");
            break;
        }

        //stdout.write_all(format!("receiving command: {}\n", str_lvalue).as_bytes());

        let lvalue = match parse(str_lvalue.as_str(), env, ctxs) {
            Ok(lv) => lv,
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                sender_stdout
                    .send(format!("ELI>>{}", e))
                    .await
                    .expect("error on channel to stdout");
                LValue::Nil
            }
        };
        //stdout.write_all(b"parsing done\n");
        match eval(&lvalue, env, ctxs) {
            Ok(lv) => match lv {
                LValue::Nil => {}
                lv => {
                    //stdout.write_all(format!("LI>> {}\n", lv).as_bytes()).expect("error stdout");
                    sender_stdout
                        .send(format!("LI>> {}", lv))
                        .await
                        .expect("error on channel to stdout");
                }
            },
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                sender_stdout
                    .send(format!("ELI>>{}", e))
                    .await
                    .expect("error on channel to stdout");
            }
        };
        if send_ack {
            sender_stdin
                .send("ACK".to_string())
                .await
                .expect("error sending ack to repl");
        }
        //stdout.write_all(b"eval done\n");
    }
}
