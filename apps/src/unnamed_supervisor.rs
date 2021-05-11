use ompas_lisp::core::*;
use ompas_lisp::structs::LValue;
use ompas_modules::_type::CtxType;
use ompas_modules::counter::CtxCounter;
use ompas_modules::doc::{CtxDoc, Documentation};
use ompas_modules::io::repl::{spawn_stdin, spawn_stdout, EXIT_CODE_STDOUT};
use ompas_modules::io::CtxIo;
use ompas_modules::math::CtxMath;
use ompas_modules::robot::CtxRobot;
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver, Sender};
use structopt::StructOpt;

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

fn main() {
    println!("uname fact base v1.0");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);
    //test_lib_model(&opt);
    lisp_interpreter();
}

pub fn lisp_interpreter() {
    let (sender_li, receiver_li): (Sender<String>, Receiver<String>) = channel();
    //Channel from Lisp Interpretor to repl
    let sender_stdin = spawn_stdin(sender_li.clone()).expect("error while spawning stdin");
    let sender_stdout = spawn_stdout().expect("error while spawning stdout");

    let root_env = &mut RefLEnv::root();
    let ctxs: &mut ContextCollection = &mut Default::default();
    let mut ctx_doc = CtxDoc::default();
    let mut ctx_io = CtxIo::default();
    let ctx_math = CtxMath::default();
    let ctx_robot = CtxRobot::default();
    let ctx_type = CtxType::default();
    let ctx_counter = CtxCounter::default();

    ctx_doc.insert_doc(CtxIo::documentation());
    ctx_doc.insert_doc(CtxMath::documentation());
    ctx_doc.insert_doc(CtxRobot::documentation());
    ctx_doc.insert_doc(CtxType::documentation());

    //Add the sender of the channel.
    ctx_io.add_sender_li(sender_li);
    ctx_io.add_sender_stdout(sender_stdout.clone());

    load_module(root_env, ctxs, ctx_doc);
    load_module(root_env, ctxs, ctx_io);
    load_module(root_env, ctxs, ctx_math);
    load_module(root_env, ctxs, ctx_robot);
    load_module(root_env, ctxs, ctx_type);
    load_module(root_env, ctxs, ctx_counter);
    let env = &mut RefLEnv::new_from_outer(root_env.clone());

    //Add core macros
    /*sender_li
    .send(core_macros())
    .expect("error while sending message");*/

    loop {
        let mut send_ack = false;
        let mut str_lvalue = receiver_li.recv().expect("bug in lisp interpretor");

        if str_lvalue.contains("repl:") {
            // stdout.write_all(b"from repl\n");
            send_ack = true;
            str_lvalue = str_lvalue.replace("repl:", "");
        }

        if str_lvalue == *"exit" {
            sender_stdout
                .send(EXIT_CODE_STDOUT.to_string())
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
                    .expect("error on channel to stdout");
                LValue::None
            }
        };
        //stdout.write_all(b"parsing done\n");
        match eval(&lvalue, env, ctxs) {
            Ok(lv) => match lv {
                LValue::None => {}
                lv => {
                    //stdout.write_all(format!("LI>> {}\n", lv).as_bytes()).expect("error stdout");
                    sender_stdout
                        .send(format!("LI>> {}", lv))
                        .expect("error on channel to stdout");
                }
            },
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                sender_stdout
                    .send(format!("ELI>>{}", e))
                    .expect("error on channel to stdout");
            }
        };
        if send_ack {
            sender_stdin
                .send("ACK".to_string())
                .expect("error sending ack to repl");
        }
        //stdout.write_all(b"eval done\n");
    }
}
