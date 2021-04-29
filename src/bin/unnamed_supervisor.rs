use fact_base::core::structs::{AsModule, LValue};
use fact_base::core::{eval, load_module, parse, ContextCollection, RefLEnv};
use fact_base::modules::_type::CtxType;
use fact_base::modules::counter::CtxCounter;
use fact_base::modules::io::{repl, CtxIO};
use fact_base::modules::math::CtxMath;
use fact_base::modules::robot::CtxRobot;
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
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
    let (sender_repl, receiver_repl): (Sender<String>, Receiver<String>) = channel();

    let root_env = &mut RefLEnv::root();
    let ctxs: &mut ContextCollection = &mut Default::default();
    load_module(root_env, ctxs, CtxCounter::get_module());
    let id_io = load_module(root_env, ctxs, CtxIO::get_module());
    //Add the sender of the channel.
    let ctx_io = ctxs
        .get_mut_context(id_io)
        .downcast_mut::<CtxIO>()
        .expect("couldn't downcast ref");
    ctx_io.add_sender(sender_li.clone());

    load_module(root_env, ctxs, CtxType::get_module());
    load_module(root_env, ctxs, CtxMath::get_module());
    load_module(root_env, ctxs, CtxRobot::get_module());
    let env = &mut RefLEnv::new_from_outer(root_env.clone());

    //let mut stdout = io::stdout();
    //let mut stderr = io::stderr();

    //Channel from X to Lisp Interpretor

    //Launch the repl thread
    let repl_join_handle = thread::Builder::new()
        .name("repl".to_string())
        .spawn(move || {
            repl::run(sender_li, receiver_repl);
        })
        .expect("error spawning repl");

    loop {
        let mut send_ack = false;
        let mut str_lvalue = receiver_li.recv().expect("bug in lisp interpretor");

        if str_lvalue.contains("repl:") {
            // stdout.write_all(b"from repl\n");
            send_ack = true;
            str_lvalue = str_lvalue.replace("repl:", "");
        }

        if str_lvalue == *"exit" {
            break;
        }

        //stdout.write_all(format!("receiving command: {}\n", str_lvalue).as_bytes());

        let lvalue = match parse(str_lvalue.as_str(), env, ctxs) {
            Ok(lv) => lv,
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                eprintln!("ELI>>{}", e);
                LValue::None
            }
        };
        //stdout.write_all(b"parsing done\n");
        match eval(&lvalue, env, ctxs) {
            Ok(lv) => match lv {
                LValue::None => {}
                lv => {
                    //stdout.write_all(format!("LI>> {}\n", lv).as_bytes()).expect("error stdout");
                    println!("LI>> {}", lv);
                }
            },
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                eprintln!("ELI>>{}", e);
            }
        };
        if send_ack {
            sender_repl
                .send("ACK".to_string())
                .expect("error sending ack to repl");
        }
        //stdout.write_all(b"eval done\n");
    }

    repl_join_handle.join().expect("error exiting repl");
}
