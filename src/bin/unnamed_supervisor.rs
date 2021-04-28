use fact_base::modules::_type::CtxType;
use fact_base::modules::counter::CtxCounter;
use fact_base::modules::io::CtxIO;
use fact_base::modules::math::CtxMath;
use fact_base::modules::robot::CtxRobot;
use fact_base::core::r#struct::{AsModule, LValue};
use fact_base::core::{eval, load_module, parse, ContextCollection, RefLEnv};
use fact_base::repl::repl_2;
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::{thread, io};
use structopt::StructOpt;
use std::io::{Write, stdout};

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

    //Channel from X to Lisp Interpretor
    let (tx1, rx1): (Sender<String>, Receiver<String>) = channel();
    //Channel from Lisp Interpretor to repl
    let (tx2, rx2): (Sender<String>, Receiver<String>) = channel();
    let tx1_clone = tx1.clone();

    let li_join_handle = thread::Builder::new()
        .name("Lisp Interpreter".to_string())
        .spawn(move|| {
            lisp_interpreter(tx1_clone, tx2.clone(), rx1);
        })
        .expect("bug spawning lisp interpreter");


    repl_2(tx1, rx2);
    li_join_handle.join();
}

pub fn lisp_interpreter(sender_LI: Sender<String>, sender: Sender<String>, receiver: Receiver<String>) {
    let root_env = &mut RefLEnv::root();
    let ctxs: &mut ContextCollection = &mut Default::default();
    load_module(root_env, ctxs, CtxCounter::get_module());
    let id_io = load_module(root_env, ctxs, CtxIO::get_module());
    //Add the sender of the channel.
    let mut ctx_io = ctxs.get_mut_context(id_io).downcast_mut::<CtxIO>().expect("couldn't downcast ref");
    ctx_io.add_sender(sender_LI.clone());
    load_module(root_env, ctxs, CtxType::get_module());
    load_module(root_env, ctxs, CtxMath::get_module());
    load_module(root_env, ctxs, CtxRobot::get_module());
    let env = &mut RefLEnv::new_from_outer(root_env.clone());

    let mut stdout = io::stdout();
    let mut stderr = io::stderr();
    loop {
        let mut send_ACK = false;
        let mut str_lvalue = receiver.recv().expect("bug in lisp interpretor");

        if str_lvalue.contains("repl:") {
            stdout.write_all(b"from repl\n");
            send_ACK = true;
            str_lvalue = str_lvalue.replace("repl:", "");
        }

        if str_lvalue == "exit".to_string() {
            break;
        }

        stdout.write_all(format!("receiving command: {}\n", str_lvalue).as_bytes());

        let lvalue = match parse(str_lvalue.as_str(), env, ctxs) {
            Ok(lv) => lv,
            Err(e) => {
                stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                LValue::None
            }
        };
        //stdout.write_all(b"parsing done\n");
        match eval(&lvalue, env, ctxs) {
            Ok(lv) => match lv {
                LValue::None => {}
                lv => {
                    stdout.write_all(format!("LI>> {}\n", lv).as_bytes());
                },
            },
            Err(e) => {
                stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
            },
        };
        if send_ACK {
            sender.send("ACK".to_string());
        }
        //stdout.write_all(b"eval done\n");
    }
}
