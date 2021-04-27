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
use std::thread;
#[warn(unused_imports)]
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

    //Channel from repl to Lisp Interpretor
    let (tx1, rx1): (Sender<String>, Receiver<String>) = channel();
    //Channel from Lisp Interpretor to repl
    let (tx2, rx2): (Sender<String>, Receiver<String>) = channel();

    let _li_join_handle = thread::Builder::new()
        .name("Lisp Interpreter".to_string())
        .spawn(move || {
            lisp_interpretor(tx2.clone(), rx1);
        })
        .expect("bug spawning lisp interpreter");

    repl_2(tx1.clone(), rx2);
}

pub fn lisp_interpretor(_: Sender<String>, receiver: Receiver<String>) {
    let root_env = &mut RefLEnv::root();
    let ctxs: &mut ContextCollection = &mut Default::default();
    load_module(root_env, ctxs, CtxCounter::get_module());
    load_module(root_env, ctxs, CtxIO::get_module());
    load_module(root_env, ctxs, CtxType::get_module());
    load_module(root_env, ctxs, CtxMath::get_module());
    load_module(root_env, ctxs, CtxRobot::get_module());
    let env = &mut RefLEnv::new_from_outer(root_env.clone());

    loop {
        let str_lvalue = receiver.recv().expect("bug in lisp interpretor");
        let lvalue = match parse(str_lvalue.as_str(), env, ctxs) {
            Ok(lv) => lv,
            Err(e) => {
                eprintln!("ELI>>{}", e);
                LValue::None
            }
        };
        match eval(&lvalue, env, ctxs) {
            Ok(lv) => match lv {
                LValue::None => {}
                lv => println!("LI>> {}", lv),
            },
            Err(e) => eprintln!("ELI>>{}", e),
        };
    }
}
