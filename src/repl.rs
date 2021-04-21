use crate::lisp_modules::counter::{CtxCounter};
use crate::lisp_root::lisp_struct::*;
use crate::lisp_root::{eval, load_module, parse, ContextCollection, RefLEnv};
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let root_env = &mut RefLEnv::root();
    let ctxs: &mut ContextCollection = &mut Default::default();
    load_module(root_env, ctxs, CtxCounter::get_module());
    //load_module(root_env, ctxs, IO::get_module());
    let env = &mut RefLEnv::new_from_outer(root_env.clone());

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(string) => {
                let str = string.as_str();
                rl.add_history_entry(str);
                let lvalue = match parse(str, env, ctxs) {
                    Ok(lv) => lv,
                    Err(e) => {
                        eprintln!("{}", e);
                        LValue::None
                    }
                };
                match eval(&lvalue, env, ctxs) {
                    Ok(lv) => match lv {
                        LValue::None => {}
                        lv => println!("{}", lv),
                    },
                    Err(e) => eprintln!("{}", e),
                };

                //println!("Line: {}", line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
