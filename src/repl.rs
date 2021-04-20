//imports for rustyline
use crate::lisp_root::lisp_struct::*;
use crate::lisp_root::{eval, parse, RefLEnv, ContextCollection, load_module};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use crate::lisp_modules::counter::Counter;

pub fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut root_env= &mut RefLEnv::root();
    let mut ctxs: &mut ContextCollection =  &mut Default::default();
    load_module(root_env, ctxs, Counter::get_module() );
    let mut env = &mut RefLEnv::new_from_outer(root_env.clone());

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
                match eval(&lvalue, env ,ctxs) {
                    Ok(lv) => match lv {
                        LValue::None => {},
                        lv => println!("{}", lv),
                    }
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


