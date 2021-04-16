//imports for rustyline
use crate::lisp::lisp_struct::*;
use crate::lisp::{LEnv, eval};
use rustyline::Editor;
use crate::lisp;
use rustyline::error::ReadlineError;

pub fn repl() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let root_env= LEnv::new_ref_counter();
    let mut env = LEnv::new_ref_counter_from_outer(&root_env);
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(string) => {
                let str = string.as_str();
                rl.add_history_entry(str);
                let lvalue = match lisp::parse(str, &mut env) {
                    Ok(lv) => lv,
                    Err(e) => {
                        eprintln!("{}", e);
                        LValue::None
                    }
                };
                match eval(&lvalue, &mut env) {
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


