//imports for rustyline
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::LispError::*;
use crate::lisp::lisp_struct::*;
use crate::lisp::LispEnv;
use aries_planning::parsing::sexpr::{parse, SExpr};
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn test_rustyline() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut env: LispEnv = Default::default();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("Line: {}", line);
                match parse(line.as_str()) {
                    Ok(s) => {
                        match eval(&s, &mut env) {
                            Ok(lisp_value) => println!("{}", lisp_value),
                            Err(e) => println!("{}", e),
                        };
                    }
                    Err(e) => println!("Error in command: {}", e.to_string()),
                };
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

pub fn eval(se: &SExpr, env: &mut LispEnv) -> Result<LispValue, LispError> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            let r_int = atom.as_str().parse::<i64>();
            return match r_int {
                Ok(int) => {
                    //println!("atom is a number: {}", int);
                    Ok(LispValue::Atom(LispAtom::Number(LispNumber::Int(int))))
                }
                Err(_) => match atom.as_str() {
                    TRUE => {
                        //println!("atom is boolean true");
                        Ok(LispValue::Atom(LispAtom::Bool(true)))
                    }
                    FALSE => {
                        //println!("atom is boolean false");
                        Ok(LispValue::Atom(LispAtom::Bool(false)))
                    }
                    s => {
                        //println!("atom is a symbol: {}", s);
                        return match env.get_symbol(s.to_string()) {
                            Ok(s) => Ok(s),
                            Err(_) => return Ok(LispValue::Atom(LispAtom::Symbol(s.into()))),
                        }
                    }
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let mut list_iter = list.iter();
            let first_atom = list_iter.pop_atom()?.clone();
            let mut is_first_atom_function: bool = false;
            match first_atom.as_str() {
                DEFINE => {
                    //println!("define a new symbol");
                    let sym = list_iter.pop_atom()?.to_string();
                    let sexpr = list_iter.pop()?;
                    let exp = eval(sexpr, env)?;
                    env.symbols.insert(sym, exp);
                }
                IF => {} //println!("conditional"),
                _ => is_first_atom_function = true,
            }
            if is_first_atom_function {
                //println!("{} is a function",first_atom);
                let proc = match eval(&SExpr::Atom(first_atom.clone()), env)? {
                    LispValue::LispFn(f) => f,
                    lv => return Err(WrongType(lv.into(), NameTypeLispValue::LispFn)),
                };
                let mut args: Vec<LispValue> = Vec::new();
                for arg in list_iter {
                    args.push(eval(arg, env)?)
                }
                //println!("args:{:?}", args);
                return proc(args);
            }
        }
    };
    Ok(LispValue::None)
}
