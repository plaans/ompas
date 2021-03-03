//imports for rustyline
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::LError::*;
use crate::lisp::lisp_struct::*;
use crate::lisp::LEnv;
use aries_planning::parsing::sexpr::{parse, SExpr};
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn test_rustyline() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut env: LEnv = Default::default();
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

pub fn eval(se: &SExpr, env: &mut LEnv) -> Result<LValue, LError> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            //Test if its an int
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValue::Atom(LAtom::Number(LNumber::Int(int)))),
                Err(_) => match atom.as_str().parse::<f64>() { //Test if its a float
                    Ok(float) => Ok(LValue::Atom(LAtom::Number(LNumber::Float(float)))),
                    Err(_) => match atom.as_str() { //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            Ok(LValue::Atom(LAtom::Bool(true)))
                        }
                        FALSE => {
                            //println!("atom is boolean false");
                            Ok(LValue::Atom(LAtom::Bool(false)))
                        }
                        s => { //is a symbol, if it exist return it
                            //println!("atom is a symbol: {}", s);
                            return match env.get_symbol(s.to_string()) {
                                Ok(s) => Ok(s),
                                Err(_) => return Ok(LValue::Atom(LAtom::Symbol(s.into()))),
                            }
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
                    env.add_entry(sym, exp);
                }
                IF => {
                    let test = list_iter.pop()?;
                    let conseq = list_iter.pop()?;
                    let alt = list_iter.pop()?;
                    return match eval(test, env) {
                        Ok(LValue::Atom(LAtom::Bool(true))) => {
                            eval(conseq, env)
                        }
                        Ok(LValue::Atom(LAtom::Bool(false))) => {
                            eval(alt, env)
                        }
                        Ok(lv) => Err(WrongType(lv.into(), NameTypeLValue::BAtom)),
                        Err(e) => Err(e)
                    }

                }
                READ => {
                    unimplemented!()
                }
                WRITE => {
                    unimplemented!()
                }
                //println!("conditional"),
                _ => is_first_atom_function = true,
            }
            if is_first_atom_function {
                //println!("{} is a function",first_atom);
                let proc = match eval(&SExpr::Atom(first_atom.clone()), env)? {
                    LValue::LFn(f) => f,
                    lv => return Err(WrongType(lv.into(), NameTypeLValue::LFn)),
                };
                let mut args: Vec<LValue> = Vec::new();
                for arg in list_iter {
                    args.push(eval(arg, env)?)
                }
                //println!("args:{:?}", args);
                return proc(args);
            }
        }
    };
    Ok(LValue::None)
}
