//imports for rustyline
use crate::lisp::lisp_language::*;
use crate::lisp::lisp_struct::LError::*;
use crate::lisp::lisp_struct::*;
use crate::lisp::LEnv;
use aries_planning::parsing::sexpr::{parse, SExpr};
use aries_utils::input::{Input, Sym};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs::File;
use std::io::Read;

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
                Ok(int) => Ok(LValue::Number(LNumber::Int(int))),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValue::Number(LNumber::Float(float))),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            Ok(LValue::Bool(true))
                        }
                        FALSE => {
                            //println!("atom is boolean false");
                            Ok(LValue::Bool(false))
                        }
                        s => {
                            //is a symbol, if it exist return it
                            //println!("atom is a symbol: {}", s);
                            match env.get_symbol(s.to_string()) {
                                Ok(lv) => Ok(lv),
                                Err(_) => Ok(LValue::Symbol(s.into()))
                            }

                        }
                    },
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
                    let sym:Sym = list_iter.pop_atom()?.into();
                    let sexpr = list_iter.pop()?;
                    let exp = eval(sexpr, env)?;
                    match exp {
                        LValue::SymType(st) => {
                            env.add_entry(sym.to_string(), LValue::Symbol(sym.clone()));
                            env.add_sym_type(sym, st);
                        }
                        lv => env.add_entry(sym.to_string(), lv),
                    };
                }
                IF => {
                    let test = list_iter.pop()?;
                    let conseq = list_iter.pop()?;
                    let alt = list_iter.pop()?;
                    return match eval(test, env) {
                        Ok(LValue::Bool(true)) => eval(conseq, env),
                        Ok(LValue::Bool(false)) => eval(alt, env),
                        Ok(lv) => Err(WrongType(lv.to_string(),lv.into(), NameTypeLValue::Bool)),
                        Err(e) => Err(e),
                    };
                }
                READ => {
                    let file_name = list_iter.pop_atom()?.to_string();
                    let mut file = match File::open(file_name) {
                        Ok(f) => f,
                        Err(e) => return Err(SpecialError(e.to_string())),
                    };
                    let mut buffer = String::new();
                    match file.read_to_string(&mut buffer) {
                        Ok(_) => {}
                        Err(e) => return Err(SpecialError(e.to_string())),
                    };
                    match parse(Input::from_string(buffer)) {
                        Ok(s) => return eval(&s, env),
                        Err(e) => return Err(SpecialError(e.to_string())),
                    };
                }
                WRITE => {
                    let name_file = list_iter.pop_atom()?.to_string();
                    env.to_file(name_file);
                    //eprintln!("new entries: {:?}", env.get_new_entries());
                }
                //println!("conditional"),
                _ => is_first_atom_function = true,
            }
            if is_first_atom_function {
                //println!("{} is a function",first_atom);
                let proc = match eval(&SExpr::Atom(first_atom.clone()), env)? {
                    LValue::LFn(f) => f,
                    lv => return Err(WrongType(lv.to_string(), lv.into(), NameTypeLValue::LFn)),
                };
                let mut args: Vec<LValue> = Vec::new();
                for arg in list_iter {
                    args.push(eval(arg, env)?)
                }
                //println!("args:{:?}", args);
                return proc(args.as_slice(), env);
            }
        }
    };
    Ok(LValue::None)
}
