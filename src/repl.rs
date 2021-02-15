#[warn(unused_imports)]
use crate::facts::{FactBase, FactBaseError};
use aries_model::Label;
use aries_planning::parsing::sexpr::{parse, SAtom, SExpr};
use aries_utils::input::{ErrLoc, Input};
use std::io::{self, Write};

struct Function {
    label: Label,
    ref_function: usize,
}

#[derive(Default)]
pub struct Repl {
    commands: Vec<SExpr>,
    functions: Vec<Function>,
    fact_base: FactBase,
}

enum ReplResult {
    Sexpr(SExpr),
    Error(FactBaseError),
    Exit,
    Ok,
}

impl From<Result<(), FactBaseError>> for ReplResult {
    fn from(r: Result<(), FactBaseError>) -> Self {
        match r {
            Ok(_) => ReplResult::Ok,
            Err(e) => ReplResult::Error(e),
        }
    }
}

impl From<Result<SExpr, FactBaseError>> for ReplResult {
    fn from(r: Result<SExpr, FactBaseError>) -> Self {
        match r {
            Ok(s) => ReplResult::Sexpr(s),
            Err(e) => ReplResult::Error(e),
        }
    }
}

impl Repl {
    fn read(&self) -> Result<SExpr, String> {
        print!("\n>>");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        let mut stdin = io::stdin(); // We get `Stdin` here.
        stdin
            .read_line(&mut buffer)
            .expect("Something went wrong..");
        match parse(Input::from_string(buffer)) {
            Ok(s) => Ok(s),
            Err(e) => Err(format!("Error in command: {}", e.to_string()).to_string()),
        }
    }

    fn eval(&mut self, commands: SExpr) -> Result<SExpr, ErrLoc> {
        let mut evaluation = SExpr::Atom(SAtom::new("ok".to_string()));
        let commands = &mut commands
            .as_list_iter()
            .ok_or_else(|| commands.invalid("Expected a list"))?;

        for current in commands {
            let mut command = current
                .as_list_iter()
                .ok_or_else(|| current.invalid("Expected a command list"))?;
            let result = match command.pop_atom()?.as_str() {
                "let" => self.fact_base.add_new_fact(command).into(),
                "set" => self.fact_base.set_fact(command).into(),
                "get" => self.fact_base.get_fact(command).into(),
                "print" => {
                    println!("print the sexpr");
                    ReplResult::Ok
                }
                "help" => {
                    println!("print help");
                    ReplResult::Ok
                }
                "exit" => {
                    println!("quit repl");
                    ReplResult::Ok
                }
                "getall" => {
                    println!("{}", self.fact_base);
                    ReplResult::Ok
                }
                other_command => {
                    println!("unnamed command");
                    ReplResult::Error(FactBaseError::Default(format!("unknown command : {}", other_command)))
                }
            };

            match result {
                ReplResult::Error(e) => println!("{}", e),
                _ => {}
            };
        }

        Ok(evaluation)
    }

    fn print(&self, s: SExpr) {
        println!("{}", s);
    }

    pub fn run(&mut self) {
        let run = true;
        while run {
            let command = self.read();
            match command {
                Ok(se) => match self.eval(se) {
                    Ok(se) => self.print(se),
                    Err(e) => println!("{}", e),
                },
                Err(e) => println!("{}", e),
            };
        }
    }
}

fn help() -> String {
    "This is the help of the repl".to_string()
}

fn close() -> String {
    "".to_string()
}
