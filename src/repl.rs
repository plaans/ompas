use std::collections::HashMap;
use aries_model::Label;
use std::fs::read;
use std::io::{self, Read, Write};
use aries_planning::parsing::sexpr::{SExpr, parse, SAtom};
use aries_utils::input::{Input, ErrLoc};
use aries_planning::parsing::sexpr::SExpr::Atom;
use crate::facts::FactBase;
use std::convert::TryInto;
use std::ops::Deref;

struct Function {
    label: Label,
    ref_function: usize,
}

#[derive(Default)]
pub struct Repl{
    commands: Vec<SExpr>,
    functions: Vec<Function>,
    factBase: FactBase,
}


impl Repl {
    fn read(&self) -> Result<SExpr, String> {
        print!("\n>>");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        let mut stdin = io::stdin(); // We get `Stdin` here.
        stdin.read_line(&mut buffer).expect("Something went wrong..");
        match parse(Input::from_string(buffer)) {
            Ok(s) => Ok(s),
            Err(e) => Err(format!("Error in command: {}", e.to_string()).to_string())
        }
    }

    fn eval(&mut self, command: SExpr) -> Result<SExpr, ErrLoc> {
        let mut evaluation = SExpr::Atom(SAtom::new("ok".to_string()));
        let command = &mut command.as_list_iter().ok_or_else(|| command.invalid("Expected a list"))?;

        for current in command {
            let mut command = current.as_list_iter().ok_or_else(|| current.invalid("Expected a command list"))?;
            match command.pop_atom()?.as_str() {
                "let" => {
                    //println!("define a new variable");
                    let len = command.len();
                    let mut key: Vec<_> = vec![];
                    for i in 0..len - 1 {
                        key.push(command.pop_atom()?.clone());
                    }
                    let value = command.pop_atom()?.clone();
                    self.factBase.add(key, value);
                },
                "set" => {
                    let len = command.len();
                    let mut key: Vec<_> = vec![];
                    for i in 0..len - 1 {
                        key.push(command.pop_atom()?.clone());
                    }
                    let value = command.pop_atom()?.clone();
                    self.factBase.set(key, value).unwrap_or(println!("wrong key"));
                    //println!("change the value of the variable")
                },
                "get" => {
                    let mut key: Vec<SAtom> = vec![];
                    for i in 0..command.len() {
                        key.push(command.pop_atom()?.clone());
                    }
                    //println!("get the value");
                    evaluation = SExpr::Atom(self.factBase.get(key));
                },
                "print" => println!("print the sexpr"),
                "help" => println!("print help"),
                "exit" => println!("quit repl"),
                "getall" => {
                    println!("{:?}", self.factBase);
                },
                _ => println!("unnamed command")
            };
        }

        Ok(evaluation)

    }

    fn print(&self, s: SExpr) {
        println!("{}", s);
    }

    pub fn run(&mut self) {
        let run = true;
        while run{
            let command = self.read();
            match command {
                Ok(se) => {
                    match self.eval(se){
                        Ok(se) => self.print(se),
                        Err(e) => println!("{}", e)
                    }
                },
                Err(e) => println!("{}", e)
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

