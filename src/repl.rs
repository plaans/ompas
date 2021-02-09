use std::collections::HashMap;
use aries_model::Label;
use std::fs::read;
use std::io::{self, Read, Write};

struct Function {
    label: Label,
    ref_function: usize,
}

pub struct Repl{
    functions:
}

impl Repl {
    fn read(&self) -> String {
        print!("\n>>");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        let mut stdin = io::stdin(); // We get `Stdin` here.
        stdin.read_line(&mut buffer).expect("Something went wrong..");
        buffer
    }

    fn eval(&self, s: String) -> String {
        "".to_string()
    }

    fn print(&self, s: String) {
        println!("{}", s);
    }

    pub fn run(&self) {
        let run = true;
        while run{
            let command = self.read();
            self.print(command);

        }
    }
}




mod functions {
    fn help() -> String {
        "This is the help of the repl".to_string()
    }

    fn close() -> String {

    }



}
