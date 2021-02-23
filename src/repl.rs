use crate::fact_base::commands::*;
#[warn(unused_imports)]
use crate::fact_base::{FactBase, FactBaseError};
use crate::fact_base::{FactBaseOk, FILE_EXTENSION};
use aries_planning::parsing::sexpr::{parse, SExpr};
use aries_utils::input::{ErrLoc, Input};
use std::env;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{self, Error, Read, Write};

//imports for rustyline
use rustyline::error::ReadlineError;
use rustyline::Editor;




pub fn test_rustyline() {
        // `()` can be used when no completer is required
        let mut rl = Editor::<()>::new();
        if rl.load_history("history.txt").is_err() {
            println!("No previous history.");
        }
        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    println!("Line: {}", line);
                },
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break
                },
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break
                },
                Err(err) => {
                    println!("Error: {:?}", err);
                    break
                }
            }
        }
        rl.save_history("history.txt").unwrap();
}