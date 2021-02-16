#[warn(unused_imports)]
use crate::facts::{FactBase, FactBaseError};
use crate::repl::commands::*;
use aries_planning::parsing::sexpr::{parse, SAtom, SExpr};
use aries_utils::input::{ErrLoc, Input};
use std::fmt::{Display, Formatter};
use std::io::{self, Write};

mod commands {
    pub const COMMAND_HELP: &str = "help";
    pub const COMMAND_DEFINE: &str = "let";
    pub const COMMAND_MODIFY: &str = "set";
    pub const COMMAND_GET: &str = "get";
    pub const COMMAND_PRINT: &str = "print";

    pub const COMMAND_EXIT: &str = "exit";
    pub const COMMAND_CLOSE: &str = "close";
    pub const COMMAND_GET_ALL: &str = "get-all";
}

#[derive(Default)]
#[warn(unused_attributes)]
pub struct Repl {
    commands: Vec<SExpr>,
    fact_base: FactBase,
}

enum ReplOk {
    SExpr(SExpr),
    Exit,
    Ok,
}

impl From<()> for ReplOk {
    fn from(_: ()) -> Self {
        ReplOk::Ok
    }
}

impl From<SExpr> for ReplOk {
    fn from(s: SExpr) -> Self {
        ReplOk::SExpr(s)
    }
}

impl Display for ReplOk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ReplOk::SExpr(s) => write!(f, "{}", s),
            ReplOk::Exit => write!(f, "EXIT REPL"),
            ReplOk::Ok => write!(f, "OK"),
        }
    }
}

enum ReplError {
    FactBaseError(FactBaseError),
    ErrLoc(ErrLoc),
    Default(String),
}

impl From<ErrLoc> for ReplError {
    fn from(e: ErrLoc) -> Self {
        ReplError::ErrLoc(e)
    }
}

impl From<FactBaseError> for ReplError {
    fn from(e: FactBaseError) -> Self {
        ReplError::FactBaseError(e)
    }
}

impl Display for ReplError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ReplError::FactBaseError(e) => write!(f, "{}", e),
            ReplError::ErrLoc(e) => write!(f, "{}", e),
            ReplError::Default(e) => write!(f, "{}", e),
        }
    }
}

type ReplResult = Result<ReplOk, ReplError>;

impl Repl {
    fn read(&mut self) -> ReplResult {
        print!("\n>>");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        let stdin = io::stdin(); // We get `Stdin` here.
        stdin
            .read_line(&mut buffer)
            .expect("Something went wrong..");
        match parse(Input::from_string(buffer)) {
            Ok(s) => {
                self.commands.push(s.clone());
                Ok(ReplOk::SExpr(s))
            }
            Err(e) => Err(ReplError::FactBaseError(FactBaseError::Default(
                format!("Error in command: {}", e.to_string()).to_string(),
            ))),
        }
    }

    fn eval(&mut self, commands: SExpr) -> ReplResult {
        let evaluation = SExpr::Atom(SAtom::new("ok".to_string()));
        let commands = &mut commands
            .as_list_iter()
            .ok_or_else(|| commands.invalid("Expected a list"))?;

        for current in commands {
            let mut command = current
                .as_list_iter()
                .ok_or_else(|| current.invalid("Expected a command list"))?;
            //TODO: unify the print in the print function of repl
            let _result: ReplOk = match command.pop_atom()?.as_str() {
                COMMAND_DEFINE => self.fact_base.add_new_fact(command)?.into(),
                COMMAND_MODIFY => self.fact_base.set_fact(command)?.into(),
                COMMAND_GET => self.fact_base.get_fact(command)?.into(),
                COMMAND_PRINT => {
                    println!("print the sexpr");
                    ReplOk::Ok
                }
                COMMAND_HELP => {
                    println!("print help");
                    help()?
                }
                COMMAND_CLOSE | COMMAND_EXIT => {
                    println!("quit repl");
                    return Ok(ReplOk::Exit);
                }
                COMMAND_GET_ALL => {
                    println!("{}", self.fact_base);
                    ReplOk::Ok
                }
                other_command => {
                    println!("unnamed command");
                    return Err(ReplError::Default(format!(
                        "unknown command : {}",
                        other_command
                    )));
                }
            };
        }

        Ok(ReplOk::SExpr(evaluation))
    }

    fn print(&self, s: SExpr) {
        println!("{}", s);
    }

    pub fn run(&mut self) {
        let mut run = true;
        while run {
            let command = self.read();
            match command {
                Ok(ReplOk::SExpr(se)) => match self.eval(se) {
                    Ok(ReplOk::SExpr(se)) => self.print(se),
                    Ok(ReplOk::Exit) => run = false,
                    Err(e) => println!("{}", e),
                    _ => {}
                },
                Err(e) => println!("{}", e),
                _ => {}
            };
        }
    }
}

fn help() -> ReplResult {
    Ok(ReplOk::SExpr(SExpr::Atom(
        "This is the help of the repl".to_string().into(),
    )))
}
