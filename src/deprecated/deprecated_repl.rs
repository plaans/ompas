use crate::deprecated::deprecated_fact_base::commands::*;
use crate::deprecated::deprecated_fact_base::*;
use aries_planning::parsing::sexpr::{parse, SExpr};
use aries_utils::input::{ErrLoc, Input};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{Error, Read, Write};
use std::{env, io};

const STR_HELP:&str =
    "FactBase version 0.1.0
Commands:
-((hist))                       : Print a list of all commands.
-((get-all))                    : Print all the fact-base in a pretty way.
-((close)) | ((exit))           : Exit the repl.
-((path))                       : Print the current repository.
-((read x))                     : Read a factbase from a file x.fb.
-((write x))                    : Write factbase to file x.fb.
-((let (type t)))               : Define a new type.
-((let (object o - t)))         : Define a new object of type t.
-((let (sf sv t1 ... tn tv))) : Define a new predicate with n parameters types and a value type.
-((let (const{optional}         : Define a new var with a type and a value of the right type.
        var x - type{optional}    Can be a constant.
        = value{optional})))
-((let (const{optional}         : Define a new state-variable with n parameters and a value.
        sv n_sv p1...pn value)))  Types must match with state-function types.
-((get (x)))                    : get a fact
-((set (var value)))            : set a variable with a new value, must be previously defined.
-((set (n_sv p1...pn value)))   : set a state-variable with a new value, must be previously defined";

#[warn(unused_attributes)]
pub struct Repl {
    commands: Vec<SExpr>,
    fact_base: FactBase,
}

impl Default for Repl {
    fn default() -> Self {
        Repl {
            commands: vec![],
            fact_base: Default::default(),
        }
    }
}

enum ReplOk {
    SExpr(SExpr),
    String(String),
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

impl From<FactBaseOk> for ReplOk {
    fn from(ok: FactBaseOk) -> Self {
        match ok {
            FactBaseOk::Ok => ReplOk::Ok,
            FactBaseOk::SExpr(s) => ReplOk::SExpr(s),
            FactBaseOk::String(s) => ReplOk::String(s),
        }
    }
}

impl Into<FactBaseOk> for ReplOk {
    fn into(self) -> FactBaseOk {
        match self {
            ReplOk::SExpr(s) => FactBaseOk::SExpr(s),
            ReplOk::String(s) => FactBaseOk::String(s),
            ReplOk::Exit => FactBaseOk::Ok,
            ReplOk::Ok => FactBaseOk::Ok,
        }
    }
}

impl Display for ReplOk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ReplOk::SExpr(s) => write!(f, "{}", s),
            ReplOk::Exit => write!(f, "EXIT REPL"),
            ReplOk::Ok => write!(f, "Ok"),
            ReplOk::String(s) => write!(f, "{}", s),
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

impl From<std::io::Error> for ReplError {
    fn from(e: Error) -> Self {
        ReplError::Default(e.to_string())
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
    fn commands_to_string(&self) -> String {
        let mut string = String::new();
        let len = self.commands.len();
        for (i, command) in self.commands.iter().enumerate() {
            string.push_str(format!("{} - {}\n", len - i, command).as_str())
        }
        string
    }

    fn read(&mut self) -> ReplResult {
        print!("\nroot>");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();
        let stdin = io::stdin(); // We get `Stdin` here.
        stdin
            .read_line(&mut buffer)
            .expect("Something went wrong..");
        match parse(buffer.as_str()) {
            Ok(s) => {
                self.commands.push(s.clone());
                Ok(ReplOk::SExpr(s))
            }
            Err(e) => Err(ReplError::Default(
                format!("Error in command: {}", e.to_string()).to_string(),
            )),
        }
    }

    fn eval(&mut self, commands: SExpr) -> ReplResult {
        let evaluation = ReplOk::Ok;
        let commands = &mut commands
            .as_list_iter()
            .ok_or_else(|| commands.invalid("Expected a list"))?;

        for current in commands {
            let mut command = current
                .as_list_iter()
                .ok_or_else(|| current.invalid("Expected a command list"))?;
            let result = match command.pop_atom()?.as_str() {
                COMMAND_DEFINE => self.fact_base.add_fact(command),
                COMMAND_MODIFY => self.fact_base.set_fact(command),
                COMMAND_GET => self.fact_base.get_fact(command),
                COMMAND_DEL => self.fact_base.del_fact(command),
                COMMAND_TEST => self.fact_base.test_fact(command),
                COMMAND_PRINT => {
                    //println!("print the sexpr");
                    Ok(FactBaseOk::Ok)
                }
                COMMAND_HELP => {
                    //println!("print help");
                    Ok(help()?.into())
                }
                COMMAND_CLOSE | COMMAND_EXIT => {
                    //println!("quit repl");
                    return Ok(ReplOk::Exit);
                }
                COMMAND_GET_ALL => Ok(FactBaseOk::String(self.fact_base.to_string())),

                COMMAND_READ => {
                    // println!("get fact base from file");
                    let file_name = command.pop_atom()?.as_str();
                    let file_name = format!("{}.{}", file_name, FILE_EXTENSION);
                    let mut file = File::open(file_name)?;
                    let mut buffer = String::new();
                    file.read_to_string(&mut buffer)?;
                    match parse(Input::from_string(buffer)) {
                        Ok(s) => Ok(self.eval(s)?.into()),
                        Err(e) => {
                            return Err(ReplError::Default(
                                format!("Error in command: {}", e.to_string()).to_string(),
                            ))
                        }
                    }
                }

                COMMAND_WRITE => {
                    let file_name = command.pop_atom()?.as_str();
                    let mut file = File::create(format!("{}.{}", file_name, FILE_EXTENSION))?;
                    let string = self.fact_base.to_commands()?;
                    file.write_all(string.as_bytes())?;
                    println!("write fact base to file");
                    Ok(FactBaseOk::Ok)
                }
                HIST_LONG | HIST_SHORT => {
                    println!("print history");
                    Ok(FactBaseOk::String(self.commands_to_string()))
                }

                COMMAND_PATH => {
                    let path = env::current_dir()?;
                    Ok(FactBaseOk::String(format!("{}", path.display())))
                }
                other_command => {
                    return Err(ReplError::Default(format!(
                        "unknown command : {}",
                        other_command
                    )));
                }
            };

            match result {
                Ok(ok) => self.print(ok.into()),
                Err(e) => println!("{}", e),
            }
        }

        Ok(evaluation)
    }

    fn print(&self, s: ReplOk) {
        match s {
            ReplOk::Ok => {}
            _ => println!("{}", s),
        };
    }

    pub fn run(&mut self) {
        let mut run = true;
        while run {
            let command = self.read();
            match command {
                Ok(ReplOk::SExpr(se)) => match self.eval(se) {
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
    Ok(ReplOk::String(STR_HELP.to_string()))
}
