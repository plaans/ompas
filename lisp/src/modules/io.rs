//!
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError;
use crate::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::{GetModule, Module};
use crate::core::structs::typelvalue::TypeLValue;
use crate::lisp_interpreter::ChannelToLispInterpreter;
use crate::modules::doc::{Documentation, LHelp};
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::sync::Arc;

/*
LANGUAGE
 */

const MOD_IO: &str = "io";
const DOC_MOD_IO: &str = "Module than handles input/output functions.";
const DOC_MOD_IO_VERBOSE: &str = "functions:\n\
                                    -print\n\
                                    -read\n\
                                    -write";

const PRINT: &str = "print";
const READ: &str = "__read__";
const WRITE: &str = "write";
//const LOAD: &str = "load";

const MACRO_READ: &str = "(defmacro read \
    (lambda (x)\
        `(eval (parse (__read__ ,x)))))\
";

#[derive(Debug)]
pub enum LogOutput {
    Stdout,
    File(PathBuf),
}

impl From<PathBuf> for LogOutput {
    fn from(pb: PathBuf) -> Self {
        Self::File(pb)
    }
}

/// Handles the channel to communicate with the Lisp Interpreter
/// Note: Be careful when there is response on the receiver
#[derive(Debug)]
pub struct CtxIo {
    sender_li: Option<ChannelToLispInterpreter>,
    log: LogOutput,
}

impl Default for CtxIo {
    fn default() -> Self {
        Self {
            sender_li: None,
            log: LogOutput::Stdout,
        }
    }
}

impl CtxIo {
    ///Set the sender to lisp interpreter
    /// Used to send commands to execute
    pub fn add_communication(&mut self, com: ChannelToLispInterpreter) {
        self.sender_li = Some(com);
    }
    ///Set the log output
    pub fn set_log_output(&mut self, output: LogOutput) {
        self.log = output
    }
}

/*pub fn load(args: &[LValue], _: &LEnv, _: & CtxIo ) -> Result<LValue, LError> {
    println!("moudle Io: load");
    Ok(LValue::None)
}*/

impl GetModule for CtxIo {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![MACRO_READ].into(),
            label: MOD_IO.into(),
        };

        module.add_fn_prelude(PRINT, print);
        module.add_fn_prelude(READ, read);
        module.add_fn_prelude(WRITE, write);

        module
    }
}

/*
DOCUMENTATION
 */

const DOC_PRINT: &str = "Print in stdout a LValue.";
const DOC_PRINT_VERBOSE: &str = "Takes a list of arguments and print them in stdout.";
const DOC_READ: &str = "Read a file an evaluate it";
const DOC_READ_VERBOSE: &str = "Takes the name of the file as argument.\n\
                                Note: The file path is relative to the path of the executable.\n\
                                Return an error if the file is not found or there is a problem while parsing and evaluation.";
const DOC_WRITE: &str = "Write a LValue to a file";
const DOC_WRITE_VERBOSE: &str = "Takes two arguments: the name of the file and the LValue\n\
                                 Note: The path of the file is relative to the path of the executable";

impl Documentation for CtxIo {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(MOD_IO, DOC_MOD_IO, DOC_MOD_IO_VERBOSE),
            LHelp::new_verbose(PRINT, DOC_PRINT, DOC_PRINT_VERBOSE),
            LHelp::new_verbose(READ, DOC_READ, DOC_READ_VERBOSE),
            LHelp::new_verbose(WRITE, DOC_WRITE, DOC_WRITE_VERBOSE),
        ]
    }
}

/// Prints in a the LogOutput the LValue.
/// If it is stdout, it is printed in the terminal
/// Otherwise in the configured file.
/// If the file is missing, it prints nothing.
pub fn print(args: &[LValue], _: &LEnv, ctx: &CtxIo) -> Result<LValue, LError> {
    let lv: LValue = match args.len() {
        0 => LValue::Nil,
        1 => args[0].clone(),
        _ => args.into(),
    };
    match &ctx.log {
        LogOutput::Stdout => println!("{}", lv),
        LogOutput::File(pb) => match File::open(pb) {
            Ok(_) => {}
            Err(_) => {
                let mut file = File::create(pb)?;
                file.write_all(format!("{}\n", lv).as_bytes())?;
            }
        },
    };

    Ok(LValue::Nil)
}

/// Read the content of a file and sends the content to the lisp interpreter.
/// The name of the file is given via args.
pub fn read(args: &[LValue], _: &LEnv, _: &CtxIo) -> Result<LValue, LError> {
    //let mut stdout = io::stdout();
    //stdout.write_all(b"module Io: read\n");
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(READ, args.into(), args.len(), 1..1));
    }
    let file_name = match &args[0] {
        LValue::Symbol(s) => s.to_string(),
        lv => return Err(WrongType(READ, lv.clone(), lv.into(), TypeLValue::Symbol)),
    };

    let mut file = match File::open(&file_name) {
        Ok(f) => f,
        Err(e) => return Err(SpecialError(READ, format!("{}: {}", file_name, e))),
    };
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    //stdout.write_all(format!("contents: {}\n", contents).as_bytes());

    //TODO: no check of the response to read

    Ok(LValue::String(contents))
}

/// Write an lvalue to a given file
///
/// # Example:
/// ```lisp
/// (write <file> <lvalue>)
pub fn write(args: &[LValue], _: &LEnv, _: &CtxIo) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(WRITE, args.into(), args.len(), 2..2));
    }

    match &args[0] {
        LValue::Symbol(s) => {
            //got our file name
            let mut f = File::create(s.to_string())?;
            f.write_all(args[1].to_string().as_bytes())?;
            Ok(LValue::Nil)
        }
        lv => Err(WrongType(WRITE, lv.clone(), lv.into(), TypeLValue::Symbol)),
    }

    //println!("module Io: write");
}

//TODO: finish writing tests for io
#[cfg(test)]
pub mod tests {
    #[test]
    pub fn test_read() {
        assert!(true)
    }

    #[test]
    pub fn test_write() {
        assert!(true)
    }
}
