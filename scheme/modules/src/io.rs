use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::{lerror, string};
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::PathBuf;
use tokio::sync::mpsc;

/*
LANGUAGE
 */

pub const MOD_IO: &str = "io";
pub const DOC_MOD_IO: &str = "Module than handles input/output functions.";
pub const DOC_MOD_IO_VERBOSE: &str = "functions:\n\
                                    -print\n\
                                    -read\n\
                                    -write";

pub const PRINT: &str = "print";
pub const READ: &str = "__read__";
pub const WRITE: &str = "write";
//const LOAD: &str = "load";

/*
DOCUMENTATION
 */

pub const DOC_PRINT: &str = "Print in stdout a LValue.";
pub const DOC_PRINT_VERBOSE: &str = "Takes a list of arguments and print them in stdout.";
pub const DOC_READ: &str = "Read a file an evaluate it";
pub const DOC_READ_VERBOSE: &str = "Takes the name of the file as argument.\n\
                                Note: The file path is relative to the path of the executable.\n\
                                Return an error if the file is not found or there is a problem while parsing and evaluation.";
pub const DOC_WRITE: &str = "Write a LValue to a file";
pub const DOC_WRITE_VERBOSE: &str = "Takes two arguments: the name of the file and the LValue\n\
                                 Note: The path of the file is relative to the path of the executable";

const MACRO_READ: &str = "(defmacro read \
    (lambda (x)\
        `(eval (parse (__read__ ,x)))))\
";

#[derive(Debug)]
pub enum LogOutput {
    Stdout,
    File(PathBuf),
    Channel(mpsc::Sender<String>),
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
    log: LogOutput,
}

impl Default for CtxIo {
    fn default() -> Self {
        Self {
            log: LogOutput::Stdout,
        }
    }
}

impl CtxIo {
    ///Set the log output
    pub fn set_log_output(&mut self, output: LogOutput) {
        self.log = output
    }
}

/*pub fn load(args: &[LValue], _: &LEnv, _: & CtxIo ) -> Result<LValue, LError> {
    println!("moudle Io: load");
    Ok(LValue::None)
}*/

impl IntoModule for CtxIo {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: vec![MACRO_READ].into(),
            label: MOD_IO.into(),
        };

        module.add_fn_prelude(PRINT, print);
        module.add_fn_prelude(READ, read);
        module.add_fn_prelude(WRITE, write);

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new_verbose(MOD_IO, DOC_MOD_IO, DOC_MOD_IO_VERBOSE),
            LHelp::new_verbose(PRINT, DOC_PRINT, DOC_PRINT_VERBOSE),
            LHelp::new_verbose(READ, DOC_READ, DOC_READ_VERBOSE),
            LHelp::new_verbose(WRITE, DOC_WRITE, DOC_WRITE_VERBOSE),
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

/// Prints in a the LogOutput the LValue.
/// If it is stdout, it is printed in the terminal
/// Otherwise in the configured file.
/// If the file is missing, it prints nothing.
#[scheme_fn]
pub fn print(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let lv: LValue = match args.len() {
        0 => LValue::Nil,
        1 => args[0].clone(),
        _ => args.into(),
    };

    let ctx = env.get_context::<CtxIo>(MOD_IO)?;

    match &ctx.log {
        LogOutput::Stdout => {
            println!("{}", lv);
        }
        LogOutput::Channel(tx) => {
            let tx = tx.clone();
            tokio::spawn(async move { tx.send(format!("PRINT - {}", lv)).await });
        }
        LogOutput::File(pb) => {
            //println!("print {} in {:?}", lv, pb);
            let mut file = OpenOptions::new()
                .write(true)
                .append(true)
                .create(true)
                .open(pb)
                .expect("error creating print file");
            file.write_all(format!("PRINT - {}\n", lv).as_bytes())?;
        }
    };
    Ok(())
}

/// Read the content of a file and sends the content to the lisp interpreter.
/// The name of the file is given via args.
#[scheme_fn]
pub fn read(file_name: Sym) -> LResult {
    let mut file = match File::open(&file_name) {
        Ok(f) => f,
        Err(e) => return Err(lerror!(READ, format!("{}: {}", file_name, e))),
    };
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    //stdout.write_all(format!("contents: {}\n", contents).as_bytes());

    //TODO: no check of the response to read

    Ok(string!(contents))
}

/// Write an lvalue to a given file
///
/// # Example:
/// ```lisp
/// (write <file> <lvalue>)
#[scheme_fn]
pub fn write(s: Sym, lv: &LValue) -> Result<(), std::io::Error> {
    let mut f = File::create(s)?;
    f.write_all(lv.to_string().as_bytes())?;
    Ok(())
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
