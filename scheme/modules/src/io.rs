use ompas_middleware::logger::LogClient;
use sompas_language::io::*;
use sompas_macros::scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::{lruntimeerror, string};
use std::env;
use std::fs::{File, OpenOptions};
use std::io::{Read, Write};
use std::path::PathBuf;

/*
LANGUAGE
 */

#[derive(Debug)]
pub enum LogOutput {
    Stdout,
    Log(LogClient),
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
pub struct ModIO {
    log: LogOutput,
}

impl Default for ModIO {
    fn default() -> Self {
        Self {
            log: LogOutput::Stdout,
        }
    }
}

impl ModIO {
    ///Set the log output
    pub fn set_log_output(&mut self, output: LogOutput) {
        self.log = output
    }
}

/*pub fn load(args: &[LValue], _: &LEnv, _: & CtxIo ) -> Result<LValue, LError> {
    println!("moudle Io: load");
    Ok(LValue::None)
}*/

impl From<ModIO> for LModule {
    fn from(m: ModIO) -> Self {
        let mut module = LModule::new(m, MOD_IO, DOC_MOD_IO);
        module.add_fn(PRINT, print, (DOC_PRINT, DOC_PRINT_VERBOSE), false);
        module.add_fn(
            __READ__,
            __read__,
            (DOC___READ__, DOC___READ___VERBOSE),
            false,
        );
        module.add_fn(WRITE, write, DOC_WRITE, false);
        module.add_fn(GET_CURRENT_DIR, get_current_dir, DOC_GET_CURRENT_DIR, false);
        module.add_fn(SET_CURRENT_DIR, set_current_dir, DOC_SET_CURRENT_DIR, false);
        module.add_macro(READ, MACRO_READ, DOC_READ);

        module
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

    let ctx = env.get_context::<ModIO>(MOD_IO)?;

    match &ctx.log {
        LogOutput::Stdout => {
            println!("{}", lv);
        }
        LogOutput::Log(log) => {
            let log = log.clone();
            tokio::spawn(async move {
                log.debug(lv).await;
            });
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
pub fn __read__(file_name: String) -> LResult {
    let mut file = match File::open(&file_name) {
        Ok(f) => f,
        Err(e) => return Err(lruntimeerror!(READ, format!("{}: {}", file_name, e))),
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

#[scheme_fn]
pub fn get_current_dir() -> Result<String, String> {
    match env::current_dir() {
        Ok(dir) => match dir.to_str() {
            Some(d) => Ok(d.to_string()),
            None => Err("Could not format PathBuf".to_string()),
        },
        Err(e) => Err(format!("Error getting current_dir: {e}")),
    }
}

#[scheme_fn]
pub fn set_current_dir(dir: String) -> Result<(), String> {
    match env::set_current_dir(dir) {
        Ok(_) => Ok(()),
        Err(e) => Err(format!("Error setting current dir: {e}")),
    }
}
