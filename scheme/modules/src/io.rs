use ompas_middleware::logger::LogClient;
use sompas_core::parse;
use sompas_language::io::*;
use sompas_macros::{async_scheme_fn, scheme_fn};
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

#[derive(Clone, Default, Debug)]
pub enum LogOutput {
    #[default]
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
#[derive(Default, Debug)]
pub struct ModIO {
    log_output: LogOutput,
}

impl ModIO {
    pub fn new(log_output: LogOutput) -> Self {
        Self { log_output }
    }
    ///Set the log output
    pub fn set_log_output(&mut self, output: LogOutput) {
        self.log_output = output
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
        module.add_fn(READ, read, (DOC_READ, DOC_READ_VERBOSE), false);
        module.add_fn(WRITE, write, DOC_WRITE, false);
        module.add_fn(GET_CURRENT_DIR, get_current_dir, DOC_GET_CURRENT_DIR, false);
        module.add_fn(SET_CURRENT_DIR, set_current_dir, DOC_SET_CURRENT_DIR, false);
        module.add_async_fn(GET_ENV_VAR, get_env_var, DOC_GET_ENV_VAR, false);
        module.add_macro(LOAD, MACRO_LOAD, DOC_LOAD);

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

    match &ctx.log_output {
        LogOutput::Stdout => {
            println!("{}", lv);
        }
        LogOutput::Log(log) => {
            let log = log.clone();
            tokio::spawn(async move {
                log.debug(lv);
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
pub fn read(file_name: String) -> LResult {
    let mut file = match File::open(&file_name) {
        Ok(f) => f,
        Err(e) => return Err(lruntimeerror!(LOAD, format!("{}: {}", file_name, e))),
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

#[async_scheme_fn]
pub async fn get_env_var(env: &LEnv, var: String) -> LResult {
    match env::var(var) {
        Ok(o) => parse(&format!("\"{o}\""), &mut env.clone()).await,
        Err(e) => Err(LRuntimeError::new("", e.to_string())),
    }
}
