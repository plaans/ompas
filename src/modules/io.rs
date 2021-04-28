//use crate::lisp_root::lisp_struct::*;

// ///Module that handles Input/Output treatment.

/*
LANGUAGE
 */

const PRINT: &str = "print";
const READ: &str = "read";
const WRITE: &str = "write";
//const LOAD: &str = "load";

use crate::core::r#struct::{AsModule, LError, LFn, LMutFn, LValue, Module, NameTypeLValue};
use crate::core::RefLEnv;
use crate::core::r#struct::LError::*;
use std::sync::mpsc::Sender;
use std::fs::File;
use std::io::{Read, Write, Error, Stdout, Stderr};
use std::io;
use std::borrow::BorrowMut;

/// Handles the channel to communicate with the Lisp Interpreter
#[derive(Debug)]
pub struct CtxIO {
    sender: Option<Sender<String>>,
}

impl Default for CtxIO {
    fn default() -> Self {
       Self {
           sender: None,
       }
    }
}

impl CtxIO {
    pub fn add_sender(&mut self, sender: Sender<String>) {
        self.sender = Some(sender);
    }
}

impl From<std::io::Error> for LError {
    fn from(e: std::io::Error) -> Self {
        SpecialError(e.to_string())
    }
}

pub fn print(args: &[LValue], _: &RefLEnv, ctx: &CtxIO) -> Result<LValue, LError> {
    let mut stdout = io::stdout();
    stdout.write_all(b"module IO: print\n");
    let lv : LValue = args.into();
    stdout.write_all(format!("{}\n", lv).as_bytes());

    Ok(LValue::None)
}

pub fn read(args: &[LValue], _: &RefLEnv, ctx: &CtxIO) -> Result<LValue, LError> {
    let mut stdout = io::stdout();
    stdout.write_all(b"module IO: read\n");
    if args.len() !=1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }
    let file_name = match &args[0] {
        LValue::Symbol(s) => s.to_string(),
        lv=> return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
    };

    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    stdout.write_all(format!("contents: {}\n", contents).as_bytes());
    ctx.sender.as_ref().expect("missing a channel").send(contents).expect("couldn't send string via channel");

    Ok(LValue::None)
}


/// Write an lvalue to a given file
///
/// example: **(write file lvalue)**
pub fn write(_: &[LValue], _: &RefLEnv, _: &CtxIO) -> Result<LValue, LError> {
    println!("moudle IO: write");
    Ok(LValue::None)
}

/*pub fn load(args: &[LValue], _: &RefLEnv, _: & CtxIO ) -> Result<LValue, LError> {
    println!("moudle IO: load");
    Ok(LValue::None)
}*/

impl AsModule for CtxIO {
    fn get_module() -> Module {
        let mut prelude = vec![];
        prelude.push((
            PRINT.into(),
            LValue::Fn(LFn::new(Box::new(print), PRINT.to_string())),
        ));
        prelude.push((
            READ.into(),
            LValue::Fn(LFn::new(Box::new(read), READ.to_string())),
        ));
        prelude.push((
            WRITE.into(),
            LValue::Fn(LFn::new(Box::new(write), WRITE.to_string())),
        ));
        //prelude.push((LOAD.into(),LValue::Fn(LFn::new(Box::new(print), LOAD.to_string()))));

        Module {
            ctx: Box::new(CtxIO::default()),
            prelude,
        }
    }
}
