use crate::doc::{Documentation, LHelp};
use ompas_lisp::core::*;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;
use std::fs::File;
use std::io::{Read, Write};
use std::sync::mpsc::Sender;
/*
LANGUAGE
 */

const MOD_IO: &str = "mod-io";
const DOC_MOD_IO: &str = "Module than handles input/output functions.";
const DOC_MOD_IO_VERBOSE: &str = "functions:\n\
                                    -print\n\
                                    -read\n\
                                    -write";

const PRINT: &str = "print";
const READ: &str = "read";
const WRITE: &str = "write";
//const LOAD: &str = "load";

/// Handles the channel to communicate with the Lisp Interpreter
#[derive(Debug)]
pub struct CtxIo {
    sender_li: Option<Sender<String>>,
    sender_stdout: Option<Sender<String>>,
}

impl Default for CtxIo {
    fn default() -> Self {
        Self {
            sender_li: None,
            sender_stdout: None,
        }
    }
}

impl CtxIo {
    pub fn add_sender_li(&mut self, sender: Sender<String>) {
        self.sender_li = Some(sender);
    }
    pub fn add_sender_stdout(&mut self, sender: Sender<String>) {
        self.sender_stdout = Some(sender);
    }
}

pub fn print(args: &[LValue], _: &RefLEnv, ctx: &CtxIo) -> Result<LValue, LError> {
    let lv: LValue = match args.len() {
        0 => LValue::None,
        1 => args[0].clone(),
        _ => args.into(),
    };
    match &ctx.sender_stdout {
        None => return Err(SpecialError("no channel for stdout".to_string())),
        Some(sender) => sender
            .send(format!("{}", lv))
            .expect("error on channel to stdout"),
    };

    Ok(LValue::None)
}

pub fn read(args: &[LValue], _: &RefLEnv, ctx: &CtxIo) -> Result<LValue, LError> {
    //let mut stdout = io::stdout();
    //stdout.write_all(b"module Io: read\n");
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    let file_name = match &args[0] {
        LValue::Symbol(s) => s.to_string(),
        lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    };

    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    //stdout.write_all(format!("contents: {}\n", contents).as_bytes());
    ctx.sender_li
        .as_ref()
        .expect("missing a channel")
        .send(contents)
        .expect("couldn't send string via channel");

    Ok(LValue::None)
}

/// Write an lvalue to a given file
///
/// # Example:
/// ```lisp
/// (write <file> <lvalue>)
pub fn write(args: &[LValue], _: &RefLEnv, _: &CtxIo) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    match &args[0] {
        LValue::Symbol(s) => {
            //got our file name
            let mut f = File::create(s.to_string())?;
            f.write_all(&args[1].to_string().as_bytes())?;
            Ok(LValue::None)
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
    }

    //println!("module Io: write");
}

/*pub fn load(args: &[LValue], _: &RefLEnv, _: & CtxIo ) -> Result<LValue, LError> {
    println!("moudle Io: load");
    Ok(LValue::None)
}*/

impl GetModule for CtxIo {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            label: MOD_IO,
        };

        module.add_fn_prelude(PRINT, Box::new(print));
        module.add_fn_prelude(READ, Box::new(read));
        module.add_fn_prelude(WRITE, Box::new(write));

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
            LHelp::new(MOD_IO, DOC_MOD_IO, Some(DOC_MOD_IO_VERBOSE)),
            LHelp::new(PRINT, DOC_PRINT, Some(DOC_PRINT_VERBOSE)),
            LHelp::new(READ, DOC_READ, Some(DOC_READ_VERBOSE)),
            LHelp::new(WRITE, DOC_WRITE, Some(DOC_WRITE_VERBOSE)),
        ]
    }
}

/// Contains the function to handle the REPL of the project.
/// The repl is based on the project rustyline.
///
/// It contains only one function (for the moment): run that takes two arguments.
pub mod repl {

    use crate::io::repl;
    use rustyline::error::ReadlineError;
    use rustyline::Editor;
    use std::sync::mpsc::{channel, Receiver, Sender};
    use std::thread;

    pub fn spawn_stdin(sender: Sender<String>) -> Option<Sender<String>> {
        let (sender_stdin, receiver_stdin): (Sender<String>, Receiver<String>) = channel();

        thread::Builder::new()
            .name("repl_stdin".to_string())
            .spawn(move || {
                repl::stdin(sender, receiver_stdin);
            })
            .expect("error spawning repl input");

        Some(sender_stdin)
    }

    pub fn spawn_stdout() -> Option<Sender<String>> {
        let (sender_stdout, receiver_stdout): (Sender<String>, Receiver<String>) = channel();

        thread::Builder::new()
            .name("repl_output".to_string())
            .spawn(move || {
                repl::output(receiver_stdout);
            })
            .expect("error spawning repl output");

        Some(sender_stdout)
    }

    /// Function to handle the repl.
    /// ### functioning:
    /// loop waiting for an object on *stdin*
    /// ### args
    /// - sender: channel object to send string to lisp interpreter.
    /// - receiver: channel object to receive ack from lisp interpreter after evaluation.
    /// Used for synchronization.
    fn stdin(sender: Sender<String>, receiver: Receiver<String>) {
        let mut rl = Editor::<()>::new();
        if rl.load_history("history.txt").is_err() {
            println!("No previous history.");
        }

        loop {
            let readline = rl.readline(">> ");

            match readline {
                Ok(string) => {
                    rl.add_history_entry(string.clone());
                    sender
                        .send(format!("repl:{}", string))
                        .expect("couldn't send lisp command");
                    let buffer = receiver.recv().expect("error receiving");
                    assert_eq!(
                        buffer, "ACK",
                        "should receive an ack from Lisp Intrepretor and nothing else"
                    );
                    //println!("repl ack: {}", buffer);
                }
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break;
                }
            }
        }
        sender
            .send("exit".to_string())
            .expect("couldn't send exit msg");
        rl.save_history("history.txt").unwrap();
    }

    pub const EXIT_CODE_STDOUT: &str = "EXIT";

    fn output(receiver: Receiver<String>) {
        loop {
            let str = receiver.recv().expect("error receiving stdout");
            if str == EXIT_CODE_STDOUT {
                break;
            }
            println!("{}", str);
        }
    }
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
