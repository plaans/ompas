use ompas_lisp::core::*;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;
use std::fs::File;
use std::io::{Read, Write};
use std::sync::mpsc::Sender;
/*
LANGUAGE
 */

const PRINT: &str = "print";
const READ: &str = "read";
const WRITE: &str = "write";
//const LOAD: &str = "load";

/// Handles the channel to communicate with the Lisp Interpreter
#[derive(Debug)]
pub struct CtxIO {
    sender: Option<Sender<String>>,
}

impl Default for CtxIO {
    fn default() -> Self {
        Self { sender: None }
    }
}

impl CtxIO {
    pub fn add_sender(&mut self, sender: Sender<String>) {
        self.sender = Some(sender);
    }
}

pub fn print(args: &[LValue], _: &RefLEnv, _: &CtxIO) -> Result<LValue, LError> {
    let lv: LValue = args.into();
    println!("{}", lv);
    //let mut stdout = io::stdout();
    //stdout.write_all(b"module IO: print\n");
    //stdout.write_all(format!("{}\n", lv).as_bytes());

    Ok(LValue::None)
}

pub fn read(args: &[LValue], _: &RefLEnv, ctx: &CtxIO) -> Result<LValue, LError> {
    //let mut stdout = io::stdout();
    //stdout.write_all(b"module IO: read\n");
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
    ctx.sender
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
pub fn write(args: &[LValue], _: &RefLEnv, _: &CtxIO) -> Result<LValue, LError> {
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

    //println!("module IO: write");
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

pub mod repl {
    use rustyline::error::ReadlineError;
    use rustyline::Editor;
    use std::sync::mpsc::{Receiver, Sender};

    pub fn run(sender: Sender<String>, receiver: Receiver<String>) {
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
}

//TODO: finish writing tests for io
#[cfg(test)]
pub mod tests {
    #[test]
    pub fn test_read() {
        unimplemented!()
    }

    #[test]
    pub fn test_write() {
        unimplemented!()
    }
}
