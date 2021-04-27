//use crate::lisp_root::lisp_struct::*;

// ///Module that handles Input/Output treatment.

/*
LANGUAGE
 */

const PRINT: &str = "print";
const READ: &str = "read";
const WRITE: &str = "write";
//const LOAD: &str = "load";

use crate::core::r#struct::{AsModule, LError, LFn, LMutFn, LValue, Module};
use crate::core::RefLEnv;

#[derive(Default, Debug)]
pub struct CtxIO {}

pub fn print(_: &[LValue], _: &RefLEnv, _: &CtxIO) -> Result<LValue, LError> {
    println!("moudle IO: print");
    Ok(LValue::None)
}

pub fn read(_: &[LValue], _: &mut RefLEnv, _: &mut CtxIO) -> Result<LValue, LError> {
    println!("moudle IO: read");
    Ok(LValue::None)
}

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
            LValue::MutFn(LMutFn::new(Box::new(read), READ.to_string())),
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
