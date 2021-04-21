//use crate::lisp_root::lisp_struct::*;

// ///Module that handles Input/Output treatment.

/*
LANGUAGE
 */

/*pub struct IO {}

const PRINT: &str = "print";
const READ: &str = "read";
const WRITE: &str = "write";
const LOAD: &str = "load";*/

/*pub fn print(values: &[LValue], ctx: &dyn NativeContext ) -> Result<LValue, LError> {
    println!("moudle IO: print");
    Ok(LValue::None)

}

pub fn read(values: &[LValue], ctx: &dyn NativeContext) -> Result<LValue, LError> {
    println!("moudle IO: read");
    Ok(LValue::None)
}

pub fn write(values: &[LValue], ctx: &dyn NativeContext) -> Result<LValue, LError> {
    println!("moudle IO: write");
    Ok(LValue::None)
}

pub fn load(values: &[LValue], ctx: &dyn NativeContext) -> Result<LValue, LError> {
    println!("moudle IO: load");
    Ok(LValue::None)
}

impl AsModule for IO {
    fn get_module() -> Module {
        let mut prelude_unmut = vec![];
        let mut prelude_mut = vec![];
        prelude_unmut.push((PRINT.into(), LNativeLambda {
            fun: Rc::new(print)
        }));
        prelude_unmut.push((READ.into(), LNativeLambda {
            fun: Rc::new(read)
        }));
        prelude_unmut.push((WRITE.into(), LNativeLambda {
            fun: Rc::new(write)
        }));
        prelude_unmut.push((LOAD.into(), LNativeLambda {
            fun: Rc::new(load)
        }));

        Module {
            ctx: Box::new(()),
            prelude_mut,
            prelude_unmut
        }
    }
}*/
