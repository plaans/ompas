///Module that handles Input/Output treatment.

/*
LANGUAGE
 */

pub struct IO {}

const PRINT: &str = "print";
const READ: &str = "read";
const WRITE: &str = "write";
const LOAD: &str = "load";

/*
pub fn print(values: &[LValue], ctx: () ) -> Result<LValue, LError> {
    unimplemented!()
}

pub fn read(values: &[LValue], ctx: ()) -> Result<LValue, LError> {
    unimplemented!()
}

pub fn write(values: &[LValue], ctx: ()) -> Result<LValue, LError> {
    unimplemented!()
}

pub fn load(values: &[LValue], ctx: ()) -> Result<LValue, LError> {
    unimplemented!()
}

pub fn create_module() -> Module {
    let mut prelude = vec![];
    prelude.push((PRINT.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(print)))));
    prelude.push((READ.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(read)))));
    prelude.push((WRITE.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(write)))));
    prelude.push((LOAD.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(load)))));

    Module {
        prelude
    }
}

impl AsModule for IO {
    fn get_module() -> Module {
        let mut prelude = vec![];
        prelude.push((PRINT.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(print)))));
        prelude.push((READ.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(read)))));
        prelude.push((WRITE.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(write)))));
        prelude.push((LOAD.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(load)))));

        Module {
            prelude
        }
    }
}*/
