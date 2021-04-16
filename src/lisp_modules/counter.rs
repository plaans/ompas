/*
LANGUAGE
*/
pub const COUNTER: &str= "counter";
pub const SET_COUNTER: &str = "set-counter";
pub const GET_COUNTER: &str = "get-counter";

/*
pub struct Counter {
    v : u32
}

pub fn get_counter(args: &[LValue], ctx: &Counter) -> Result<LValue, LError>{
    Ok(LValue::Number(LNumber::Int(*ctx.v as i64)))
}

pub fn set_counter(args: &[LValue], ctx: &mut Counter) -> Result<LValue, LError>{
    if args.len() != 1 {
        Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }else {
        match args.first().unwrap() {
            LValue::Number(LNumber::Int(i)) => {
                ctx.v = *i as u32;
                Ok(LValue::None)
            }
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Number))
        }
    }
}

impl AsModule for Counter {
    fn get_module() -> Module {
        let mut prelude= vec![];
        prelude.push((GET_COUNTER.into(), LNativeLambdaEnum::LambdaUnMut(LNativeLambdaPrime::new(Box::new(get_counter)))));
        prelude.push((SET_COUNTER.into(), LNativeLambdaEnum::LambdaMut(LNativeMutLambdaPrime::new(Box::new(set_counter)))));

        Module {
            prelude
        }
    }
}*/
