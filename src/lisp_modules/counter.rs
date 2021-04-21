/*
LANGUAGE
*/
use crate::lisp_root::lisp_struct::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use crate::lisp_root::lisp_struct::*;
use crate::lisp_root::RefLEnv;

pub const TYPE_COUNTER: &str = "counter";
pub const SET_COUNTER: &str = "set-counter";
pub const GET_COUNTER: &str = "get-counter";
pub const NEW_COUNTER: &str = "new_counter";
pub const DECREMENT_COUNTER: &str = "decrement-counter";
pub const INCREMENT_COUNTER: &str = "increment-counter";

#[derive(Default)]
pub struct CtxCounter {
    pub(crate) counters: Vec<Counter>,
}

impl CtxCounter {
    pub fn new_counter(&mut self) -> usize {
        self.counters.push(Counter::default());
        self.counters.len() - 1
    }
}

#[derive(Default, Clone)]
pub struct Counter {
    val: u32,
}

pub fn get_counter(args: &[LValue], _: &mut RefLEnv, ctx: &CtxCounter) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(LNumber::USize(u)) => match ctx.counters.get(*u) {
            None => Err(SpecialError("index out of reach".to_string())),
            Some(c) => Ok(LValue::Number(LNumber::Int(c.val as i64))),
        },
        lv => Err(WrongType(
            lv.clone(),
            lv.into(),
            NameTypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn decrement_counter(
    args: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxCounter,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(LNumber::USize(u)) => match ctx.counters.get_mut(*u) {
            None => Err(SpecialError("index out of reach".to_string())),
            Some(c) => {
                if c.val > 0 {
                    c.val -=1;
                }
                Ok(LValue::None)
            }
        }
        lv => Err(WrongType(
            lv.clone(),
            lv.into(),
            NameTypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn increment_counter(
    args: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxCounter,
) -> Result<LValue, LError> {

    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    match &args[0] {
        LValue::Number(LNumber::USize(u)) => match ctx.counters.get_mut(*u) {
            None => Err(SpecialError("index out of reach".to_string())),
            Some(c) => {
                c.val += 1;
                Ok(LValue::None)
            }
        }
        lv => Err(WrongType(
            lv.clone(),
            lv.into(),
            NameTypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn set_counter(
    args: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxCounter,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    match &args[0] {
        LValue::Number(LNumber::USize(u)) => match ctx.counters.get_mut(*u) {
            None => Err(SpecialError("index out of reach".to_string())),
            Some(c) => {
                c.val = args[1].as_int()? as u32;
                Ok(LValue::None)
            }
        },
        lv => Err(WrongType(
            lv.clone(),
            lv.into(),
            NameTypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn new_counter(
    _: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxCounter,
) -> Result<LValue, LError> {
    Ok(LValue::Number(LNumber::USize(ctx.new_counter())))
}

impl AsModule for CtxCounter {
    fn get_module() -> Module {
        let mut prelude = vec![];
        prelude.push((
            GET_COUNTER.into(),
            LValue::NativeLambda(LNativeLambda::new(Box::new(get_counter))),
        ));
        prelude.push((
            SET_COUNTER.into(),
            LValue::NativeMutLambda(LNativeMutLambda::new(Box::new(set_counter))),
        ));

        prelude.push((
            NEW_COUNTER.into(),
            LValue::NativeMutLambda(LNativeMutLambda::new(Box::new(new_counter))),
        ));

        prelude.push((
            INCREMENT_COUNTER.into(),
            LValue::NativeMutLambda(LNativeMutLambda::new(Box::new(increment_counter))),
        ));

        prelude.push((
            DECREMENT_COUNTER.into(),
            LValue::NativeMutLambda(LNativeMutLambda::new(Box::new(decrement_counter))),
        ));

        Module {
            ctx: Box::new(Self::default()),
            prelude,
        }
    }
}
