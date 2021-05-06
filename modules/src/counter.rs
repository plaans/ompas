use crate::doc::{Documentation, LHelp};
use ompas_lisp::core::*;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;

/*
LANGUAGE
*/

const MOD_COUNTER: &str = "mod-counter";
const DOC_MOD_COUNTER: &str = "documentation of the module counter";

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

pub fn get_counter(args: &[LValue], _: &RefLEnv, ctx: &CtxCounter) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get(*u) {
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
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get_mut(*u) {
            None => Err(SpecialError("index out of reach".to_string())),
            Some(c) => {
                if c.val > 0 {
                    c.val -= 1;
                }
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

pub fn increment_counter(
    args: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxCounter,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get_mut(*u) {
            None => Err(SpecialError("index out of reach".to_string())),
            Some(c) => {
                c.val += 1;
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

pub fn set_counter(
    args: &[LValue],
    _: &mut RefLEnv,
    ctx: &mut CtxCounter,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get_mut(*u) {
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

pub fn new_counter(_: &[LValue], _: &mut RefLEnv, ctx: &mut CtxCounter) -> Result<LValue, LError> {
    Ok(LValue::Number(LNumber::Usize(ctx.new_counter())))
}

impl AsModule for CtxCounter {
    fn as_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            label: MOD_COUNTER,
        };

        module.add_fn_prelude(GET_COUNTER, Box::new(get_counter));
        module.add_mut_fn_prelude(SET_COUNTER, Box::new(set_counter));
        module.add_mut_fn_prelude(NEW_COUNTER, Box::new(new_counter));
        module.add_mut_fn_prelude(INCREMENT_COUNTER, Box::new(increment_counter));
        module.add_mut_fn_prelude(DECREMENT_COUNTER, Box::new(decrement_counter));

        module
    }
}

/*
DOCUMENTATION
 */

const DOC_GET_COUNTER: &str = "";
const DOC_SET_COUNTER: &str = "";
const DOC_NEW_COUNTER: &str = "";
const DOC_INCREMENT_COUNTER: &str = "";
const DOC_DECREMENT_COUNTER: &str = "";

impl Documentation for CtxCounter {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_COUNTER, DOC_MOD_COUNTER, None),
            LHelp::new(GET_COUNTER, DOC_GET_COUNTER, None),
            LHelp::new(SET_COUNTER, DOC_SET_COUNTER, None),
            LHelp::new(NEW_COUNTER, DOC_NEW_COUNTER, None),
            LHelp::new(INCREMENT_COUNTER, DOC_INCREMENT_COUNTER, None),
            LHelp::new(DECREMENT_COUNTER, DOC_DECREMENT_COUNTER, None),
        ]
    }
}
