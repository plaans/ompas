//! Example Module
//! Gives an example on how to code a rust library to be bound in Scheme.
use crate::core::structs::documentation::{Documentation, LHelp};
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use crate::core::structs::lerror::LResult;
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::{IntoModule, Module};
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::core::structs::typelvalue::TypeLValue;
use crate::modules::deprecated::counter::language::*;
use anyhow::bail;
use std::convert::TryFrom;
use std::sync::Arc;

/*
LANGUAGE
*/
pub mod language {

    pub const MOD_COUNTER: &str = "counter";
    pub const DOC_MOD_COUNTER: &str = "documentation of the module counter";

    pub const TYPE_COUNTER: &str = "counter";
    pub const SET_COUNTER: &str = "set-counter";
    pub const GET_COUNTER: &str = "get-counter";
    pub const NEW_COUNTER: &str = "new_counter";
    pub const DECREMENT_COUNTER: &str = "decrement-counter";
    pub const INCREMENT_COUNTER: &str = "increment-counter";

    pub const DOC_GET_COUNTER: &str = "Get value of a counter.";
    pub const DOC_SET_COUNTER: &str = "Set value of a counter.";
    pub const DOC_SET_COUNTER_VERBOSE: &str = "Takes 2 arguments: the counter and the new value. Value must be a Natural.\n\
                                        Example: (set-counter cnt 10) #sets the value of the counter cnt to one.";
    pub const DOC_NEW_COUNTER: &str = "Create a new counter.";
    pub const DOC_NEW_COUNTER_VERBOSE: &str = "Takes no arguments. Return the id of the counter.\n\
                                        Example: (define cnt (new-counter))";
    pub const DOC_INCREMENT_COUNTER: &str = "Increment counter's value by one.";
    pub const DOC_DECREMENT_COUNTER: &str = "Decrement counter's value by one until it reaches 0.";
}

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

pub fn get_counter(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 1 {
        bail!(WrongNumberOfArgument(
            GET_COUNTER,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxCounter>(MOD_COUNTER)?;

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get(*u) {
            None => bail!(SpecialError(GET_COUNTER, "index out of reach".to_string())),
            Some(c) => Ok(LValue::Number(LNumber::Int(c.val as i64))),
        },
        lv => bail!(WrongType(
            GET_COUNTER,
            lv.clone(),
            lv.into(),
            TypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn decrement_counter(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 1 {
        return bail!(WrongNumberOfArgument(
            DECREMENT_COUNTER,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_mut_context::<CtxCounter>(MOD_COUNTER)?;

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get_mut(*u) {
            None => bail!(SpecialError(
                DECREMENT_COUNTER,
                "index out of reach".to_string(),
            )),
            Some(c) => {
                if c.val > 0 {
                    c.val -= 1;
                }
                Ok(LValue::Nil)
            }
        },
        lv => bail!(WrongType(
            DECREMENT_COUNTER,
            lv.clone(),
            lv.into(),
            TypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn increment_counter(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 1 {
        bail!(WrongNumberOfArgument(
            INCREMENT_COUNTER,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_mut_context::<CtxCounter>(MOD_COUNTER)?;

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get_mut(*u) {
            None => bail!(SpecialError(
                INCREMENT_COUNTER,
                "index out of reach".to_string(),
            )),
            Some(c) => {
                c.val += 1;
                Ok(LValue::Nil)
            }
        },
        lv => bail!(WrongType(
            INCREMENT_COUNTER,
            lv.clone(),
            lv.into(),
            TypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn set_counter(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 2 {
        return bail!(WrongNumberOfArgument(
            SET_COUNTER,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    let ctx = env.get_mut_context::<CtxCounter>(MOD_COUNTER)?;

    match &args[0] {
        LValue::Number(LNumber::Usize(u)) => match ctx.counters.get_mut(*u) {
            None => bail!(SpecialError(SET_COUNTER, "index out of reach".to_string())),
            Some(c) => {
                c.val = i64::try_from(&args[1])? as u32;
                Ok(LValue::Nil)
            }
        },
        lv => bail!(WrongType(
            SET_COUNTER,
            lv.clone(),
            lv.into(),
            TypeLValue::Other(TYPE_COUNTER.to_string()),
        )),
    }
}

pub fn new_counter(_: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_mut_context::<CtxCounter>(MOD_COUNTER)?;
    Ok(LValue::Number(LNumber::Usize(ctx.new_counter())))
}

impl IntoModule for CtxCounter {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_COUNTER.into(),
        };

        module.add_fn_prelude(GET_COUNTER, get_counter);
        module.add_fn_prelude(SET_COUNTER, set_counter);
        module.add_fn_prelude(NEW_COUNTER, new_counter);
        module.add_fn_prelude(INCREMENT_COUNTER, increment_counter);
        module.add_fn_prelude(DECREMENT_COUNTER, decrement_counter);

        module
    }

    fn documentation(self) -> Documentation {
        vec![
            LHelp::new(MOD_COUNTER, DOC_MOD_COUNTER),
            LHelp::new(GET_COUNTER, DOC_GET_COUNTER),
            LHelp::new_verbose(SET_COUNTER, DOC_SET_COUNTER, DOC_SET_COUNTER_VERBOSE),
            LHelp::new_verbose(NEW_COUNTER, DOC_NEW_COUNTER, DOC_NEW_COUNTER_VERBOSE),
            LHelp::new(INCREMENT_COUNTER, DOC_INCREMENT_COUNTER),
            LHelp::new(DECREMENT_COUNTER, DOC_DECREMENT_COUNTER),
        ]
        .into()
    }

    fn pure_fonctions(self) -> PureFonctionCollection {
        Default::default()
    }
}
