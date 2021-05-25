#![allow(dead_code)]
#![allow(unused_imports)]
use im::ordmap::DiffItem::Update;
use im::HashMap;
use ompas_lisp::core::LEnv;
use ompas_lisp::functions::map;
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};
use serde_json::ser::State;
use std::borrow::Borrow;
use std::convert::TryFrom;
use std::mem;
use tokio::sync::mpsc::Sender;

/*
LANGUAGE
 */

const MOD_STATE: &str = "mod-state";

//functions
const SET_STATE: &str = "set-state";
const GET_STATE: &str = "get-state";
const UPDATE_STATE: &str = "update-state";

const KEY_DYNAMIC: &str = "dynamic";
const KEY_STATIC: &str = "static";

//Documentation
const DOC_MOD_STATE: &str = "Module to handle state. Contains a map with all the facts.";
const DOC_MOD_STATE_VERBOSE: &str = "functions:\n\
                                     \t- set-state\n\
                                     \t- get-state\n\
                                     \t- update-state";

const DOC_SET_STATE: &str = "Set a map as a the new state";
const DOC_SET_VERBOSE: &str = "Takes 2 arguments:\n\
                                \t- the type of state to set: {static, dynamic}\n\
                                \t- the map that will overwrite the current state";
const DOC_GET_STATE: &str = "Return the current state.";
const DOC_GET_STATE_VERBOSE: &str = "Takes an optional argument: {static, dynamic}";
const DOC_UPDATE_STATE: &str = "Update the current state with facts of a map";
const DOC_UPDATE_VERBOSE: &str = "Takes 2 arguments:\n\
                                \t- the type of state to set: {static, dynamic}\n\
                                \t- the map that will overwrite the current state";

#[derive(Default)]
pub struct CtxState {
    static_state: LState,
    dynamic_state: LState,
    sender_stdout: Option<Sender<String>>,
}

enum StateType {
    Static,
    Dynamic,
}

impl CtxState {
    pub fn set_sender_stdout(&mut self, sender: Sender<String>) {
        self.sender_stdout = Some(sender)
    }

    pub fn get_sender_stdout(&mut self) -> &Option<Sender<String>> {
        &self.sender_stdout
    }

    fn set_state(&mut self, s: LState, st: &StateType) {
        match st {
            StateType::Static => self.static_state = s,
            StateType::Dynamic => self.dynamic_state = s,
        };
    }

    fn update_state(&mut self, s: LState, st: &StateType) {
        let _old = mem::replace(
            match st {
                StateType::Static => &mut self.static_state,
                StateType::Dynamic => &mut self.dynamic_state,
            },
            s,
        );
    }

    fn get_state(&self, st: Option<StateType>) -> LState {
        match st {
            None => self.static_state.union(&self.dynamic_state),
            Some(st) => match st {
                StateType::Static => self.static_state.clone(),
                StateType::Dynamic => self.dynamic_state.clone(),
            },
        }
    }
}

impl GetModule for CtxState {
    #[allow(clippy::let_and_return, unused_mut)]
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_STATE,
        };

        module.add_fn_prelude(GET_STATE, Box::new(get_state));
        module.add_mut_fn_prelude(SET_STATE, Box::new(set_state));
        module.add_mut_fn_prelude(UPDATE_STATE, Box::new(update_state));

        // Add LValue here

        module
    }
}

impl Documentation for CtxState {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_STATE, DOC_MOD_STATE, None),
            LHelp::new(GET_STATE, DOC_GET_STATE, None),
            LHelp::new(SET_STATE, DOC_SET_STATE, None),
            LHelp::new(UPDATE_STATE, DOC_UPDATE_STATE, None),
        ]
    }
}

#[derive(Clone, Default)]
pub struct LState {
    inner: im::HashMap<LValue, LValue>,
}

impl LState {
    pub fn union(&self, other: &Self) -> Self {
        self.inner.clone().union(other.inner.clone()).into()
    }

    pub fn append(&mut self, other: &LState) {
        let _ = self.inner.clone().union(other.inner.clone());
    }
}

impl From<&LState> for im::HashMap<LValue, LValue> {
    fn from(ls: &LState) -> Self {
        ls.inner.clone()
    }
}
impl From<LState> for im::HashMap<LValue, LValue> {
    fn from(ls: LState) -> Self {
        (&ls).into()
    }
}
impl From<&im::HashMap<LValue, LValue>> for LState {
    fn from(m: &HashMap<LValue, LValue>) -> Self {
        Self { inner: m.clone() }
    }
}
impl From<im::HashMap<LValue, LValue>> for LState {
    fn from(m: HashMap<LValue, LValue>) -> Self {
        (&m).into()
    }
}

impl From<&LState> for LValue {
    fn from(ls: &LState) -> Self {
        ls.inner.clone().into()
    }
}

impl From<LState> for LValue {
    fn from(ls: LState) -> Self {
        (&ls).into()
    }
}

fn set_state(args: &[LValue], _: &mut LEnv, ctx: &mut CtxState) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    let first_arg = &args[0];
    let state_type = match String::try_from(first_arg) {
        Ok(s) => match s.as_str() {
            KEY_DYNAMIC => StateType::Dynamic,
            KEY_STATIC => StateType::Static,
            _ => {
                return Err(SpecialError(format!(
                    "Expected keywords {} or {}",
                    KEY_STATIC, KEY_DYNAMIC
                )))
            }
        },
        Err(_) => {
            return Err(WrongType(
                first_arg.clone(),
                first_arg.into(),
                NameTypeLValue::String,
            ))
        }
    };

    match &args[1] {
        LValue::Map(m) => {
            ctx.set_state(m.into(), &state_type);
            Ok(LValue::Nil)
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}

fn get_state(args: &[LValue], _: &LEnv, ctx: &CtxState) -> Result<LValue, LError> {
    match args.len() {
        0 => Ok(LValue::Map(ctx.get_state(None).into())),
        1 => match &args[0] {
            LValue::Symbol(s) => match s.as_str() {
                KEY_STATIC => Ok(ctx.get_state(Some(StateType::Static)).into()),
                KEY_DYNAMIC => Ok(ctx.get_state(Some(StateType::Dynamic)).into()),
                _ => Err(SpecialError(format!(
                    "Expected keywords {} or {}",
                    KEY_STATIC, KEY_DYNAMIC
                ))),
            },
            lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol)),
        },
        _ => Err(WrongNumberOfArgument(args.into(), args.len(), 0..1)),
    }
}

///Update the last state with the new facts of the map.
fn update_state(args: &[LValue], _: &mut LEnv, ctx: &mut CtxState) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    let first_arg = &args[0];
    let state_type = match String::try_from(first_arg) {
        Ok(s) => match s.as_str() {
            KEY_DYNAMIC => StateType::Dynamic,
            KEY_STATIC => StateType::Static,
            _ => {
                return Err(SpecialError(format!(
                    "Expected keywords {} or {}",
                    KEY_STATIC, KEY_DYNAMIC
                )))
            }
        },
        Err(_) => {
            return Err(WrongType(
                first_arg.clone(),
                first_arg.into(),
                NameTypeLValue::String,
            ))
        }
    };

    match &args[1] {
        LValue::Map(m) => {
            ctx.update_state(m.into(), &state_type);
            Ok(LValue::Nil)
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}
