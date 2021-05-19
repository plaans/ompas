#![allow(dead_code)]
#![allow(unused_imports)]
use im::ordmap::DiffItem::Update;
use im::HashMap;
use ompas_lisp::core::RefLEnv;
use ompas_lisp::functions::map;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};
use std::sync::mpsc::Sender;

/*
LANGUAGE
 */

const MOD_STATE: &str = "mod-state";

//functions
const SET_STATE: &str = "set-state";
const GET_STATE: &str = "get-state";
const GET_LAST_STATE: &str = "get-last-state";
const UPDATE_STATE: &str = "update-state";

//Documentation
const DOC_MOD_STATE: &str = "Documentation for mod-state.";
const DOC_MOD_STATE_VERBOSE: &str = "functions:";

const DOC_SET_STATE: &str = "todo";
const DOC_GET_STATE: &str = "todo";
const DOC_GET_LAST_STATE: &str = "todo";
const DOC_UPDATE_STATE: &str = "todo";

#[derive(Default)]
pub struct CtxState {
    states: Vec<LState>,
    sender_stdout: Option<Sender<String>>,
}

impl CtxState {
    pub fn set_sender_stdout(&mut self, sender: Sender<String>) {
        self.sender_stdout = Some(sender)
    }

    pub fn get_sender_stdout(&mut self) -> &Option<Sender<String>> {
        &self.sender_stdout
    }

    fn add_state(&mut self, s: LState) {
        self.states.push(s)
    }

    fn get_last_state(&self) -> Option<&LState> {
        self.states.last()
    }

    fn get_state(&self, id: usize) -> Option<&LState> {
        self.states.get(id)
    }
}

impl GetModule for CtxState {
    #[allow(clippy::let_and_return, unused_mut)]
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            label: MOD_STATE,
        };

        module.add_fn_prelude(GET_STATE, Box::new(get_state));
        module.add_fn_prelude(GET_LAST_STATE, Box::new(get_last_state));
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
            LHelp::new(GET_LAST_STATE, DOC_GET_LAST_STATE, None),
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
    pub fn append(&self, other: LState) -> LState {
        LState {
            inner: self.inner.clone().union(other.inner),
        }
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

fn set_state(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxState) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Map(m) => {
            ctx.add_state(m.into());
            Ok(LValue::Nil)
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}

fn get_state(args: &[LValue], _: &RefLEnv, ctx: &CtxState) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => match ctx.get_state(n.into()) {
            None => Ok(LValue::Nil),
            Some(m) => Ok(LValue::Map(m.into())),
        },
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}

fn get_last_state(_: &[LValue], _: &RefLEnv, ctx: &CtxState) -> Result<LValue, LError> {
    match ctx.get_last_state() {
        None => Ok(LValue::Nil),
        Some(ls) => Ok(LValue::Map(ls.into())),
    }
}

///Update the last state with the new facts of the map.
fn update_state(args: &[LValue], _: &mut RefLEnv, ctx: &mut CtxState) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Map(m) => {
            match ctx.get_last_state() {
                None => ctx.add_state(m.into()),
                Some(ls) => {
                    let ls = ls.clone();
                    ctx.add_state(ls.append(m.into()))
                }
            };
            Ok(LValue::Nil)
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Map)),
    }
}
