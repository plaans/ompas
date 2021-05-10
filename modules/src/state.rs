#![allow(dead_code)]
#![allow(unused_imports)]
use crate::doc::{Documentation, LHelp};
use im::HashMap;
use ompas_lisp::structs::{GetModule, LValue, Module};

/*
LANGUAGE
 */

const MOD_STATE: &str = "mod-state";
const DOC_MOD_STATE: &str = "Documentation for mod-state.";
const DOC_MOD_STATE_VERBOSE: &str = "functions:";

pub struct CtxState {
    states: Vec<LState>,
}

impl GetModule for CtxState {
    #[allow(clippy::let_and_return, unused_mut)]
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            label: MOD_STATE,
        };

        // Add LValue here

        module
    }
}

impl Documentation for CtxState {
    fn documentation() -> Vec<LHelp> {
        vec![LHelp::new(MOD_STATE, DOC_MOD_STATE, None)]
    }
}

pub struct LState {
    inner: im::HashMap<LValue, LValue>,
}
