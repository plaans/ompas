use crate::core::LEnv;
use crate::modules::doc::{Documentation, LHelp};
use crate::structs::{GetModule, LError, LValue, Module};
use std::sync::Arc;

/*
LANGUAGE
 */

const MOD_STRING: &str = "string";
const CONCATENATE: &str = "concatenate";
const DOC_CONCATENATE: &str = "todo!";

#[derive(Default)]
pub struct CtxString {}

impl GetModule for CtxString {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(()),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_STRING.to_string(),
        };

        module.add_fn_prelude(CONCATENATE, concatenate);
        module
    }
}

impl Documentation for CtxString {
    fn documentation() -> Vec<LHelp> {
        vec![LHelp::new(CONCATENATE, DOC_CONCATENATE)]
    }
}

//add test for concatenate
pub fn concatenate(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    let mut str = String::new();
    for e in args {
        str.push_str(e.to_string().as_str())
    }
    Ok(str.into())
}
