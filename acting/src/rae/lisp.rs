use ompas_lisp::structs::{GetModule, Module, LValue, LError};
use ompas_modules::doc::{Documentation, LHelp};
use std::alloc::LayoutError;
use ompas_lisp::structs::LValue::Nil;
use crate::rae::progress::Stream;

pub struct CtxRAE {
    stream: Stream,
    log: String,
}

impl GetModule for CtxRAE {
    fn get_module(self) -> Module {
        todo!()
    }
}

impl Documentation for CtxRAE {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}

//Others functions could be add to interogate and launch rae.

pub fn launch_rae(args: &[LValue], _env: &Env, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(Nil)
}

//Add an event to the stream of RAE
//access asynchronously to the stream
pub fn add_event(args: &[LValue], _env: &Env, ctx: &CtxRAE) -> Result<LValue, LError> {

    Ok(Nil)
}
