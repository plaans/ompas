use ompas_lisp::structs::{LValue, LError};
use ompas_lisp::structs::LValue::Nil;
use crate::rae::context::{CtxRAE};
use ompas_lisp::core::LEnv;


//Others functions could be add to interogate and launch rae.

pub fn launch_rae(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(Nil)
}

//Add an event to the stream of RAE
//access asynchronously to the stream
pub fn add_event(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {

    Ok(Nil)
}
