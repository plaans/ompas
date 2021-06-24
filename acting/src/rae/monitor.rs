use tokio::sync::mpsc::Sender;
use crate::rae::job::*;
use ompas_lisp::structs::{GetModule, Module, LValue, LError, LValueS};
use ompas_lisp::core::LEnv;
use crate::rae::lisp::CtxRAE;
use ompas_lisp::structs::LValue::Nil;
use crate::rae::context::RAEEnv;
use std::sync::Arc;

pub struct CtxRAEMonitor {
    sender_to_rae: Sender<Job>,
    env: RAEEnv
}

pub const RAE_TRIGGER_EVENT: &str = "rae-trigger-event";
pub const RAE_TRIGGER_TASK: &str = "rae-trigger-task";

impl GetModule for CtxRAEMonitor {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: ""
        };

        module.add_fn_prelude(RAE_TRIGGER_EVENT, trigger_event);
        module.add_fn_prelude(RAE_TRIGGER_TASK, trigger_task);

        module

    }
}

//Add an event to the stream of RAE
//access asynchronously to the stream
pub fn trigger_event(_: &[LValue], _env: &LEnv, _: &CtxRAEMonitor) -> Result<LValue, LError> {
    Ok(LValue::String("trigger event not yet implemented".to_string()))
}

pub fn trigger_task(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    let job = Job::new(args.into(), JobType::Task);
    let mut sender = ctx.stream.get_sender();
    tokio::spawn(async move {
        sender.send(job).await.unwrap();
    });
    Ok(Nil)
}