use crate::rae::context::RAEEnv;
use crate::rae::module::mod_rae_exec::{Job, JobType};
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::{GetModule, LError, LValue, LValueS, Module};
use ompas_modules::doc::{Documentation, LHelp};
use std::sync::Arc;
use tokio::sync::mpsc::Sender;

#[derive(Default)]
pub struct CtxRaeMonitor {
    pub sender_to_rae: Option<Sender<Job>>,
    pub env: RAEEnv,
}

pub const RAE_TRIGGER_EVENT: &str = "rae-trigger-event";
pub const RAE_TRIGGER_TASK: &str = "rae-trigger-task";

pub const DOC_RAE_TRIGGER_EVENT: &str = "todo!";
pub const DOC_RAE_TRIGGER_TASK: &str = "todo!";

impl GetModule for CtxRaeMonitor {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: "",
        };

        module.add_fn_prelude(RAE_TRIGGER_EVENT, trigger_event);
        module.add_fn_prelude(RAE_TRIGGER_TASK, trigger_task);

        module
    }
}

impl Documentation for CtxRaeMonitor {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(RAE_TRIGGER_TASK, DOC_RAE_TRIGGER_TASK, None),
            LHelp::new(RAE_TRIGGER_EVENT, DOC_RAE_TRIGGER_EVENT, None),
        ]
    }
}

//Add an event to the stream of RAE
//access asynchronously to the stream
pub fn trigger_event(_: &[LValue], _env: &LEnv, _: &CtxRaeMonitor) -> Result<LValue, LError> {
    Ok(LValue::String(
        "trigger event not yet implemented".to_string(),
    ))
}

pub fn trigger_task(args: &[LValue], _env: &LEnv, ctx: &CtxRaeMonitor) -> Result<LValue, LError> {
    let job = Job::new(args.into(), JobType::Task);
    let sender = ctx.sender_to_rae.clone().unwrap();
    tokio::spawn(async move {
        sender.send(job).await.expect("could not task job to rae");
    });
    Ok(Nil)
}
