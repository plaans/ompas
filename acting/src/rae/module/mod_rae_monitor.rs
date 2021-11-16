use crate::rae::context::rae_env::RAEEnv;
use crate::rae::module::mod_rae_exec::{Job, JobType};
use ompas_lisp::core::LEnv;
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::{GetModule, LError, LValue, LValueS, Module};
use std::sync::Arc;
use tokio::sync::mpsc::Sender;

pub struct CtxRaeMonitor {
    pub sender_to_rae: Option<Sender<Job>>,
    pub env: RAEEnv,
}

pub const RAE_TRIGGER_EVENT: &str = "rae-trigger-event";
pub const RAE_TRIGGER_TASK: &str = "rae-trigger-task";
pub const MOD_RAE_CTX_MONITOR: &str = "rae-context-monitor";

pub const DOC_RAE_TRIGGER_EVENT: &str = "Sends to RAE an event to handle";
pub const DOC_RAE_TRIGGER_EVENT_VERBOSE: &str = "";
pub const DOC_RAE_TRIGGER_TASK: &str = "Sends to RAE a task to execute";
pub const DOC_RAE_TRIGGER_TASK_VERBOSE: &str = "Example: (rae-trigger-task t_dumber robot0)";

impl GetModule for CtxRaeMonitor {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_RAE_CTX_MONITOR.to_string(),
        };

        module.add_fn_prelude(RAE_TRIGGER_EVENT, trigger_event);
        module.add_fn_prelude(RAE_TRIGGER_TASK, trigger_task);

        module
    }
}

impl Documentation for CtxRaeMonitor {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(
                RAE_TRIGGER_TASK,
                DOC_RAE_TRIGGER_TASK,
                DOC_RAE_TRIGGER_EVENT_VERBOSE,
            ),
            LHelp::new_verbose(
                RAE_TRIGGER_EVENT,
                DOC_RAE_TRIGGER_EVENT,
                DOC_RAE_TRIGGER_TASK_VERBOSE,
            ),
        ]
    }
}

/// Add an event to the stream of RAE
/// access asynchronously to the stream
pub fn trigger_event(_: &[LValue], _env: &LEnv, _: &CtxRaeMonitor) -> Result<LValue, LError> {
    Ok(LValue::String(
        "trigger event not yet implemented".to_string(),
    ))
}

/// Sends via a channel a task to execute.
pub fn trigger_task(args: &[LValue], _env: &LEnv, ctx: &CtxRaeMonitor) -> Result<LValue, LError> {
    let job = Job::new(args.into(), JobType::Task);
    let sender = ctx.sender_to_rae.clone().unwrap();
    tokio::spawn(async move {
        sender.send(job).await.expect("could not task job to rae");
    });
    Ok(Nil)
}
