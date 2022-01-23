use crate::rae::context::mutex;
use crate::rae::context::rae_env::RAEEnv;
use crate::rae::context::ressource_access::wait_on;
use crate::rae::module::rae_exec::{Job, JobType};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::structs::documentation::{Documentation, LHelp};
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::module::{IntoModule, Module};
use ompas_lisp::core::structs::purefonction::PureFonctionCollection;
use ompas_utils::dyn_async;
use std::sync::Arc;
use tokio::sync::mpsc::Sender;

pub struct CtxRaeMonitor {
    pub sender_to_rae: Option<Sender<Job>>,
    pub env: RAEEnv,
}

pub const RAE_TRIGGER_EVENT: &str = "trigger-event";
pub const RAE_TRIGGER_TASK: &str = "trigger-task";
pub const MOD_RAE_MONITOR: &str = "rae-monitor";
pub const RAE_GET_MUTEXES: &str = "get-mutexes";
pub const RAE_GET_WAIT_ONS: &str = "get-wait-ons";

pub const DOC_MOD_RAE_MONITOR: &str = "Exposes functions to send requests to RAE.";
pub const DOC_MOD_RAE_MONITOR_VERBOSE: &str = "functions:\n\
-trigger-event\n\
-trigger-task\n\
-get-mutexes\n\
-get-wait-on";

pub const DOC_RAE_TRIGGER_EVENT: &str = "Sends to RAE an event to handle";
pub const DOC_RAE_TRIGGER_EVENT_VERBOSE: &str = "";
pub const DOC_RAE_TRIGGER_TASK: &str = "Sends to RAE a task to execute";
pub const DOC_RAE_TRIGGER_TASK_VERBOSE: &str = "Example: (rae-trigger-task t_dumber robot0)";

impl IntoModule for CtxRaeMonitor {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_RAE_MONITOR.to_string(),
        };

        module.add_fn_prelude(RAE_TRIGGER_EVENT, trigger_event);
        module.add_fn_prelude(RAE_TRIGGER_TASK, trigger_task);
        module.add_async_fn_prelude(RAE_GET_MUTEXES, get_mutexes);
        module.add_async_fn_prelude(RAE_GET_WAIT_ONS, get_wait_ons);

        module
    }

    fn documentation(&self) -> Documentation {
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
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

/// Add an event to the stream of RAE
/// access asynchronously to the stream
pub fn trigger_event(_: &[LValue], _: &LEnv) -> LResult {
    Ok(LValue::String(
        "trigger event not yet implemented".to_string(),
    ))
}

/// Sends via a channel a task to execute.
pub fn trigger_task(args: &[LValue], env: &LEnv) -> LResult {
    let env = env.clone();

    let ctx = env.get_context::<CtxRaeMonitor>(MOD_RAE_MONITOR)?;

    let job = Job::new(args.into(), JobType::Task);
    let sender = ctx.sender_to_rae.clone().unwrap();
    tokio::spawn(async move {
        sender.send(job).await.expect("could not task job to rae");
    });
    Ok(LValue::Nil)
}

#[macro_rules_attribute(dyn_async!)]
async fn get_mutexes<'a>(_: &'a [LValue], _: &'a LEnv) -> LResult {
    Ok(mutex::get_debug().await.into())
}

#[macro_rules_attribute(dyn_async!)]
async fn get_wait_ons<'a>(_: &'a [LValue], _: &'a LEnv) -> LResult {
    Ok(wait_on::get_debug().await.into())
}
