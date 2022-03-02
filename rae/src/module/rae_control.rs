use crate::module::rae_exec::{Job, JobType};
use crate::module::{CtxRae, MOD_RAE};
use crate::supervisor::{rae_run, RAEOptions};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::WrongNumberOfArgument;
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_utils::dyn_async;

pub const RAE_TRIGGER_EVENT: &str = "trigger-event";
pub const RAE_TRIGGER_TASK: &str = "trigger-task";
pub const RAE_LAUNCH: &str = "launch";
pub const RAE_CONFIGURE_PLATFORM: &str = "configure-platform";

pub const DOC_RAE_TRIGGER_EVENT: &str = "Sends to RAE an event to handle";
pub const DOC_RAE_TRIGGER_EVENT_VERBOSE: &str = "";
pub const DOC_RAE_TRIGGER_TASK: &str = "Sends to RAE a task to execute";
pub const DOC_RAE_TRIGGER_TASK_VERBOSE: &str = "Example: (rae-trigger-task t_dumber robot0)";
pub const DOC_RAE_CONFIGURE_PLATFORM: &str =
    "Set the options of the platform when it will be runned";

/// Launch main loop of rae in an other asynchronous task.
#[macro_rules_attribute(dyn_async!)]
pub async fn rae_launch<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let options = ctx.get_options().await.clone();

    let context = ctx.own_rae_env().await;

    tokio::spawn(async move {
        rae_run(context, &options, "rae-log.txt".to_string()).await;
    });
    Ok(LValue::String("rae launched succesfully".to_string()))
}

#[macro_rules_attribute(dyn_async!)]
pub async fn configure_platform<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_CONFIGURE_PLATFORM,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }
    let mut string = String::default();
    for arg in args {
        string.push_str(format!("{} ", arg).as_str())
    }

    let rae_options = RAEOptions::new_with_platform_config(Default::default(), Some(string));
    ctx.set_options(rae_options).await;
    Ok(LValue::Nil)
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

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let job = Job::new(args.into(), JobType::Task);
    let sender = ctx.sender_to_rae.clone().unwrap();
    tokio::spawn(async move {
        sender.send(job).await.expect("could not task job to rae");
    });
    Ok(LValue::Nil)
}
