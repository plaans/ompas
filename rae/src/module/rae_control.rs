use crate::module::rae_exec::{Job, JobType};
use crate::module::{CtxRae, MOD_RAE};
use crate::supervisor::options::{
    Planner, RAEOptions, SelectMode, ARIES, ARIES_OPT, GREEDY, HEURISTIC, LEARNING, PLANNING,
    RAE_PLAN, UPOM,
};
use crate::supervisor::rae_run;
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument};
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_utils::dyn_async;
use std::convert::TryInto;

pub const RAE_TRIGGER_EVENT: &str = "trigger-event";
pub const RAE_TRIGGER_TASK: &str = "trigger-task";
pub const RAE_LAUNCH: &str = "launch";
pub const RAE_CONFIGURE_PLATFORM: &str = "configure-platform";
pub const RAE_CONFIGURE_SELECT: &str = "configure-select";

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
#[macro_rules_attribute(dyn_async!)]
pub async fn configure_select<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_CONFIGURE_SELECT,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let m: String = (&args[0]).try_into()?;
    let select_mode = match m.as_str() {
        GREEDY => SelectMode::Greedy,
        PLANNING | ARIES => SelectMode::Planning(Planner::Aries, false),
        ARIES_OPT => SelectMode::Planning(Planner::Aries, true),
        UPOM => SelectMode::Planning(Planner::UPOM, false),
        RAE_PLAN => SelectMode::Planning(Planner::RAEPlan(Default::default()), false),
        HEURISTIC => SelectMode::Heuristic,
        LEARNING => SelectMode::Learning,
        _ => {
            return Err(SpecialError(
                RAE_CONFIGURE_SELECT,
                format!(
                    "Select mode is either {}, {}, {} or {}.",
                    GREEDY, PLANNING, HEURISTIC, LEARNING
                ),
            ))
        }
    };

    ctx.set_select_mode(select_mode).await;

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
