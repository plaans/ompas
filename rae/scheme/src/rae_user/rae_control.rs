use crate::rae_user::{CtxRae, MOD_RAE};
use ompas_rae_core::rae_run;
use ompas_rae_language::*;
use ompas_rae_structs::job::{Job, JobType};
use ompas_rae_structs::options::*;
use ompas_rae_structs::options::{Planner, RAEOptions, SelectMode};
use sompas_macros::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
pub async fn rae_launch(env: &LEnv) -> &str {
    let ctx = env.get_context::<CtxRae>(MOD_RAE).unwrap();

    let options = ctx.get_options().await.clone();

    let context = ctx.own_rae_env().await;

    tokio::spawn(async move {
        rae_run(context, &options, "rae-log.txt".to_string()).await;
    });
    "rae launched succesfully"
}

#[async_scheme_fn]
pub async fn configure_platform(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_CONFIGURE_PLATFORM,
            args,
            1..usize::MAX,
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
#[async_scheme_fn]
pub async fn configure_select(env: &LEnv, m: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE).unwrap();

    let select_mode = match m.as_str() {
        GREEDY => SelectMode::Greedy,
        PLANNING | ARIES => SelectMode::Planning(Planner::Aries, false),
        ARIES_OPT => SelectMode::Planning(Planner::Aries, true),
        UPOM => SelectMode::Planning(Planner::UPOM, false),
        RAE_PLAN => SelectMode::Planning(Planner::RAEPlan(Default::default()), false),
        HEURISTIC => SelectMode::Heuristic,
        LEARNING => SelectMode::Learning,
        _ => {
            return Err(lruntimeerror!(
                RAE_CONFIGURE_SELECT,
                format!(
                    "Select mode is either {}, {}, {} or {}.",
                    GREEDY, PLANNING, HEURISTIC, LEARNING
                )
            ))
        }
    };

    ctx.set_select_mode(select_mode).await;
    Ok(())
}

/// Add an event to the stream of RAE
/// access asynchronously to the stream
#[scheme_fn]
pub fn trigger_event() -> &str {
    "trigger event not yet implemented"
}

/// Sends via a channel a task to execute.
#[scheme_fn]
pub fn trigger_task(env: &LEnv, args: &[LValue]) {
    let env = env.clone();

    let ctx = env.get_context::<CtxRae>(MOD_RAE).unwrap();

    let job = Job::new(args.into(), JobType::Task);
    let sender = ctx.sender_to_rae.clone().unwrap();
    tokio::spawn(async move {
        sender.send(job).await.expect("could not task job to rae");
    });
}
