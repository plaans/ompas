use crate::rae_user::{CtxRae, MOD_RAE};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_rae_core::rae_run;
use ompas_rae_language::*;
use ompas_rae_structs::exec_context::job::{Job, JobType};
use ompas_rae_structs::exec_context::options::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LRuntimeError::{Anyhow, WrongNumberOfArgument};
use sompas_structs::lerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_utils::dyn_async;
use std::convert::TryInto;

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
            return Err(Anyhow(
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
lfn!{pub trigger_event(_, _){
    Ok(LValue::String(
        "trigger event not yet implemented".to_string(),
    ))
}

/// Sends via a channel a task to execute.
lfn!{pub trigger_task(args, env){
    let env = env.clone();

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let job = Job::new(args.into(), JobType::Task);
    let sender = ctx.sender_to_rae.clone().unwrap();
    tokio::spawn(async move {
        sender.send(job).await.expect("could not task job to rae");
    });
    Ok(LValue::Nil)
}
