use crate::rae_exec::*;
use log::{info, warn};
use ompas_rae_core::error::RaeExecError;
use ompas_rae_core::planning::{CtxPlanning, MOD_PLANNING};
use ompas_rae_structs::task_collection::TaskStatus;
use sompas_core::modules::list::append;
use sompas_core::{eval, parse};
use sompas_modules::utils::contains;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::convert::{TryFrom, TryInto};

#[scheme_fn]
pub fn is_platform_defined(env: &LEnv) -> bool {
    env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)
        .unwrap()
        .platform_interface
        .is_some()
}

#[async_scheme_fn]
pub async fn exec_command(env: &LEnv, args: &[LValue]) -> LResult {
    let parent_task: usize = env
        .get_ref_symbol(PARENT_TASK)
        .map(|n| LNumber::try_from(n).unwrap().into())
        .unwrap();
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let (action_id, mut rx) = ctx.agenda.add_action(args.into(), parent_task).await;
    let debug: LValue = args.into();
    info!("exec command {}: {}", action_id, debug);

    let eval_model = || async {
        let string = format!("((get-action-model '{}) {})", args[0], {
            let mut str = "".to_string();
            for e in &args[1..] {
                str.push_str(format!("'{}", e).as_str())
            }
            str
        });
        //println!("in eval model, string: {}", string);
        let mut env = env.clone();
        eval(&parse(&string, &mut env).await?, &mut env).await
    };

    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            match &ctx.platform_interface {
                Some(platform) => {
                    platform.exec_command(args, action_id).await?;

                    //println!("await on action (id={})", action_id);
                    info!("waiting on action {}", action_id);

                    let mut action_status: TaskStatus = ctx.agenda.get_status(&action_id).await;

                    loop {
                        //println!("waiting on status:");
                        match action_status {
                            TaskStatus::Pending => {
                                //println!("not triggered");
                            }
                            TaskStatus::Running => {
                                //println!("running");
                            }
                            TaskStatus::Failure => {
                                warn!("Command {} is a failure.", action_id);
                                ctx.agenda.set_end_time(&action_id).await;
                                return Ok(RaeExecError::ActionFailure.into());
                            }
                            TaskStatus::Done => {
                                info!("Command {} is a success.", action_id);
                                ctx.agenda.set_end_time(&action_id).await;
                                return Ok(true.into());
                            }
                        }
                        action_status = rx.recv().await.unwrap();
                    }
                }
                None => {
                    let r = eval_model().await;
                    algorithms::set_success_for_task(env, &[action_id.into()]).await?;
                    r
                }
            }
        }
        SYMBOL_SIMU_MODE => eval_model().await,
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[async_scheme_fn]
pub async fn launch_platform(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC).unwrap();

    if let Some(platform) = &ctx.platform_interface {
        platform.launch_platform(args).await
    } else {
        Ok("No platform defined".into())
    }
}

#[async_scheme_fn]
pub async fn start_platform(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC).unwrap();

    if let Some(platform) = &ctx.platform_interface {
        platform.start_platform(args).await
    } else {
        Ok("No platform defined".into())
    }
}
#[async_scheme_fn]
pub async fn open_com(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC).unwrap();

    if let Some(platform) = &ctx.platform_interface {
        platform.open_com(args).await
    } else {
        Ok("No platform defined".into())
    }
}

#[async_scheme_fn]
pub async fn cancel_command(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC).unwrap();

    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            if let Some(platform) = &ctx.platform_interface {
                platform.cancel_command(args).await
            } else {
                Ok("cannot cancel command in internal platform (action is instantaneous)".into())
            }
        }
        SYMBOL_SIMU_MODE => Ok("No cancellation of action in simulation mode".into()),
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

enum InstanceMode {
    Instances,
    Check,
}

#[async_scheme_fn]
pub async fn instance(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC).unwrap();
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;

    let look_in_state = |facts: im::HashMap<LValue, LValue>| {
        let ctx_planning = env.get_context::<CtxPlanning>(MOD_PLANNING)?;
        let mut values: Vec<LValue> = vec![];

        let mode = match args.len() {
            1 => InstanceMode::Instances,
            2 => InstanceMode::Check,
            _ => {
                return Err(LRuntimeError::wrong_number_of_args(
                    RAE_INSTANCE,
                    args,
                    1..2,
                ))
            }
        };

        let t = match &mode {
            InstanceMode::Instances => args[0].clone(),
            InstanceMode::Check => args[1].clone(),
        };

        let mut types: Vec<String> = ctx_planning.domain.get_childs(&t.to_string());
        types.push(t.to_string());
        for t in &types {
            let key = vec![RAE_INSTANCE.into(), LValue::from(t)].into();
            values.push(facts.get(&key).unwrap_or(&LValue::Nil).clone());
        }

        let instances = append(env, values.as_slice())?;
        match &mode {
            InstanceMode::Instances => Ok(instances),
            InstanceMode::Check => contains(env, &[instances, args[0].clone()]),
        }
    };

    match mode.as_str() {
        SYMBOL_EXEC_MODE => {
            if let Some(platform) = &ctx.platform_interface {
                platform.instance(args).await
            } else {
                let state: im::HashMap<LValue, LValue> = get_facts(env, &[]).await?.try_into()?;
                look_in_state(state)
            }
        }
        SYMBOL_SIMU_MODE => {
            let state: im::HashMap<LValue, LValue> = env.get_symbol(STATE).unwrap().try_into()?;
            look_in_state(state)
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}
