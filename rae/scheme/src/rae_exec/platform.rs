use crate::rae_exec::*;
use log::{error, info};
use ompas_rae_core::ctx_planning::{CtxPlanning, MOD_PLANNING};
use ompas_rae_core::error::RaeExecError;
use ompas_rae_structs::contexts::ctx_mode::{CtxMode, RAEMode, CTX_MODE};
use ompas_rae_structs::contexts::ctx_task::{CtxTask, CTX_TASK};
use ompas_rae_structs::state::task_status::TaskStatus;
use sompas_core::modules::list::append;
use sompas_core::{eval, parse};
use sompas_modules::utils::contains;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lswitch::InterruptionReceiver;
use sompas_structs::lvalue::LValue;
use std::convert::TryInto;

#[scheme_fn]
pub fn is_platform_defined(env: &LEnv) -> bool {
    env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)
        .unwrap()
        .platform_interface
        .is_some()
}

#[async_scheme_fn]
pub async fn exec_command(env: &LEnv, args: &[LValue]) -> LAsyncHandler {
    let env = env.clone();
    let args = args.to_vec();

    let (tx, mut int_rx) = new_interruption_handler();

    let f = (Box::pin(async move {
        let args = args.as_slice();

        let parent_task = env.get_context::<CtxTask>(CTX_TASK)?.parent_id;
        let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
        let (action_id, mut rx) = ctx.agenda.add_action(args.into(), parent_task).await;
        let debug: LValue = args.into();
        info!("exec command {}: {}", action_id, debug);
        let eval_model = |int: Option<InterruptionReceiver>| async {
            let string = format!("((get-action-model '{}) {})", args[0], {
                let mut str = "".to_string();
                for e in &args[1..] {
                    str.push_str(format!("'{}", e).as_str())
                }
                str
            });
            //println!("in eval model, string: {}", string);
            let mut env = env.clone();
            eval(&parse(&string, &mut env).await?, &mut env, int).await
        };

        let mode = env.get_context::<CtxMode>(CTX_MODE)?.mode;

        match mode {
            RAEMode::Exec => {
                match &ctx.platform_interface {
                    Some(platform) => {
                        platform.exec_command(args, action_id).await?;

                        //println!("await on action (id={})", action_id);
                        info!("waiting on action {}", action_id);

                        //let mut action_status: TaskStatus = ctx.agenda.get_status(&action_id).await;

                        let f = async {
                            while rx.changed().await.is_ok() {
                                //println!("waiting on status:");
                                let action_status = *rx.borrow();
                                match action_status {
                                    TaskStatus::Pending => {
                                        //println!("not triggered");
                                    }
                                    TaskStatus::Running => {
                                        //println!("running");
                                    }
                                    TaskStatus::Failure => {
                                        error!("Command {} is a failure.", action_id);
                                        ctx.agenda.set_end_time(&action_id).await;
                                        return Ok(RaeExecError::ActionFailure.into());
                                    }
                                    TaskStatus::Done => {
                                        info!("Command {} is a success.", action_id);
                                        ctx.agenda.set_end_time(&action_id).await;
                                        return Ok(true.into());
                                    }
                                }
                            }
                            Err(LRuntimeError::new(
                                RAE_EXEC_COMMAND,
                                "error on action status channel",
                            ))
                        };
                        tokio::select! {
                            _ = int_rx.recv() => {
                                platform.cancel_command(&[action_id.into()]).await
                            }
                            r = f => {
                                r
                            }
                        }
                    }
                    None => {
                        let r = eval_model(Some(int_rx)).await;
                        algorithms::set_success_for_task(&env, &[action_id.into()]).await?;
                        r
                    }
                }
            }
            RAEMode::Simu => eval_model(None).await,
        }
    }) as FutureResult)
        .shared();

    tokio::spawn(f.clone());

    LAsyncHandler::new(f, tx)
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
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let mode = env.get_context::<CtxMode>(CTX_MODE)?.mode;
    match mode {
        RAEMode::Exec => {
            if let Some(platform) = &ctx.platform_interface {
                platform.cancel_command(args).await
            } else {
                Ok("cannot cancel command in internal platform (action is instantaneous)".into())
            }
        }
        RAEMode::Simu => Ok("No cancellation of action in simulation mode".into()),
    }
}

enum InstanceMode {
    All,
    Instances,
    Check,
}

pub fn look_in_state(
    env: &LEnv,
    args: &[LValue],
    facts: im::HashMap<LValue, LValue>,
) -> Result<LValue, LRuntimeError> {
    let ctx_planning = env.get_context::<CtxPlanning>(MOD_PLANNING)?;
    let mut values: Vec<LValue> = vec![];

    let mode = match args.len() {
        0 => InstanceMode::All,
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
        InstanceMode::All => LValue::Nil,
    };

    match &mode {
        InstanceMode::All => {
            let mut map: HashMap<LValue, LValue> = Default::default();
            let th = ctx_planning.domain.get_type_hierarchy();
            let types = th.get_types();
            for t in &types {
                let key = list![RAE_INSTANCE.into(), t.into()];
                map.insert(key.clone(), facts.get(&key).unwrap_or(&LValue::Nil).clone());
            }
            Ok(map.into())
        }
        mode => {
            let mut types: Vec<String> = ctx_planning.domain.get_childs(&t.to_string());
            types.push(t.to_string());
            for t in &types {
                let key = vec![RAE_INSTANCE.into(), LValue::from(t)].into();
                values.push(facts.get(&key).unwrap_or(&LValue::Nil).clone());
            }

            let instances: LValue = append(env, values.as_slice())?;
            match &mode {
                InstanceMode::Instances => Ok(instances),
                InstanceMode::Check => contains(env, &[instances, args[0].clone()]),
                InstanceMode::All => unreachable!(),
            }
        }
    }
}

#[async_scheme_fn]
pub async fn instance(env: &LEnv, args: &[LValue]) -> LResult {
    match &env
        .get_context::<CtxRaeExec>(MOD_RAE_EXEC)?
        .platform_interface
    {
        None => {
            let state: im::HashMap<LValue, LValue> = get_facts(env, &[]).await?.try_into()?;
            look_in_state(env, args, state)
        }
        Some(platform) => platform.instance(args).await,
    }
}
