use crate::module::rae_exec::error::RaeExecError;
use crate::module::rae_exec::*;
use ::macro_rules_attribute::macro_rules_attribute;
use log::{info, warn};
use ompas_lisp::core::root_module::list::cdr;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument};
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::{eval, parse};
use ompas_lisp::modules::utils::contains;
use ompas_utils::dyn_async;
use std::convert::TryInto;

pub const RAE_EXEC_COMMAND: &str = "rae-exec-command";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_OPEN_COM_PLATFORM: &str = "rae-open-com-platform";
pub const RAE_START_PLATFORM: &str = "rae-start-platform";
pub const RAE_IS_PLATFORM_DEFINED: &str = "rae-platform?";
pub const RAE_INSTANCE: &str = "instance";

pub fn is_platform_defined(_: &[LValue], env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    Ok(ctx.platform_interface.is_some().into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn exec_command<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    let eval_model = || async {
        let string = format!(
            "((get-action-model (quote {}) {})",
            args[0],
            cdr(args, env)?
        );
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
                    let command_id = ctx.actions_progress.get_new_id();
                    let debug: LValue = args.into();
                    info!("exec command {}: {}", command_id, debug);
                    platform.exec_command(args, command_id).await?;

                    //println!("await on action (id={})", action_id);

                    let mut receiver = ctx.actions_progress.declare_new_watcher(&command_id).await;
                    info!("waiting on action {}", command_id);

                    let mut action_status = ctx
                        .actions_progress
                        .status
                        .read()
                        .await
                        .get(&command_id)
                        .unwrap()
                        .clone();

                    loop {
                        //println!("waiting on status:");
                        match action_status {
                            Status::Pending => {
                                //println!("not triggered");
                            }
                            Status::Running => {
                                //println!("running");
                            }
                            Status::Failure => {
                                warn!("Command {} is a failure.", command_id);
                                return Ok(RaeExecError::ActionFailure.into());
                            }
                            Status::Done => {
                                info!("Command {} is a success.", command_id);
                                return Ok(true.into());
                            }
                        }
                        action_status = if let true = receiver.recv().await.unwrap() {
                            ctx.actions_progress
                                .status
                                .read()
                                .await
                                .get(&command_id)
                                .unwrap()
                                .clone()
                        } else {
                            unreachable!("the signal for an update should always be true")
                        }
                    }
                }
                None => eval_model().await,
            }
        }
        SYMBOL_SIMU_MODE => eval_model().await,
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn launch_platform<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if let Some(platform) = &ctx.platform_interface {
        match &ctx.actions_progress.sync.sender {
            None => Err(SpecialError(
                RAE_LAUNCH_PLATFORM,
                "sender to actions status watcher missing.".to_string(),
            )),
            Some(_) => platform.launch_platform(args).await,
        }
    } else {
        Ok("No platform defined".into())
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn start_platform<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if let Some(platform) = &ctx.platform_interface {
        platform.start_platform(args).await
    } else {
        Ok("No platform defined".into())
    }
}
#[macro_rules_attribute(dyn_async!)]
pub async fn open_com<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if let Some(platform) = &ctx.platform_interface {
        match &ctx.actions_progress.sync.sender {
            None => Err(SpecialError(
                RAE_OPEN_COM_PLATFORM,
                "sender to actions status watcher missing.".to_string(),
            )),
            Some(_) => platform.open_com(args).await,
        }
    } else {
        Ok("No platform defined".into())
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn cancel_command<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

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

#[macro_rules_attribute(dyn_async!)]
pub async fn instance<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    /*
    The previous lambda definition was as follow

    pub const LAMBDA_INSTANCE: &str = "(define instance
        (lambda args
            (if (rae-platform?)
                (enr (cons fn-instance args))
                (cond ((= (len args) 1)
                        (get (rae-get-facts) (list 'instance (car args))))
                      ((= (len args) 2)
                        (contains
                            (get (rae-get-facts) (list 'instance (cadr args)))
                            (car args))))))))";*/

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    if let Some(platform) = &ctx.platform_interface {
        platform.instance(args).await
    } else {
        let facts: im::HashMap<LValue, LValue> = get_facts(&[], env).await?.try_into()?;
        match args.len() {
            1 => {
                let key = vec![RAE_INSTANCE.into(), args[0].clone()].into();
                let value = facts.get(&key).unwrap_or(&LValue::Nil);
                Ok(value.clone())
            }
            2 => {
                let key = vec![RAE_INSTANCE.into(), args[1].clone()].into();
                let instances = facts.get(&key).unwrap_or(&LValue::Nil);
                contains(&[instances.clone(), args[0].clone()], env)
            }
            _ => Err(WrongNumberOfArgument(
                RAE_INSTANCE,
                args.into(),
                args.len(),
                1..2,
            )),
        }
    }
}
