use crate::rae::context::actions_progress::Status;
use crate::rae::module::rae_exec::{
    CtxRaeExec, SYMBOL_EXEC_MODE, SYMBOL_RAE_MODE, SYMBOL_SIMU_MODE,
};
use ::macro_rules_attribute::macro_rules_attribute;
use log::{info, warn};
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LValue};
use ompas_utils::dyn_async;
use std::convert::TryInto;

pub const RAE_EXEC_COMMAND: &str = "rae-exec-command";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_OPEN_COM_PLATFORM: &str = "rae-open-com-platform";
pub const RAE_START_PLATFORM: &str = "rae-start-platform";
pub const RAE_INSTANCE: &str = "instance";

#[macro_rules_attribute(dyn_async!)]
pub async fn exec_command<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
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
                                return Ok(false.into());
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
                None => {
                    Ok(LValue::Nil)
                    //Internal simulator behaviour
                }
            }
        }
        SYMBOL_SIMU_MODE => Ok(LValue::Nil),
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn launch_platform<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a mut CtxRaeExec,
) -> Result<LValue, LError> {
    if let Some(platform) = &mut ctx.platform_interface {
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
pub async fn start_platform<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a mut CtxRaeExec,
) -> Result<LValue, LError> {
    if let Some(platform) = &mut ctx.platform_interface {
        platform.start_platform(args).await
    } else {
        Ok("No platform defined".into())
    }
}
#[macro_rules_attribute(dyn_async!)]
pub async fn open_com<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a mut CtxRaeExec,
) -> Result<LValue, LError> {
    if let Some(platform) = &mut ctx.platform_interface {
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
pub async fn cancel_command<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
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
pub async fn instance<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRaeExec,
) -> Result<LValue, LError> {
    if let Some(platform) = &ctx.platform_interface {
        platform.instance(args).await
    } else {
        Err(SpecialError(
            RAE_INSTANCE,
            "instance not yet implemented in internal rae functionning".into(),
        ))
    }
}
