use crate::error::RaeExecError;
use crate::exec::acting_context::ModActingContext;
use crate::exec::mode::{CtxMode, RAEMode};
use crate::exec::*;
use ompas_interface::platform::Platform;
use ompas_language::exec::acting_context::*;
use ompas_language::exec::mode::CTX_MODE;
use ompas_language::exec::platform::*;
use ompas_middleware::logger::LogClient;
use ompas_structs::acting_manager::action_status::ProcessStatus;
use ompas_structs::acting_manager::process::process_ref::ProcessRef;
use ompas_structs::acting_manager::ActingProcessId;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use tokio::sync::watch;

pub struct ModPlatform {
    acting_manager: ActingManager,
    platform: Platform,
    pub(crate) log: LogClient,
}

impl ModPlatform {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            acting_manager: exec.acting_manager.clone(),
            platform: exec.platform.clone(),
            log: exec.log.clone(),
        }
    }
}

impl From<ModPlatform> for LModule {
    fn from(m: ModPlatform) -> Self {
        let mut module = LModule::new(m, MOD_PLATFORM, DOC_MOD_PLATFORM);
        module.add_async_fn(_EXEC_COMMAND, exec_command, DOC__EXEC_COMMAND, false);
        module.add_async_fn(CANCEL_COMMAND, cancel_command, DOC_CANCEL_COMMAND, false);
        module.add_fn(
            IS_PLATFORM_DEFINED,
            is_platform_defined,
            DOC_IS_PLATFORM_DEFINED,
            false,
        );
        module.add_async_fn(START_PLATFORM, start_platform, DOC_START_PLATFORM, false);
        module.add_lambda(EXEC_COMMAND, LAMBDA_EXEC_COMMAND, DOC_EXEC_COMMAND);
        module.add_lambda(CTX_ACQUIRE, LAMBDA_CTX_ACQUIRE, DOC_CTX_ACQUIRE);
        module.add_lambda(
            CTX_EXEC_COMMAND,
            LAMBDA_CTX_EXEC_COMMAND,
            DOC_CTX_EXEC_COMMAND,
        );
        module.add_lambda(CTX_ARBITRARY, LAMBDA_CTX_ARBITRARY, DOC_CTX_ARBITRARY);
        module.add_lambda(CTX_EXEC_TASK, LAMBDA_CTX_EXEC_TASK, DOC_CTX_EXEC_TASK);
        module
    }
}

#[async_scheme_fn]
pub async fn exec_command(env: &LEnv, command: &[LValue]) -> LAsyncHandle {
    let env = env.clone();
    let command_slice = command.to_vec();
    let debug = LValue::from(command).to_string();

    let (tx, mut int_rx) = new_interruption_handler();

    let f = (Box::pin(async move {
        let command_slice = command_slice.as_slice();

        let pr: ProcessRef = env
            .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
            .process_ref
            .clone();

        let acting_manager = &env.get_context::<ModPlatform>(MOD_PLATFORM)?.acting_manager;

        let command_id: ActingProcessId = match &pr {
            ProcessRef::Id(id) => {
                acting_manager
                    .new_command(
                        Label::Action(acting_manager.get_number_subtask(*id).await),
                        id,
                        debug,
                        ProcessOrigin::Execution,
                    )
                    .await
            }

            ProcessRef::Relative(id, labels) => match acting_manager.get_id(pr.clone()).await {
                Some(id) => id,
                None => {
                    acting_manager
                        .new_command(
                            labels.last().unwrap().clone(),
                            id,
                            debug,
                            ProcessOrigin::Execution,
                        )
                        .await
                }
            },
        };

        let mut rx: watch::Receiver<ProcessStatus> = acting_manager.subscribe(&command_id).await;

        let mod_platform = env.get_context::<ModPlatform>(MOD_PLATFORM)?;
        let log = mod_platform.log.clone();
        log.info(format!(
            "Exec command {command_id}: {}.",
            LValue::from(command_slice.clone())
        ))
        .await;

        let mode = env.get_context::<CtxMode>(CTX_MODE)?.mode;

        match mode {
            RAEMode::Exec => {
                match mod_platform.platform.is_exec_defined() {
                    true => {
                        mod_platform
                            .platform
                            .exec_command_on_platform(command_slice, command_id)
                            .await?;
                    }
                    false => {
                        mod_platform
                            .platform
                            .exec_command_model(env.clone(), command_slice, command_id)
                            .await;
                    }
                }

                log.info(format!("Waiting on command {command_id}.")).await;

                let f = async {
                    while rx.changed().await.is_ok() {
                        //println!("waiting on status:");
                        let action_status = *rx.borrow();
                        match action_status {
                            ProcessStatus::Rejected => {
                                log.error(format!("Command {command_id} is a rejected."))
                                    .await;
                                mod_platform
                                    .acting_manager
                                    .set_end(&command_id, None, action_status)
                                    .await;
                                return Ok(RaeExecError::ActionFailure.into());
                            }
                            ProcessStatus::Accepted => {}
                            ProcessStatus::Pending => {
                                //println!("not triggered");
                            }
                            ProcessStatus::Running(_) => {
                                //println!("running");
                            }
                            ProcessStatus::Failure => {
                                log.error(format!("Command {command_id} is a failure."))
                                    .await;
                                mod_platform
                                    .acting_manager
                                    .set_end(&command_id, None, action_status)
                                    .await;
                                return Ok(RaeExecError::ActionFailure.into());
                            }
                            ProcessStatus::Success => {
                                log.info(format!("Command {command_id} is a success."))
                                    .await;
                                mod_platform
                                    .acting_manager
                                    .set_end(&command_id, None, action_status)
                                    .await;
                                return Ok(true.into());
                            }
                            ProcessStatus::Cancelled(_) => {
                                log.info(format!("Command {command_id} has been cancelled."))
                                    .await;
                                mod_platform
                                    .acting_manager
                                    .set_end(&command_id, None, action_status)
                                    .await;
                                return Ok(true.into());
                            }
                        }
                    }
                    Err(LRuntimeError::new(
                        EXEC_COMMAND,
                        "error on action status channel",
                    ))
                };
                tokio::select! {
                    _ = int_rx.recv() => {
                        mod_platform.platform.cancel_command(command_id).await;
                        Ok(LValue::Err(Default::default()))
                    }
                    r = f => {
                        r
                    }
                }
            }
            RAEMode::Simu => {
                mod_platform
                    .platform
                    .sim_command(env.clone(), &command_slice)
                    .await
            }
        }
    }) as FutureResult)
        .shared();

    tokio::spawn(f.clone());

    LAsyncHandle::new(f, tx)
}

#[async_scheme_fn]
pub async fn cancel_command(env: &LEnv, command_id: usize) -> LResult {
    let mod_platform = env.get_context::<ModPlatform>(MOD_PLATFORM)?;
    let mode = env.get_context::<CtxMode>(CTX_MODE)?.mode;
    match mode {
        RAEMode::Exec => {
            mod_platform.platform.cancel_command(command_id).await;
            todo!()
        }
        RAEMode::Simu => Ok("No cancellation of action in simulation mode".into()),
    }
}

#[scheme_fn]
pub fn is_platform_defined(env: &LEnv) -> bool {
    env.get_context::<ModPlatform>(MOD_PLATFORM)
        .unwrap()
        .platform
        .is_exec_defined()
}

#[async_scheme_fn]
pub async fn start_platform(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModPlatform>(MOD_PLATFORM).unwrap();

    ctx.platform.start(Default::default()).await
}

/*enum InstanceMode {
    All,
    Instances,
    Check,
}*/

/*
pub fn look_in_state(
    env: &LEnv,
    args: &[LValue],
    facts: im::HashMap<LValue, LValue>,
) -> Result<LValue, LRuntimeError> {
    let ctx_planning = env.get_context::<CtxPlanning>(CTX_PLANNING)?;
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
}*/
