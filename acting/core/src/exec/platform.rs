use crate::error::RaeExecError;
use crate::exec::context::ModContext;
use crate::exec::mode::{CtxMode, RAEMode};
use crate::exec::*;
use ompas_interface::platform::Platform;
use ompas_language::exec::context::MOD_CONTEXT;
use ompas_language::exec::mode::CTX_MODE;
use ompas_language::exec::platform::*;
use ompas_middleware::logger::LogClient;
use ompas_structs::state::action_status::ActionStatus;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;

pub struct ModPlatform {
    platform: Platform,
    supervisor: Supervisor,
    pub(crate) log: LogClient,
}

impl ModPlatform {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            platform: exec.platform.clone(),
            supervisor: exec.supervisor.clone(),
            log: exec.log.clone(),
        }
    }
}

impl From<ModPlatform> for LModule {
    fn from(m: ModPlatform) -> Self {
        let mut module = LModule::new(m, MOD_PLATFORM, DOC_MOD_PLATFORM);
        module.add_async_fn(EXEC_COMMAND, exec_command, DOC_EXEC_COMMAND, false);
        module.add_async_fn(CANCEL_COMMAND, cancel_command, DOC_CANCEL_COMMAND, false);
        module.add_fn(
            IS_PLATFORM_DEFINED,
            is_platform_defined,
            DOC_IS_PLATFORM_DEFINED,
            false,
        );
        module.add_async_fn(START_PLATFORM, start_platform, DOC_START_PLATFORM, false);
        module
    }
}

#[async_scheme_fn]
pub async fn exec_command(env: &LEnv, command: &[LValue]) -> LAsyncHandle {
    let env = env.clone();
    let command = command.to_vec();

    let (tx, mut int_rx) = new_interruption_handler();

    let f = (Box::pin(async move {
        let command = command.as_slice();

        let parent_task = env
            .get_context::<ModContext>(MOD_CONTEXT)?
            .get_task_id()
            .await;
        let mod_platform = env.get_context::<ModPlatform>(MOD_PLATFORM)?;
        let log = mod_platform.log.clone();
        let (command_id, mut rx) = mod_platform
            .supervisor
            .add_command(command.into(), parent_task)
            .await;
        let debug: LValue = command.into();
        log.info(format!("Exec command {command_id}: {debug}."))
            .await;

        let mode = env.get_context::<CtxMode>(CTX_MODE)?.mode;

        match mode {
            RAEMode::Exec => {
                match mod_platform.platform.is_exec_defined() {
                    true => {
                        mod_platform
                            .platform
                            .exec_command_on_platform(command, command_id)
                            .await?;
                    }
                    false => {
                        mod_platform
                            .platform
                            .exec_command_model(env.clone(), command, command_id)
                            .await;
                    }
                }

                log.info(format!("Waiting on command {command_id}.")).await;

                let f = async {
                    while rx.changed().await.is_ok() {
                        //println!("waiting on status:");
                        let action_status = *rx.borrow();
                        match action_status {
                            ActionStatus::Rejected => {
                                log.error(format!("Command {command_id} is a rejected."))
                                    .await;
                                mod_platform.supervisor.set_end_time(&command_id).await;
                                return Ok(RaeExecError::ActionFailure.into());
                            }
                            ActionStatus::Accepted => {}
                            ActionStatus::Pending => {
                                //println!("not triggered");
                            }
                            ActionStatus::Running(_) => {
                                //println!("running");
                            }
                            ActionStatus::Failure => {
                                log.error(format!("Command {command_id} is a failure."))
                                    .await;
                                mod_platform.supervisor.set_end_time(&command_id).await;
                                return Ok(RaeExecError::ActionFailure.into());
                            }
                            ActionStatus::Success => {
                                log.info(format!("Command {command_id} is a success."))
                                    .await;
                                mod_platform.supervisor.set_end_time(&command_id).await;
                                return Ok(true.into());
                            }
                            ActionStatus::Cancelled(_) => {
                                log.info(format!("Command {command_id} has been cancelled."))
                                    .await;
                                mod_platform.supervisor.set_end_time(&command_id).await;
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
                    .sim_command(env.clone(), command)
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
