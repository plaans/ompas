use crate::error::RaeExecError;
use crate::exec::mode::{CtxMode, RAEMode};
use crate::exec::task::ModTask;
use crate::exec::*;
use ompas_middleware::logger::LogClient;
use ompas_rae_interface::platform::{Platform, PlatformDescriptor};
use ompas_rae_language::exec::mode::CTX_MODE;
use ompas_rae_language::exec::platform::*;
use ompas_rae_language::exec::task::MOD_TASK;
use ompas_rae_structs::agenda::Agenda;
use ompas_rae_structs::state::action_status::ActionStatus;
use sompas_core::parse;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lswitch::InterruptionReceiver;
use sompas_structs::lvalue::LValue;

pub struct ModPlatform {
    platform: Option<Platform>,
    agenda: Agenda,
    pub(crate) log: LogClient,
}

impl ModPlatform {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            platform: exec.platform.clone(),
            agenda: exec.agenda.clone(),
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
pub async fn exec_command(env: &LEnv, args: &[LValue]) -> LAsyncHandle {
    let env = env.clone();
    let args = args.to_vec();

    let (tx, mut int_rx) = new_interruption_handler();

    let f = (Box::pin(async move {
        let args = args.as_slice();

        let parent_task = env.get_context::<ModTask>(MOD_TASK)?.parent_id;
        let mod_platform = env.get_context::<ModPlatform>(MOD_PLATFORM)?;
        let log = mod_platform.log.clone();
        let (command_id, mut rx) = mod_platform
            .agenda
            .add_command(args.into(), parent_task)
            .await;
        let debug: LValue = args.into();
        log.info(format!("Exec command {command_id}: {debug}."))
            .await;
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
                match &mod_platform.platform {
                    Some(platform) => {
                        platform.exec_command(args, command_id).await;

                        //println!("await on action (id={})", action_id);
                        log.info(format!("Waiting on command {command_id}.")).await;
                        //let mut action_status: TaskStatus = ctx.agenda.get_status(&action_id).await;

                        let f = async {
                            while rx.changed().await.is_ok() {
                                //println!("waiting on status:");
                                let action_status = *rx.borrow();
                                match action_status {
                                    ActionStatus::Rejected => {
                                        log.error(format!("Command {command_id} is a rejected."))
                                            .await;
                                        mod_platform.agenda.set_end_time(&command_id).await;
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
                                        mod_platform.agenda.set_end_time(&command_id).await;
                                        return Ok(RaeExecError::ActionFailure.into());
                                    }
                                    ActionStatus::Success => {
                                        log.info(format!("Command {command_id} is a success."))
                                            .await;
                                        mod_platform.agenda.set_end_time(&command_id).await;
                                        return Ok(true.into());
                                    }
                                    ActionStatus::Cancelled(_) => {
                                        log.info(format!(
                                            "Command {command_id} has been cancelled."
                                        ))
                                        .await;
                                        mod_platform.agenda.set_end_time(&command_id).await;
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
                                platform.cancel_command(command_id).await;
                                Ok(LValue::Err(Default::default()))
                            }
                            r = f => {
                                r
                            }
                        }
                    }
                    None => {
                        let r = eval_model(Some(int_rx)).await;
                        set_success_for_task(&env, &[command_id.into()]).await?;
                        r
                    }
                }
            }
            RAEMode::Simu => eval_model(None).await,
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
            if let Some(platform) = &mod_platform.platform {
                platform.cancel_command(command_id).await;
                todo!()
            } else {
                Ok("cannot cancel command in internal platform (action is instantaneous)".into())
            }
        }
        RAEMode::Simu => Ok("No cancellation of action in simulation mode".into()),
    }
}

#[scheme_fn]
pub fn is_platform_defined(env: &LEnv) -> bool {
    env.get_context::<ModPlatform>(MOD_PLATFORM)
        .unwrap()
        .platform
        .is_some()
}

#[async_scheme_fn]
pub async fn start_platform(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModPlatform>(MOD_PLATFORM).unwrap();

    if let Some(platform) = &ctx.platform {
        platform.start(Default::default()).await;
        Ok(LValue::Nil)
    } else {
        Ok("No platform defined".into())
    }
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
