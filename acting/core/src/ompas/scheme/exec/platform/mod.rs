use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::error::RaeExecError;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId, ActionId};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::scheme::exec::acting_context::ModActingContext;
use crate::ompas::scheme::exec::mode::{CtxMode, RAEMode};
use crate::ompas::scheme::exec::platform::exec_platform::ExecPlatform;
use crate::ompas::scheme::exec::platform::platform_config::PlatformConfig;
use crate::ompas::scheme::exec::ModExec;
use async_trait::async_trait;
use futures::FutureExt;
use lisp_domain::LispDomain;
use ompas_language::exec::acting_context::*;
use ompas_language::exec::mode::CTX_MODE;
use ompas_language::exec::platform::*;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PROCESS_TOPIC_PLATFORM};
use ompas_middleware::logger::LogClient;
use ompas_middleware::ProcessInterface;
use sompas_core::eval;
use sompas_macros::{async_scheme_fn, scheme_fn};
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::{new_interruption_handler, InterruptionSender};
use sompas_structs::lvalue::LValue;
use std::any::Any;
use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::{watch, RwLock};

pub mod exec_platform;
pub mod lisp_domain;
pub mod platform_config;
pub mod platform_declaration;

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
        let args = command_slice.iter().map(|lv| lv.as_cst()).collect();

        let pr: ProcessRef = env
            .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
            .process_ref
            .clone();

        let acting_manager = &env.get_context::<ModPlatform>(MOD_PLATFORM)?.acting_manager;

        let command_id: ActingProcessId = match &pr {
            ProcessRef::Id(id) => {
                acting_manager
                    .new_action(
                        Label::Action(acting_manager.get_number_subtask(*id).await),
                        id,
                        args,
                        debug,
                        ProcessOrigin::Execution,
                    )
                    .await
            }

            ProcessRef::Relative(id, labels) => match acting_manager.get_id(pr.clone()).await {
                Some(id) => id,
                None => {
                    acting_manager
                        .new_action(
                            *labels.last().unwrap(),
                            id,
                            args,
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
            LValue::from(command_slice)
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
                    .sim_command(env.clone(), command_slice)
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

/// Trait that a platform needs to implement to be able to be used as execution platform in RAE.
#[async_trait]
pub trait PlatformDescriptor: Any + Send + Sync {
    ///Launch the platform (such as the simulation in godot) and open communication
    async fn start(&self, config: PlatformConfig);

    ///Stops the platform.
    async fn stop(&self);

    ///Returns the domain of the platform
    async fn domain(&self) -> LispDomain;

    ///Returns a module loaded into the evaluation environment with other bindings
    async fn module(&self) -> Option<LModule>;

    ///Returns the server info in order to connect OMPAS to the platform using grpc services
    async fn socket(&self) -> SocketAddr;
}

#[derive(Default, Clone)]
pub struct Platform {
    ompas_domain: Arc<RwLock<OMPASDomain>>,
    acting_manager: ActingManager,
    exec: Option<ExecPlatform>,
    lisp_domain: LispDomain,
    interrupters: Arc<RwLock<HashMap<ActionId, InterruptionSender>>>,
}

impl Platform {
    pub fn new(
        ompas_domain: Arc<RwLock<OMPASDomain>>,
        acting_manager: ActingManager,
        exec: Option<ExecPlatform>,
        lisp_domain: LispDomain,
    ) -> Self {
        Self {
            ompas_domain,
            acting_manager,
            exec,
            lisp_domain,
            interrupters: Arc::new(Default::default()),
        }
    }

    pub async fn sim_command(&self, mut env: LEnv, command: &[LValue]) -> LResult {
        let label = command[0].to_string();
        let command = LValue::from(command);

        let model: LValue = match self.ompas_domain.read().await.commands.get(&label) {
            Some(command) => command.get_model(&ModelKind::SimModel).unwrap(),
            None => {
                return Err(Default::default());
            }
        };
        env.insert(label, model);
        eval(&command, &mut env, None).await
    }

    pub async fn exec_command_model(&self, mut env: LEnv, command: &[LValue], command_id: usize) {
        let (tx, rx) = new_interruption_handler();
        self.interrupters.write().await.insert(command_id, tx);

        let label = command[0].to_string();
        let command = LValue::from(command);
        let supervisor = self.acting_manager.clone();
        let interrupters = self.interrupters.clone();

        let model: LValue = match self.ompas_domain.read().await.commands.get(&label) {
            Some(command) => {
                supervisor
                    .set_status(&command_id, ProcessStatus::Accepted)
                    .await;
                command.get_model(&ModelKind::PlantModel).unwrap()
            }
            None => {
                supervisor
                    .set_status(&command_id, ProcessStatus::Rejected)
                    .await;
                return;
            }
        };

        tokio::spawn(async move {
            let process: ProcessInterface = ProcessInterface::new(
                format!("PROCESS_COMMAND_SIM_{}", command_id),
                PROCESS_TOPIC_PLATFORM,
                LOG_TOPIC_PLATFORM,
            )
            .await;
            env.insert(label, model);
            match eval(&command, &mut env, Some(rx)).await {
                Err(err) => {
                    supervisor
                        .set_status(&command_id, ProcessStatus::Failure)
                        .await;
                    process
                        .log_error(format!("Eval error executing command {}: {}", command, err))
                        .await;
                    process.kill(PROCESS_TOPIC_PLATFORM).await;
                }
                Ok(LValue::Err(err)) => {
                    supervisor
                        .set_status(&command_id, ProcessStatus::Failure)
                        .await;
                    process
                        .log_error(format!(
                            "Execution of {}({}) returned an error: {}",
                            command, command_id, err
                        ))
                        .await;
                }
                Ok(_) => {
                    supervisor
                        .set_status(&command_id, ProcessStatus::Success)
                        .await
                }
            };
            interrupters.write().await.remove(&command_id);
        });
    }

    pub async fn exec_command_on_platform(
        &self,
        command: &[LValue],
        command_id: usize,
    ) -> Result<(), LRuntimeError> {
        if let Some(exec) = &self.exec {
            exec.exec_command(command, command_id).await;
            Ok(())
        } else {
            Err(LRuntimeError::new(
                "exec_command_on_platform",
                "No execution platform is defined.",
            ))
        }
    }

    pub async fn cancel_command(&self, command_id: usize) {
        match &self.exec {
            None => {
                let int: Option<InterruptionSender> =
                    self.interrupters.write().await.remove(&command_id);

                match int {
                    None => {}
                    Some(mut int) => int.interrupt().await,
                }
            }
            Some(p) => p.cancel_command(command_id).await,
        }
    }

    pub async fn start(&self, config: PlatformConfig) -> Result<String, LRuntimeError> {
        if let Some(exec) = &self.exec {
            exec.start(config).await;
            Ok("Platform successfully launched.".to_string())
        } else {
            Ok("No execution platform defined. Running in simulation.".to_string())
        }
    }

    pub async fn stop(&self) {
        if let Some(exec) = &self.exec {
            exec.stop().await;
        }
    }

    pub fn domain(&self) -> LispDomain {
        self.lisp_domain.clone()
    }

    pub async fn module(&self) -> Option<LModule> {
        match &self.exec {
            Some(p) => p.module().await,
            None => None,
        }
    }

    pub fn is_exec_defined(&self) -> bool {
        self.exec.is_some()
    }

    pub async fn try_set_config_platform(&self, config: PlatformConfig) -> Result<(), ()> {
        if let Some(exec) = &self.exec {
            *exec.config.write().await = config;
            Ok(())
        } else {
            Err(())
        }
    }

    pub async fn try_get_config_platform(&self) -> Result<PlatformConfig, ()> {
        match &self.exec {
            None => Err(()),
            Some(exec) => Ok(exec.config.read().await.clone()),
        }
    }
}
