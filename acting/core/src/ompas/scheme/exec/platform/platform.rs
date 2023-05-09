use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::OMPASDomain;
use crate::ompas::manager::acting::{ActingManager, ActionId};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::scheme::exec::platform::exec_platform::ExecPlatform;
use crate::ompas::scheme::exec::platform::lisp_domain::LispDomain;
use crate::ompas::scheme::exec::platform::platform_config::PlatformConfig;
use crate::ompas::scheme::exec::platform::PlatformDescriptor;
use ompas_language::interface::{LOG_TOPIC_PLATFORM, PROCESS_TOPIC_PLATFORM};
use ompas_middleware::ProcessInterface;
use sompas_core::eval;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::{new_interruption_handler, InterruptionSender};
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

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
            Some(command) => command.get_model(&ModelKind::SimModel).unwrap().clone(),
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
                command.get_model(&ModelKind::PlantModel).unwrap().clone()
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
