use crate::model::acting_domain::model::ModelKind;
use crate::ompas::manager::acting::{ActingManager, ActionId};
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::platform::exec_platform::ExecPlatform;
use crate::ompas::manager::platform::platform_config::PlatformConfig;
use crate::ompas::manager::state::action_status::ProcessStatus;
use async_trait::async_trait;
use lisp_domain::LispDomain;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::ProcessInterface;
use sompas_core::eval;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::{new_interruption_handler, InterruptionSender};
use sompas_structs::lvalue::LValue;
use std::any::Any;
use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::sync::RwLock;

pub mod exec_platform;
pub mod lisp_domain;
pub mod platform_config;
pub mod platform_declaration;

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
pub struct PlatformManager {
    ompas_domain: DomainManager,
    acting_manager: ActingManager,
    exec: Option<ExecPlatform>,
    lisp_domain: LispDomain,
    interrupters: Arc<RwLock<HashMap<ActionId, InterruptionSender>>>,
}

impl PlatformManager {
    pub fn new(
        ompas_domain: DomainManager,
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

        let model: LValue = match self.ompas_domain.get_command(&label).await {
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

        let model: LValue = match self.ompas_domain.get_command(&label).await {
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
                PROCESS_TOPIC_OMPAS,
                LOG_TOPIC_OMPAS,
            )
            .await;
            env.insert(label, model);
            let result = eval(&command, &mut env, Some(rx)).await;
            match &result {
                Err(err) => {
                    supervisor
                        .set_status(&command_id, ProcessStatus::Failure)
                        .await;
                    process.log_error(format!(
                        "Runtime error executing command model of {}: {}",
                        command, err
                    ));
                    process.kill(PROCESS_TOPIC_OMPAS);
                }
                Ok(lv) => {
                    if let LValue::Err(_) = lv {
                        supervisor
                            .set_status(&command_id, ProcessStatus::Failure)
                            .await;
                        process.log_error(format!(
                            "Execution of {}({}) is a failure : {}",
                            command, command_id, lv,
                        ));
                    } else {
                        process.log_info(format!(
                            "Execution of {}({}) is a success: {}",
                            command, command_id, lv,
                        ));
                        supervisor
                            .set_status(&command_id, ProcessStatus::Success)
                            .await
                    }
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
                    Some(mut int) => int.interrupt(),
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
