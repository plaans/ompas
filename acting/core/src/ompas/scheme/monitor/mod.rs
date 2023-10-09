//! Module containing the Scheme library to setup RAE environment
use control::*;
use debug_conversion::*;
use model::*;
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::Master;
use sompas_core::{eval_init, get_root_env};
use sompas_structs::lmodule::LModule;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
pub mod control;
pub mod debug_continuous_planning;
pub mod debug_conversion;
pub mod log;
pub mod model;
pub mod planning;
use crate::ompas::interface::rae_command::OMPASJob;
use crate::ompas::manager::acting::ActingManager;
use crate::ompas::manager::ompas::OMPASManager;
use crate::ompas::manager::platform::exec_platform::ExecPlatform;
use crate::ompas::manager::platform::platform_declaration::PlatformDeclaration;
use crate::ompas::manager::platform::PlatformManager;
use crate::ompas::scheme::exec::ModExec;
use crate::ompas::scheme::monitor::debug_continuous_planning::ModContinuousPlanning;
use crate::ompas::scheme::monitor::log::ModLog;
use crate::ompas::scheme::monitor::planning::ModPlanning;
use ompas_language::monitor::*;
use ompas_language::process::{LOG_TOPIC_OMPAS, OMPAS};
use sompas_modules::ModExtendedStd;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::{ImportType, LEnv};

//LANGUAGE

#[derive(Default)]
pub struct ModMonitor {
    pub(crate) options: OMPASManager,
    pub acting_manager: ActingManager,
    pub log: LogClient,
    pub task_stream: Arc<RwLock<Option<tokio::sync::mpsc::UnboundedSender<OMPASJob>>>>,
    pub(crate) platform: PlatformManager,
    pub(crate) empty_env: LEnv,
}

impl From<ModMonitor> for LModule {
    fn from(m: ModMonitor) -> Self {
        let mod_domain = ModModel::new(&m);
        let mod_control = ModControl::new(&m);
        let mod_planning = ModPlanning::new(&m);
        let mod_continuous_planning = ModContinuousPlanning::new(&m);
        let mut module = LModule::new(m, MOD_MONITOR, DOC_MOD_MONITOR);
        module.add_submodule(mod_domain, ImportType::WithoutPrefix);
        module.add_submodule(ModLog::default(), ImportType::WithoutPrefix);
        module.add_submodule(ModDebugConversion::default(), ImportType::WithoutPrefix);
        module.add_submodule(mod_control, ImportType::WithoutPrefix);
        module.add_submodule(mod_planning, ImportType::WithoutPrefix);
        module.add_submodule(mod_continuous_planning, ImportType::WithoutPrefix);

        module
    }
}

impl ModMonitor {
    pub async fn new(platform: impl Into<PlatformDeclaration>, log_dir: Option<PathBuf>) -> Self {
        let mut module = Self::default();

        Master::new_log_topic(
            LOG_TOPIC_OMPAS,
            log_dir.map(|p| FileDescriptor::Directory(p.canonicalize().unwrap())),
        )
        .await;

        module.log = LogClient::new(OMPAS, LOG_TOPIC_OMPAS).await;

        let platform = match platform.into() {
            PlatformDeclaration::Exec(exec) => {
                let lisp_domain = exec.read().await.domain().await;
                PlatformManager::new(
                    module.acting_manager.domain.clone(),
                    module.acting_manager.clone(),
                    Some(
                        ExecPlatform::new(
                            exec,
                            module.acting_manager.clone(),
                            Arc::new(Default::default()),
                            module.log.clone(),
                            Arc::new(Default::default()),
                        )
                        .await,
                    ),
                    lisp_domain,
                )
            }
            PlatformDeclaration::Simu(s) => PlatformManager::new(
                module.acting_manager.domain.clone(),
                module.acting_manager.clone(),
                None,
                s,
            ),
        };

        module.platform = platform;

        module.init_empty_env().await;
        module
    }

    /// Initialize the libraries to load inside Scheme env.
    /// Takes as argument the execution platform.
    ///
    async fn init_empty_env(&mut self) {
        let mut env: LEnv = get_root_env().await;
        env.import_module(ModExtendedStd::default(), WithoutPrefix);
        env.import_module(ModExec::new(&ModControl::new(self)).await, WithoutPrefix);
        eval_init(&mut env).await;
        self.empty_env = env;
    }
}
