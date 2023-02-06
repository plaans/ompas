//! Module containing the Scheme library to setup RAE environment
use control::*;
use debug_conversion::*;
use domain::*;
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::Master;
use ompas_structs::job::Job;
use ompas_structs::rae_options::OMPASOptions;
use sompas_core::{eval_init, get_root_env};
use sompas_structs::lmodule::LModule;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
pub mod control;
pub mod debug_conversion;
pub mod domain;
pub mod log;

use crate::exec::ModExec;
use crate::monitor::log::ModLog;
use ompas_interface::exec_platform::ExecPlatform;
use ompas_interface::platform::Platform;
use ompas_interface::platform_declaration::PlatformDeclaration;
use ompas_language::monitor::*;
use ompas_language::process::{LOG_TOPIC_OMPAS, OMPAS};
use ompas_structs::acting_domain::OMPASDomain;
use ompas_structs::agenda::Agenda;
use ompas_structs::monitor::MonitorCollection;
use ompas_structs::rae_command::OMPASJob;
use ompas_structs::resource::ResourceCollection;
use ompas_structs::state::world_state::WorldState;
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_structs::lenv::ImportType::{WithPrefix, WithoutPrefix};
use sompas_structs::lenv::LEnv;

//LANGUAGE

pub const TOKIO_CHANNEL_SIZE: usize = 100;

#[derive(Default)]
pub struct ModMonitor {
    pub(crate) options: Arc<RwLock<OMPASOptions>>,
    pub state: WorldState,
    pub resources: ResourceCollection,
    pub monitors: MonitorCollection,
    pub agenda: Agenda,
    pub log: LogClient,
    pub task_stream: Arc<RwLock<Option<tokio::sync::mpsc::Sender<OMPASJob>>>>,
    pub(crate) platform: Platform,
    pub(crate) ompas_domain: Arc<RwLock<OMPASDomain>>,
    pub(crate) empty_env: LEnv,
    pub(crate) tasks_to_execute: Arc<RwLock<Vec<Job>>>,
}

impl From<ModMonitor> for LModule {
    fn from(m: ModMonitor) -> Self {
        let mod_domain = ModDomain::new(&m);
        let mod_control = ModControl::new(&m);
        let mut module = LModule::new(m, MOD_MONITOR, DOC_MOD_MONITOR);
        module.add_submodule(mod_domain);
        module.add_submodule(ModLog::default());
        module.add_submodule(ModDebugConversion::default());
        module.add_submodule(mod_control);

        module
    }
}

impl ModMonitor {
    pub async fn new(
        platform: impl Into<PlatformDeclaration>,
        working_dir: Option<PathBuf>,
    ) -> Self {
        let mut module = Self::default();

        Master::new_log_topic(
            LOG_TOPIC_OMPAS,
            working_dir.map(|p| FileDescriptor::AbsolutePath(p.canonicalize().unwrap())),
        )
        .await;

        module.log = LogClient::new(OMPAS, LOG_TOPIC_OMPAS).await;

        let platform = match platform.into() {
            PlatformDeclaration::Exec(exec) => {
                let lisp_domain = exec.read().await.domain().await;
                Platform::new(
                    module.ompas_domain.clone(),
                    module.agenda.clone(),
                    Some(
                        ExecPlatform::new(
                            exec,
                            module.state.clone(),
                            module.agenda.clone(),
                            Arc::new(Default::default()),
                            module.log.clone(),
                            Arc::new(Default::default()),
                        )
                        .await,
                    ),
                    lisp_domain,
                )
            }
            PlatformDeclaration::Simu(s) => {
                Platform::new(module.ompas_domain.clone(), module.agenda.clone(), None, s)
            }
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
        env.import_module(ModAdvancedMath::default(), WithoutPrefix);
        env.import_module(ModString::default(), WithoutPrefix);
        env.import_module(ModTime::new(2), WithoutPrefix);
        env.import_module(ModUtils::default(), WithoutPrefix);
        env.import_module(ModString::default(), WithPrefix);

        env.import_module(
            ModExec::new(&ModControl::new(&ModMonitor::default())).await,
            WithoutPrefix,
        );
        eval_init(&mut env).await;
        self.empty_env = env;
    }
}
