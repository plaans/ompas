//! Module containing the Scheme library to setup RAE environment
use control::*;
use debug_conversion::*;
use domain::*;
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::Master;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::internal_state::OMPASInternalState;
use ompas_rae_structs::job::Job;
use ompas_rae_structs::rae_options::OMPASOptions;
use sompas_core::{eval_init, get_root_env};
use sompas_structs::lmodule::{InitScheme, LModule};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
pub mod control;
pub mod debug_conversion;
pub mod domain;
pub mod log;

use crate::exec::ModExec;
use crate::monitor::log::ModLog;
use ompas_rae_interface::platform::{Domain, Platform, PlatformDescriptor};
use ompas_rae_interface::PLATFORM_CLIENT;
use ompas_rae_language::monitor::*;
use ompas_rae_language::process::{LOG_TOPIC_OMPAS, OMPAS};
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::LEnv;

//LANGUAGE

pub const TOKIO_CHANNEL_SIZE: usize = 100;

pub struct ModMonitor {
    pub(crate) options: Arc<RwLock<OMPASOptions>>,
    pub(crate) interface: OMPASInternalState,
    pub(crate) platform: Option<Platform>,
    pub(crate) domain: Arc<RwLock<RAEDomain>>,
    pub(crate) platform_domain: InitScheme,
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
    /// Initialize the libraries to load inside Scheme env.
    /// Takes as argument the execution platform.
    ///

    async fn init_empty_env(&mut self) {
        let mut env: LEnv = get_root_env().await;
        env.import_module(ModAdvancedMath::default(), WithoutPrefix);
        env.import_module(ModString::default(), WithoutPrefix);
        env.import_module(ModTime::new(2), WithoutPrefix);
        env.import_module(ModUtils::default(), WithoutPrefix);
        env.import_module(
            ModExec::new(&ModControl::new(&ModMonitor::default())).await,
            WithoutPrefix,
        );
        eval_init(&mut env).await;
        self.empty_env = env;
    }

    pub async fn new(
        platform: impl PlatformDescriptor,
        working_dir: Option<PathBuf>,
        display_log: bool,
    ) -> Self {
        /*let channel =
        ompas_rae_log::init(log.clone()) //change with configurable display
            .unwrap_or_else(|e| panic!("Error while initiating logger : {}", e));*/

        Master::new_log_topic(
            LOG_TOPIC_OMPAS,
            working_dir.map(|p| FileDescriptor::AbsolutePath(p.canonicalize().unwrap())),
        )
        .await;

        let log_client = LogClient::new(OMPAS, LOG_TOPIC_OMPAS).await;

        if display_log {
            Master::start_display_log_topic(LOG_TOPIC_OMPAS).await;
        }

        let interface = OMPASInternalState {
            state: Default::default(),
            resources: Default::default(),
            monitors: Default::default(),
            agenda: Default::default(),
            log: log_client,
            command_stream: Arc::new(RwLock::new(None)),
        };

        let platform = Platform::new(
            Arc::new(RwLock::new(platform)),
            interface.state.clone(),
            interface.agenda.clone(),
            Arc::new(Default::default()),
            LogClient::new(PLATFORM_CLIENT, LOG_TOPIC_OMPAS).await,
            Arc::new(Default::default()),
        )
        .await;

        let domain: InitScheme = match platform.domain().await {
            Domain::String(s) => vec![s].into(),
            Domain::File(f) => vec![fs::read_to_string(f).unwrap()].into(),
        };

        let mut module = Self {
            options: Arc::new(Default::default()),
            interface,
            platform: Some(platform),
            domain: Default::default(),
            platform_domain: domain,
            empty_env: Default::default(),
            tasks_to_execute: Arc::new(Default::default()),
        };

        module.init_empty_env().await;
        module
    }
}

impl ModMonitor {
    pub fn get_domain(&self) -> &InitScheme {
        &self.platform_domain
    }

    pub fn set_domain(&mut self, domain: InitScheme) {
        self.platform_domain = domain;
    }
}

impl Default for ModMonitor {
    fn default() -> Self {
        Self {
            options: Default::default(),
            interface: OMPASInternalState {
                state: Default::default(),
                resources: Default::default(),
                monitors: Default::default(),
                agenda: Default::default(),
                log: Default::default(),
                command_stream: Arc::new(Default::default()),
            },
            platform: None,
            domain: Default::default(),
            platform_domain: Default::default(),
            empty_env: Default::default(),
            tasks_to_execute: Arc::new(Default::default()),
        }
    }
}
