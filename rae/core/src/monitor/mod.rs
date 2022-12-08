//! Module containing the Scheme library to setup RAE environment
use crate::exec::ModExec;
use control::*;
use debug_conversion::*;
use domain::*;
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::Master;
use ompas_rae_planning::aries::structs::ConversionContext;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::internal_state::OMPASInternalState;
use ompas_rae_structs::job::Job;
use ompas_rae_structs::rae_options::OMPASOptions;
use ompas_rae_structs::select_mode::SelectMode;
use sompas_core::{eval_init, get_root_env};
use sompas_modules::advanced_math::ModMath;
use sompas_modules::io::{LogOutput, ModIO};
use sompas_modules::utils::ModUtils;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lmodule::{InitScheme, LModule};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
pub mod control;
pub mod debug_conversion;
pub mod domain;
pub mod log;

use crate::monitor::log::ModLog;
use ompas_rae_interface::platform::{Domain, Platform, PlatformDescriptor};
use ompas_rae_interface::PLATFORM_CLIENT;
use ompas_rae_language::monitor::*;
use ompas_rae_language::process::{LOG_TOPIC_OMPAS, OMPAS};
use sompas_modules::time::ModTime;

//LANGUAGE

pub const TOKIO_CHANNEL_SIZE: usize = 100;

pub struct ModMonitor {
    pub(crate) options: Arc<RwLock<OMPASOptions>>,
    pub(crate) interface: OMPASInternalState,
    pub(crate) platform: Option<Platform>,
    pub(crate) rae_domain: Arc<RwLock<RAEDomain>>,
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

    pub async fn init_empty_env(&self) -> LEnv {
        let mut empty_env = get_root_env().await;
        empty_env.import_module(ModUtils::default(), WithoutPrefix);
        empty_env.import_module(ModMath::default(), WithoutPrefix);
        empty_env.import_module(ModIO::default(), WithoutPrefix);
        empty_env.import_module(ModExec::new(&self).await, WithoutPrefix);
        eval_init(&mut empty_env).await;
        empty_env
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

        Self {
            options: Arc::new(Default::default()),
            interface,
            platform: Some(platform),
            rae_domain: Default::default(),
            platform_domain: domain,
            //todo:
            empty_env: LEnv::default(),
            tasks_to_execute: Arc::new(Default::default()),
        }
    }

    pub fn get_empty_env(&self) -> LEnv {
        self.empty_env.clone()
    }

    pub async fn get_exec_env(&self) -> LEnv {
        let mut env: LEnv = get_root_env().await;
        let log = LogClient::new("eval-ompas", LOG_TOPIC_OMPAS).await;
        env.log = log;

        if let Some(platform) = &self.platform {
            if let Some(module) = platform.module().await {
                //println!("import of platform module");
                env.import_module(module, WithoutPrefix);
            }
        }

        env.import_module(ModUtils::default(), WithoutPrefix);

        env.import_module(ModMath::default(), WithoutPrefix);

        let mut ctx_io = ModIO::default();
        ctx_io.set_log_output(LogOutput::Log(self.interface.log.clone()));

        env.import_module(ctx_io, WithoutPrefix);

        env.import_module(ModTime::new(2), WithoutPrefix);
        env.import_module(ModExec::new(&self).await, WithoutPrefix);
        eval_init(&mut env).await;

        let domain_exec_symbols: LEnvSymbols = self.rae_domain.read().await.get_exec_env();

        env.set_new_top_symbols(domain_exec_symbols);

        env
    }
}

impl ModMonitor {
    pub async fn get_options(&self) -> OMPASOptions {
        self.options.read().await.clone()
    }

    pub async fn set_options(&self, options: OMPASOptions) {
        *self.options.write().await = options;
    }

    pub async fn set_select_mode(&self, select_mode: SelectMode) {
        self.options.write().await.set_select_mode(select_mode);
    }

    pub fn get_domain(&self) -> &InitScheme {
        &self.platform_domain
    }

    pub fn set_domain(&mut self, domain: InitScheme) {
        self.platform_domain = domain;
    }

    pub async fn get_conversion_context(&self) -> ConversionContext {
        ConversionContext {
            domain: self.rae_domain.read().await.clone(),
            env: self.get_empty_env(),
            state: self.interface.state.get_snapshot().await,
        }
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
            rae_domain: Default::default(),
            platform_domain: Default::default(),
            empty_env: Default::default(),
            tasks_to_execute: Arc::new(Default::default()),
        }
    }
}
