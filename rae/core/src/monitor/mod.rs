//! Module containing the Scheme library to setup RAE environment

use crate::contexts::ctx_domain::{CtxDomain, CTX_DOMAIN};
use crate::contexts::ctx_mode::{CtxMode, CTX_MODE};
use crate::contexts::ctx_rae::{CtxRae, CTX_RAE};
use crate::contexts::ctx_state::{CtxState, CTX_STATE};
use crate::contexts::ctx_task::{CtxTask, CTX_TASK};
use crate::exec::CtxRaeExec;
use domain::*;
use log::{activate_log, deactivate_log};
use ompas_middleware::logger::{FileDescriptor, LogClient};
use ompas_middleware::Master;
use ompas_rae_language::*;
use ompas_rae_planning::aries::structs::ConversionContext;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::internal_state::OMPASInternalState;
use ompas_rae_structs::job::Job;
use ompas_rae_structs::rae_options::OMPASOptions;
use ompas_rae_structs::select_mode::SelectMode;
use planning::*;
use sompas_core::{eval_init, get_root_env};
use sompas_modules::advanced_math::CtxMath;
use sompas_modules::io::{CtxIo, LogOutput};
use sompas_modules::utils::CtxUtils;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::module::{InitLisp, IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use user_interface::*;
pub mod domain;
pub mod log;
pub mod planning;
pub mod user_interface;

use crate::monitor::log::{get_log_level, set_log_level};
use ompas_rae_interface::platform::{Domain, Platform, PlatformDescriptor};
use ompas_rae_interface::PLATFORM_CLIENT;

//LANGUAGE
const MOD_RAE_USER: &str = "rae_user";
const DOC_MOD_RAE: &str = "Module exposed to the user to configure and launch rae.";
const DOC_MOD_RAE_VERBOSE: &str = "functions:\n\
-getters : get-methods, get-actions, get-symbol-type, get-tasks, get-state-functions, get-env,\n\
    get-state, get-status, get-agenda, get-config-platform\n\
-definitions : def-state-function, def-actions, def-action-model, def-action-operational-model,\n\
    def-task, def-method, def-initial-state\n\
-configuration: configure-platform\n\
-launch: launch";

const DOC_RAE_LAUNCH: &str = "Launch the main rae loop in an asynchronous task.";

pub const TOKIO_CHANNEL_SIZE: usize = 100;

pub struct CtxRaeUser {
    options: Arc<RwLock<OMPASOptions>>,
    interface: OMPASInternalState,
    platform: Option<Platform>,
    rae_domain: Arc<RwLock<RAEDomain>>,
    platform_domain: InitLisp,
    empty_env: LEnv,
    tasks_to_execute: Arc<RwLock<Vec<Job>>>,
}

impl IntoModule for CtxRaeUser {
    fn into_module(self) -> Module {
        let mut init_lisp: InitLisp = vec![
            MACRO_DEF_COMMAND,
            MACRO_DEF_TASK,
            MACRO_DEF_METHOD,
            MACRO_DEF_LAMBDA,
            MACRO_DEF_STATE_FUNCTION,
            MACRO_PDDL_MODEL,
            MACRO_OM_MODEL,
            MACRO_DEF_COMMAND_PDDL_MODEL,
            MACRO_DEF_COMMAND_OM_MODEL,
            MACRO_DEF_INITIAL_STATE,
            MACRO_DEF_TYPES,
            MACRO_DEF_OBJECTS,
        ]
        .into();
        init_lisp.append(&mut self.platform_domain.clone());

        //let domain = Default::default();
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: init_lisp,
            label: MOD_RAE_USER.to_string(),
        };

        /*
        RAE control
         */
        module.add_async_fn_prelude(RAE_LAUNCH, launch);
        module.add_async_fn_prelude(RAE_STOP, stop);
        module.add_async_fn_prelude(RAE_CONFIGURE_PLATFORM, configure_platform);
        module.add_async_fn_prelude(RAE_SET_SELECT, set_select);
        module.add_async_fn_prelude(RAE_TRIGGER_TASK, trigger_task);
        module.add_async_fn_prelude(RAE_ADD_TASK_TO_EXECUTE, add_task_to_execute);

        /*
        LOG
         */
        module.add_async_fn_prelude(ACTIVATE_LOG, activate_log);
        module.add_async_fn_prelude(DEACTIVATE_LOG, deactivate_log);
        module.add_async_fn_prelude(GET_LOG_LEVEL, get_log_level);
        module.add_async_fn_prelude(SET_LOG_LEVEL, set_log_level);
        /*
        GETTERS
         */
        module.add_async_fn_prelude(RAE_GET_METHODS, get_methods);
        module.add_async_fn_prelude(RAE_GET_STATE_FUNCTIONS, get_state_function);
        module.add_async_fn_prelude(RAE_GET_ACTIONS, get_actions);
        module.add_async_fn_prelude(RAE_GET_TASKS, get_tasks);
        module.add_async_fn_prelude(RAE_GET_ENV, get_env);
        module.add_async_fn_prelude(RAE_GET_CONFIG_PLATFORM, get_config_platform);
        module.add_async_fn_prelude(RAE_GET_SELECT, get_select);

        //Domain Definition
        module.add_async_fn_prelude(RAE_ADD_STATE_FUNCTION, add_state_function);
        module.add_async_fn_prelude(RAE_ADD_COMMAND, add_command);
        module.add_async_fn_prelude(RAE_ADD_COMMAND_MODEL, add_command_model);
        module.add_async_fn_prelude(RAE_ADD_TASK_MODEL, add_task_model);
        module.add_async_fn_prelude(RAE_ADD_TASK, add_task);
        module.add_async_fn_prelude(RAE_ADD_METHOD, add_method);
        module.add_async_fn_prelude(RAE_ADD_LAMBDA, add_lambda);
        module.add_async_fn_prelude(RAE_ADD_FACTS, add_facts);
        module.add_async_fn_prelude(RAE_ADD_CONSTANT, add_object);
        module.add_async_fn_prelude(RAE_ADD_OBJECT, add_object);
        module.add_async_fn_prelude(RAE_ADD_TYPE, add_type);
        //module.add_async_fn_prelude(RAE_DEF_CONSTANTS, add_objects);
        module.add_async_fn_prelude(RAE_ADD_TYPES, add_types);
        module.add_async_fn_prelude(RAE_ADD_OBJECTS, add_objects);
        module.add_async_fn_prelude(GENERATE_TEST_TYPE_EXPR, generate_test_type_expr);

        //functions to debug the functioning of rae
        module.add_async_fn_prelude(RAE_GET_STATE, get_state);
        module.add_async_fn_prelude(RAE_GET_AGENDA, get_agenda);
        module.add_async_fn_prelude(RAE_GET_TASK_NETWORK, get_task_network);
        module.add_async_fn_prelude(RAE_GET_TYPE_HIERARCHY, get_type_hierarchy);
        module.add_async_fn_prelude(RAE_GET_STATS, get_stats);
        module.add_async_fn_prelude(RAE_EXPORT_STATS, export_stats);

        //Conversion functions
        module.add_async_fn_prelude(RAE_CONVERT_EXPR, convert_expr);
        module.add_async_fn_prelude(RAE_CONVERT_DOMAIN, convert_domain);
        module.add_async_fn_prelude(RAE_PRE_PROCESS_LAMBDA, pre_process_lambda);
        module.add_async_fn_prelude(RAE_PRE_PROCESS_EXPR, pre_process_expr);
        module.add_async_fn_prelude(RAE_PRE_PROCESS_DOMAIN, pre_process_domain);
        module.add_async_fn_prelude(RAE_CONVERT_COND_EXPR, convert_cond_expr);
        module.add_async_fn_prelude(RAE_PLAN_TASK, plan_task);

        //Trigger task

        module.add_async_fn_prelude(RAE_GET_RESOURCES, get_resources);
        module.add_async_fn_prelude(RAE_GET_MONITORS, get_monitors);

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new_verbose(MOD_RAE_USER, DOC_MOD_RAE, DOC_MOD_RAE_VERBOSE),
            LHelp::new(RAE_GET_METHODS, DOC_RAE_GET_METHODS),
            LHelp::new(RAE_GET_ACTIONS, DOC_RAE_GET_ACTIONS),
            LHelp::new_verbose(
                RAE_GET_SYMBOL_TYPE,
                DOC_RAE_GET_SYMBOL_TYPE,
                DOC_RAE_GET_SYMBOL_TYPE_VERBOSE,
            ),
            LHelp::new(RAE_GET_TASKS, DOC_RAE_GET_TASKS),
            LHelp::new(RAE_GET_STATE_FUNCTIONS, DOC_RAE_GET_STATE_FUNCTIONS),
            LHelp::new(RAE_GET_ENV, DOC_RAE_GET_ENV),
            LHelp::new(RAE_LAUNCH, DOC_RAE_LAUNCH),
            LHelp::new(RAE_GET_STATE, DOC_RAE_GET_STATE),
            LHelp::new(RAE_GET_STATUS, DOC_RAE_GET_STATUS),
            LHelp::new_verbose(
                RAE_ADD_STATE_FUNCTION,
                DOC_ADD_STATE_FUNCTION,
                DOC_ADD_STATE_FUNCTION_VERBOSE,
            ),
            LHelp::new_verbose(RAE_ADD_COMMAND, DOC_ADD_ACTION, DOC_ADD_ACTION_VERBOSE),
            LHelp::new_verbose(RAE_ADD_TASK, DOC_ADD_TASK, DOC_ADD_TASK_VERBOSE),
            LHelp::new_verbose(RAE_ADD_METHOD, DOC_ADD_METHOD, DOC_ADD_METHOD_VERBOSE),
            LHelp::new(RAE_ADD_LAMBDA, DOC_ADD_LAMBDA),
            LHelp::new(RAE_ADD_FACTS, DOC_ADD_INITIAL_STATE),
            LHelp::new(RAE_CONFIGURE_PLATFORM, DOC_RAE_CONFIGURE_PLATFORM),
            LHelp::new(RAE_GET_CONFIG_PLATFORM, DOC_RAE_GET_CONFIG_PLATFORM),
            LHelp::new(RAE_GET_AGENDA, DOC_RAE_GET_AGENDA),
            LHelp::new_verbose(
                RAE_TRIGGER_TASK,
                DOC_RAE_TRIGGER_TASK,
                DOC_RAE_TRIGGER_EVENT_VERBOSE,
            ),
            LHelp::new_verbose(
                RAE_TRIGGER_EVENT,
                DOC_RAE_TRIGGER_EVENT,
                DOC_RAE_TRIGGER_TASK_VERBOSE,
            ),
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        vec![].into()
    }
}

impl CtxRaeUser {
    /// Initialize the libraries to load inside Scheme env.
    /// Takes as argument the execution platform.
    ///

    pub async fn init_empty_env() -> LEnv {
        let mut empty_env = get_root_env().await;
        empty_env.import_module(CtxUtils::default(), WithoutPrefix);
        empty_env.import_module(CtxMath::default(), WithoutPrefix);
        empty_env.import_module(CtxIo::default(), WithoutPrefix);
        empty_env.import_module(CtxRaeExec::default(), WithoutPrefix);
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

        let domain: InitLisp = match platform.domain().await {
            Domain::String(s) => vec![s].into(),
            Domain::File(f) => vec![fs::read_to_string(f).unwrap()].into(),
        };

        Self {
            options: Arc::new(Default::default()),
            interface,
            platform: Some(platform),
            rae_domain: Default::default(),
            platform_domain: domain,
            empty_env: Self::init_empty_env().await,
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

        env.import_module(CtxUtils::default(), WithoutPrefix);

        env.import_module(CtxMath::default(), WithoutPrefix);

        let mut ctx_io = CtxIo::default();
        ctx_io.set_log_output(LogOutput::Log(self.interface.log.clone()));

        env.import_module(ctx_io, WithoutPrefix);

        let ctx_rae = CtxRae {
            resources: self.interface.resources.clone(),
            monitors: self.interface.monitors.clone(),
            platform_interface: self.platform.clone(),
            agenda: self.interface.agenda.clone(),
            log_client: self.interface.log.clone(),
        };

        let ctx_state = CtxState {
            state: self.interface.state.clone(),
        };

        env.import_context(Context::new(ctx_rae), CTX_RAE);
        env.import_module(CtxRaeExec::default(), WithoutPrefix);
        env.import_context(Context::new(ctx_state), CTX_STATE);
        env.import_context(Context::new(CtxTask::default()), CTX_TASK);
        env.import_context(Context::new(CtxMode::default()), CTX_MODE);
        env.import_context(
            Context::new(CtxDomain::new(self.rae_domain.read().await.clone())),
            CTX_DOMAIN,
        );
        eval_init(&mut env).await;

        let domain_exec_symbols: LEnvSymbols = self.rae_domain.read().await.get_exec_env();

        env.set_new_top_symbols(domain_exec_symbols);

        env
    }
}

impl CtxRaeUser {
    /*pub fn get_log(&self) -> &PathBuf {
        &self.interface.log.path
    }
    pub fn set_log(&mut self, log: PathBuf) {
        self.interface.log.path = log;
    }*/

    pub async fn get_options(&self) -> OMPASOptions {
        self.options.read().await.clone()
    }

    pub async fn set_options(&self, options: OMPASOptions) {
        *self.options.write().await = options;
    }

    pub async fn set_select_mode(&self, select_mode: SelectMode) {
        self.options.write().await.set_select_mode(select_mode);
    }

    pub fn get_domain(&self) -> &InitLisp {
        &self.platform_domain
    }

    pub fn set_domain(&mut self, domain: InitLisp) {
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

impl Default for CtxRaeUser {
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
