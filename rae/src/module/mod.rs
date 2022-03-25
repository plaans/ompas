//! Module containing the Scheme library to setup RAE environment

use crate::context::rae_env::RAEEnv;
use crate::module::rae_control::*;
use crate::module::rae_conversion::*;
use crate::module::rae_description::*;
use crate::module::rae_exec::{CtxRaeExec, Job, Platform};
use crate::module::rae_monitor::*;
use crate::module::rae_planning::plan_task;
use crate::planning::structs::ConversionContext;
use crate::supervisor::{rae_log, RAEOptions, TOKIO_CHANNEL_SIZE};
use chrono::{DateTime, Utc};
use ompas_lisp::core::structs::contextcollection::Context;
use ompas_lisp::core::structs::documentation::{Documentation, LHelp};
use ompas_lisp::core::structs::lenv::ImportType::{WithPrefix, WithoutPrefix};
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::module::{InitLisp, IntoModule, Module};
use ompas_lisp::core::structs::purefonction::PureFonctionCollection;
use ompas_lisp::modules::_type::CtxType;
use ompas_lisp::modules::advanced_math::CtxMath;
use ompas_lisp::modules::io::{CtxIo, LogOutput};
use ompas_lisp::modules::utils::CtxUtils;
use std::path::PathBuf;
use std::sync::Arc;
use std::{env, fs, mem};
use tokio::sync::mpsc::Sender;
use tokio::sync::{mpsc, RwLock};

pub mod rae_control;
pub mod rae_conversion;
pub mod rae_description;
pub mod rae_exec;
pub mod rae_monitor;
mod rae_planning;

//LANGUAGE
const MOD_RAE: &str = "rae";
const DOC_MOD_RAE: &str = "Module exposed to the user to configure and launch rae.";
const DOC_MOD_RAE_VERBOSE: &str = "functions:\n\
-getters : get-methods, get-actions, get-symbol-type, get-tasks, get-state-functions, get-env,\n\
    get-state, get-status, get-agenda, get-config-platform\n\
-definitions : def-state-function, def-actions, def-action-model, def-action-operational-model,\n\
    def-task, def-method, def-initial-state\n\
-configuration: configure-platform\n\
-launch: launch";

const DOC_RAE_LAUNCH: &str = "Launch the main rae loop in an asynchronous task.";

pub struct CtxRae {
    log: PathBuf,
    options: Arc<RwLock<RAEOptions>>,
    env: Arc<RwLock<RAEEnv>>,
    domain: InitLisp,
    sender_to_rae: Option<Sender<Job>>,
}

impl CtxRae {
    /// Initialize the libraries to load inside Scheme env.
    /// Takes as argument the execution platform.
    pub async fn init_ctx_rae(
        platform: Option<Platform>,
        working_directory: Option<PathBuf>,
    ) -> Self {
        //println!("in init ctx_rae");

        let mut ctx_rae = CtxRae::default();
        let (sender_job, receiver_job) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        ctx_rae.set_sender_to_rae(Some(sender_job));

        let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
        let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();

        let dir_path: PathBuf = match working_directory {
            Some(wd) => {
                let mut dir_path = wd;
                dir_path.push("rae_logs");
                dir_path
            }
            None => format!(
                "{}/ompas/rae_logs",
                match env::var("HOME") {
                    Ok(val) => val,
                    Err(_) => ".".to_string(),
                }
            )
            .into(),
        };

        fs::create_dir_all(&dir_path).expect("could not create rae logs directory");
        let mut file_path = dir_path.clone();
        file_path.push(format!("rae_{}.txt", string_date));

        let (mut rae_env, platform) = match platform {
            Some(platform) => {
                let (sender_sync, receiver_sync) = mpsc::channel(TOKIO_CHANNEL_SIZE);
                let mut rae_env: RAEEnv =
                    RAEEnv::new(Some(receiver_job), Some(receiver_sync)).await;
                let domain = platform.get_ref().read().await.domain().await;

                ctx_rae.set_domain(vec![domain].into());

                let context_platform = platform.get_ref().read().await.context_platform();

                rae_env
                    .env
                    .import(context_platform, WithPrefix)
                    .await
                    .expect("error loading ctx of the platform");

                rae_env.actions_progress.sync.sender = Some(sender_sync);

                platform
                    .get_ref()
                    .write()
                    .await
                    .init(rae_env.state.clone(), rae_env.actions_progress.clone())
                    .await;

                (rae_env, Some(platform))
            }
            None => (RAEEnv::new(Some(receiver_job), None).await, None),
        };

        //Clone all structs that need to be shared to monitor action status, state and agenda.

        let ctx_rae_exec = CtxRaeExec {
            actions_progress: rae_env.actions_progress.clone(),
            state: rae_env.state.clone(),
            platform_interface: platform,
            agenda: rae_env.agenda.clone(),
        };

        rae_env
            .env
            .import(CtxUtils::default(), WithoutPrefix)
            .await
            .expect("error loading utils");

        rae_env
            .env
            .import(CtxMath::default(), WithoutPrefix)
            .await
            .expect("error loading math");

        let mut ctx_io = CtxIo::default();

        let channel = rae_log::init(file_path.clone()).expect("Error while initiating logger.");

        ctx_io.set_log_output(LogOutput::Channel(channel));
        ctx_rae.set_log(file_path);

        rae_env
            .env
            .import(ctx_rae_exec, WithoutPrefix)
            .await
            .expect("error loading rae exec");

        rae_env
            .env
            .import(CtxRaeDescription::default(), WithoutPrefix)
            .await
            .expect("error loading rae description");

        rae_env
            .env
            .import(ctx_io, WithoutPrefix)
            .await
            .expect("error loading io");

        ctx_rae.set_rae_env(rae_env).await;
        ctx_rae
    }

    #[allow(unused)]
    pub(crate) async fn init_simu_env(working_directory: Option<PathBuf>) -> LEnv {
        /*Construction of the environment for simulation.
        This enviroment will contain the following modules:
        - io
        - math
        - utils
        - ctx_rae_simu
         */
        let mut env = LEnv::root().await;
        env.import(CtxUtils::default(), WithoutPrefix)
            .await
            .expect("error loading utils");

        env.import(CtxMath::default(), WithoutPrefix)
            .await
            .expect("error loading math");

        let mut ctx_io = CtxIo::default();
        if let Some(ref path) = working_directory {
            ctx_io.set_log_output(LogOutput::File(path.clone()));
        }

        env.import(ctx_io, WithoutPrefix)
            .await
            .expect("error loading io");

        /*import(&mut env, &mut ctxs, CtxRaeSim::default(), WithoutPrefix)
        .await
        .expect("error loading raesim");*/

        env.import(CtxType::default(), WithoutPrefix)
            .await
            .expect("error loading type");

        env
    }
}

impl CtxRae {
    pub fn get_log(&self) -> &PathBuf {
        &self.log
    }
    pub fn set_log(&mut self, log: PathBuf) {
        self.log = log;
    }

    pub async fn get_options(&self) -> RAEOptions {
        self.options.read().await.clone()
    }

    pub async fn set_options(&self, options: RAEOptions) {
        *self.options.write().await = options;
    }

    pub fn get_rae_env(&self) -> Arc<RwLock<RAEEnv>> {
        self.env.clone()
    }

    pub async fn set_rae_env(&self, rae_env: RAEEnv) {
        *self.env.write().await = rae_env
    }

    pub fn get_domain(&self) -> &InitLisp {
        &self.domain
    }

    pub fn set_domain(&mut self, domain: InitLisp) {
        self.domain = domain;
    }

    pub async fn own_rae_env(&self) -> RAEEnv {
        let mut src = self.env.write().await;

        let new_env = RAEEnv {
            job_receiver: None,
            status_watcher: None,
            agenda: src.agenda.clone(),
            actions_progress: src.actions_progress.clone(),
            state: src.state.clone(),
            env: src.env.clone(),
            domain_env: src.domain_env.clone(),
        };
        mem::replace(&mut *src, new_env)
    }

    pub async fn get_conversion_context(&self) -> ConversionContext {
        let rae_env = self.env.read().await;
        ConversionContext {
            domain: rae_env.domain_env.clone(),
            env: rae_env.env.clone(),
            state: rae_env.state.get_snapshot().await,
        }
    }

    pub fn set_sender_to_rae(&mut self, sender_to_rae: Option<mpsc::Sender<Job>>) {
        self.sender_to_rae = sender_to_rae;
    }

    pub fn get_sender_to_rae(&self) -> &Option<Sender<Job>> {
        &self.sender_to_rae
    }
}

impl Default for CtxRae {
    fn default() -> Self {
        Self {
            log: PathBuf::default(),
            options: Default::default(),
            env: Arc::new(RwLock::new(RAEEnv {
                job_receiver: None,
                status_watcher: None,
                agenda: Default::default(),
                actions_progress: Default::default(),
                state: Default::default(),
                env: Default::default(),
                domain_env: Default::default(),
            })),
            domain: Default::default(),
            sender_to_rae: None,
        }
    }
}

impl IntoModule for CtxRae {
    fn into_module(self) -> Module {
        let domain = self.domain.clone();
        //let domain = Default::default();
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: domain,
            label: MOD_RAE.to_string(),
        };

        module.add_async_fn_prelude(RAE_LAUNCH, rae_launch);

        module.add_async_fn_prelude(RAE_GET_METHODS, get_methods);
        module.add_async_fn_prelude(RAE_GET_STATE_FUNCTIONS, get_state_function);
        module.add_async_fn_prelude(RAE_GET_ACTIONS, get_actions);
        module.add_async_fn_prelude(RAE_GET_TASKS, get_tasks);
        //module.add_fn_prelude(RAE_GET_METHODS_PARAMETERS, get_methods_parameters);
        //module.add_fn_prelude(RAE_GET_SYMBOL_TYPE, get_symbol_type);
        module.add_async_fn_prelude(RAE_GET_ENV, get_env);
        module.add_async_fn_prelude(RAE_CONFIGURE_PLATFORM, configure_platform);
        module.add_async_fn_prelude(RAE_GET_CONFIG_PLATFORM, get_config_platform);

        //Domain Definition
        module.add_async_fn_prelude(RAE_DEF_STATE_FUNCTION, def_state_function);
        module.add_async_fn_prelude(RAE_DEF_ACTION, def_action);
        module.add_async_fn_prelude(RAE_DEF_ACTION_MODEL, def_action_model);
        module.add_async_fn_prelude(
            RAE_DEF_ACTION_OPERATIONAL_MODEL,
            def_action_operational_model,
        );
        module.add_async_fn_prelude(RAE_DEF_TASK, def_task);
        module.add_async_fn_prelude(RAE_DEF_METHOD, def_method);
        module.add_async_fn_prelude(RAE_DEF_LAMBDA, def_lambda);
        module.add_async_fn_prelude(RAE_DEF_INITIAL_STATE, def_initial_state);

        //functions to debug the functionnement of rae
        module.add_async_fn_prelude(RAE_GET_STATE, get_state);
        module.add_async_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_async_fn_prelude(RAE_GET_AGENDA, get_agenda);

        //Conversion functions
        module.add_async_fn_prelude(RAE_CONVERT_EXPR, convert_expr);
        module.add_async_fn_prelude(RAE_CONVERT_DOMAIN, convert_domain);
        module.add_async_fn_prelude(RAE_PRE_PROCESS_LAMBDA, pre_process_lambda);
        module.add_async_fn_prelude(RAE_PRE_PROCESS_EXPR, pre_process_expr);
        module.add_async_fn_prelude(RAE_PRE_PROCESS_DOMAIN, pre_process_domain);
        module.add_async_fn_prelude(RAE_CONVERT_COND_EXPR, convert_cond_expr);
        module.add_async_fn_prelude(RAE_PLAN_TASK, plan_task);

        //Trigger task
        module.add_fn_prelude(RAE_TRIGGER_EVENT, trigger_event);
        module.add_fn_prelude(RAE_TRIGGER_TASK, trigger_task);
        module.add_async_fn_prelude(RAE_GET_MUTEXES, get_mutexes);
        module.add_async_fn_prelude(RAE_GET_MONITORS, get_monitors);

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new_verbose(MOD_RAE, DOC_MOD_RAE, DOC_MOD_RAE_VERBOSE),
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
                RAE_DEF_STATE_FUNCTION,
                DOC_DEF_STATE_FUNCTION,
                DOC_DEF_STATE_FUNCTION_VERBOSE,
            ),
            LHelp::new_verbose(RAE_DEF_ACTION, DOC_DEF_ACTION, DOC_DEF_ACTION_VERBOSE),
            LHelp::new_verbose(RAE_DEF_TASK, DOC_DEF_TASK, DOC_DEF_TASK_VERBOSE),
            LHelp::new_verbose(RAE_DEF_METHOD, DOC_DEF_METHOD, DOC_DEF_METHOD_VERBOSE),
            LHelp::new(RAE_DEF_LAMBDA, DOC_DEF_LAMBDA),
            LHelp::new(RAE_DEF_INITIAL_STATE, DOC_DEF_INITIAL_STATE),
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
