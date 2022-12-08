use crate::exec::ModExec;
use crate::monitor::{ModMonitor, TOKIO_CHANNEL_SIZE};
use crate::rae;
use ompas_middleware::logger::LogClient;
use ompas_middleware::ProcessInterface;
use ompas_rae_interface::platform::{Platform, PlatformConfig, PlatformDescriptor};
use ompas_rae_language::monitor::control::*;
use ompas_rae_language::process::{LOG_TOPIC_OMPAS, PROCESS_STOP_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_rae_language::select::*;
use ompas_rae_planning::aries::conversion::convert_domain_to_chronicle_hierarchy;
use ompas_rae_planning::aries::structs::{ConversionCollection, ConversionContext};
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::internal_state::OMPASInternalState;
use ompas_rae_structs::job::Job;
use ompas_rae_structs::monitor::task_check_wait_for;
use ompas_rae_structs::rae_options::OMPASOptions;
use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::action_state::*;
use ompas_rae_structs::state::action_status::*;
use ompas_rae_structs::state::world_state::*;
use sompas_core::{eval_init, get_root_env};
use sompas_macros::*;
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::{LogOutput, ModIO};
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_type};
use std::mem;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;
use tokio::sync::{mpsc, RwLock};

pub struct ModControl {
    pub(crate) options: Arc<RwLock<OMPASOptions>>,
    pub(crate) interface: OMPASInternalState,
    pub(crate) platform: Option<Platform>,
    pub(crate) domain: Arc<RwLock<RAEDomain>>,
    pub(crate) tasks_to_execute: Arc<RwLock<Vec<Job>>>,
    pub(crate) cc: Arc<RwLock<Option<ConversionCollection>>>,
}

impl ModControl {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            options: monitor.options.clone(),
            interface: monitor.interface.clone(),
            platform: monitor.platform.clone(),
            domain: monitor.domain.clone(),
            tasks_to_execute: monitor.tasks_to_execute.clone(),
            cc: Arc::new(Default::default()),
        }
    }

    pub async fn convert_domain(&self) {
        let select_mode = *self.options.read().await.get_select_mode();

        let cc: Option<ConversionCollection> =
            if matches!(select_mode, SelectMode::Planning(Planner::Aries(_))) {
                let instant = Instant::now();
                match convert_domain_to_chronicle_hierarchy(ConversionContext {
                    domain: self.domain.read().await.clone(),
                    env: self.get_exec_env().await,
                    state: self.interface.state.get_snapshot().await,
                }) {
                    Ok(r) => {
                        self.interface
                            .log
                            .info(format!(
                                "Conversion time: {:.3} ms",
                                instant.elapsed().as_micros() as f64 / 1000.0
                            ))
                            .await;
                        Some(r)
                    }
                    Err(e) => {
                        self.options
                            .write()
                            .await
                            .set_select_mode(SelectMode::Greedy);
                        self.interface
                            .log
                            .warn(format!("Cannot plan with the domain...{e}"))
                            .await;
                        None
                    }
                }
            } else {
                None
            };
        *self.cc.write().await = cc;
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

        env.import_module(ModAdvancedMath::default(), WithoutPrefix);

        let mut ctx_io = ModIO::default();
        ctx_io.set_log_output(LogOutput::Log(self.interface.log.clone()));

        env.import_module(ctx_io, WithoutPrefix);

        env.import_module(ModTime::new(2), WithoutPrefix);
        env.import_module(ModExec::new(&self).await, WithoutPrefix);
        eval_init(&mut env).await;

        let domain_exec_symbols: LEnvSymbols = self.domain.read().await.get_exec_env();

        env.set_new_top_symbols(domain_exec_symbols);

        env
    }
}

impl From<ModControl> for LModule {
    fn from(m: ModControl) -> Self {
        let mut module = LModule::new(m, MOD_CONTROL, DOC_MOD_CONTROL);
        module.add_async_fn(START, start, DOC_START, false);
        module.add_async_fn(STOP, stop, DOC_STOP, false);
        module.add_async_fn(
            TRIGGER_TASK,
            trigger_task,
            (DOC_TRIGGER_TASK, DOC_TRIGGER_TASK_VERBOSE),
            false,
        );
        module.add_async_fn(
            ADD_TASK_TO_EXECUTE,
            add_task_to_execute,
            DOC_ADD_TASK_TO_EXECUTE,
            false,
        );
        module.add_async_fn(
            SET_CONFIG_PLATFORM,
            set_config_platform,
            DOC_SET_CONFIG_PLATFORM,
            false,
        );
        module.add_async_fn(
            GET_CONFIG_PLATFORM,
            get_config_platform,
            DOC_GET_CONFIG_PLATFORM,
            false,
        );
        module.add_async_fn(SET_SELECT, set_select, DOC_SET_SELECT, false);
        module.add_async_fn(GET_DOMAIN, get_domain, DOC_GET_SELECT, false);
        module.add_async_fn(GET_STATE, get_state, DOC_GET_STATE, false);
        module.add_async_fn(
            GET_CONFIG_PLATFORM,
            get_config_platform,
            DOC_GET_CONFIG_PLATFORM,
            false,
        );
        module.add_async_fn(
            GET_TASK_NETWORK,
            get_task_network,
            DOC_GET_TASK_NETWORK,
            false,
        );
        module.add_async_fn(
            GET_TYPE_HIERARCHY,
            get_type_hierarchy,
            DOC_GET_TYPE_HIERARCHY,
            false,
        );
        module.add_async_fn(
            GET_AGENDA,
            get_agenda,
            (DOC_GET_AGENDA, DOC_GET_AGENDA_VERBOSE),
            false,
        );
        module.add_async_fn(GET_RESOURCES, get_resources, DOC_GET_RESOURCES, false);
        module.add_async_fn(GET_MONITORS, get_monitors, DOC_GET_MONITORS, false);
        module.add_async_fn(GET_COMMANDS, get_commands, DOC_GET_COMMANDS, false);
        module.add_async_fn(GET_TASKS, get_tasks, DOC_GET_TASKS, false);
        module.add_async_fn(GET_METHODS, get_methods, DOC_GET_METHODS, false);
        module.add_async_fn(
            GET_STATE_FUNCTIONS,
            get_state_functions,
            DOC_GET_STATE_FUNCTIONS,
            false,
        );
        module.add_async_fn(GET_STATS, get_stats, DOC_GET_STATS, false);
        module.add_async_fn(EXPORT_STATS, export_stats, DOC_EXPORT_STATS, false);
        module
    }
}

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
pub async fn start(env: &LEnv) -> &str {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let mut tasks_to_execute: Vec<Job> = vec![];
    mem::swap(
        &mut *ctx.tasks_to_execute.write().await,
        &mut tasks_to_execute,
    );

    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    *ctx.interface.command_stream.write().await = Some(tx);
    //*ctx.interface.stop_tx.write().await = Some(tx_stop);
    let interface = ctx.interface.clone();
    let platform = ctx.platform.clone();

    if let Some(platform) = &platform {
        platform.start(Default::default()).await;
    }

    let env = ctx.get_exec_env().await;
    let log = ctx.interface.log.clone();

    let state = ctx.interface.state.clone();
    let receiver_event_update_state = state.subscribe_on_update().await;
    let env_clone = env.clone();
    let monitors = interface.monitors.clone();
    tokio::spawn(async move {
        task_check_wait_for(receiver_event_update_state, monitors, env_clone).await
    });

    tokio::spawn(async move {
        rae(log, env, rx).await;
    });

    /*if ctx.interface.log.display {
        ompas_rae_log::display_logger(killer.subscribe(), ctx.interface.log.path.clone());
    }*/

    for t in tasks_to_execute {
        ctx.interface
            .command_stream
            .read()
            .await
            .as_ref()
            .unwrap()
            .send(t.into())
            .await
            .expect("error sending job")
    }

    "rae launched succesfully"
}

#[async_scheme_fn]
pub async fn stop(env: &LEnv) {
    let process: ProcessInterface =
        ProcessInterface::new(PROCESS_STOP_OMPAS, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    process.kill(PROCESS_TOPIC_OMPAS).await;
    drop(process);
    if let Some(platform) = &ctx.platform {
        platform.stop().await;
    }

    tokio::time::sleep(Duration::from_secs(1)).await; //hardcoded moment to wait for all process to be killed.
    *ctx.interface.command_stream.write().await = None;
    ctx.interface.state.clear().await;
    ctx.interface.agenda.clear().await;
    ctx.interface.resources.clear().await;
    ctx.interface.monitors.clear().await;
}

/// Sends via a channel a task to execute.
#[async_scheme_fn]
pub async fn trigger_task(env: &LEnv, args: &[LValue]) -> Result<LAsyncHandle, LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let (tx, mut rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let job = Job::new(tx, args.into());

    match ctx.interface.get_sender().await {
        None => Err(LRuntimeError::new(TRIGGER_TASK, "no sender to rae")),
        Some(sender) => {
            tokio::spawn(async move {
                sender
                    .send(job.into())
                    .await
                    .expect("could not send job to rae");
            });

            Ok(rx.recv().await.unwrap())
        }
    }
}

/// Sends via a channel a task to execute.
#[async_scheme_fn]
pub async fn add_task_to_execute(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let (tx, _) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let job = Job::new(tx, args.into());

    match ctx.interface.get_sender().await {
        None => {
            ctx.tasks_to_execute.write().await.push(job);
        }
        Some(sender) => {
            tokio::spawn(async move {
                sender
                    .send(job.into())
                    .await
                    .expect("could not send job to rae");
            });
        }
    };
    Ok(())
}

#[async_scheme_fn]
pub async fn set_config_platform(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            SET_CONFIG_PLATFORM,
            args,
            1..usize::MAX,
        ));
    }
    let mut string = String::default();
    for arg in args {
        string.push_str(format!("{} ", arg).as_str())
    }
    if let Some(platform) = &ctx.platform {
        *platform.config.write().await = PlatformConfig::new_string(string);
    }

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn get_config_platform(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let string = String::new();
    match &ctx.platform {
        None => string,
        Some(p) => p.config.read().await.format(),
    }
    //string
}

#[async_scheme_fn]
pub async fn get_select(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    ctx.options.read().await.get_select_mode().to_string()
}

#[async_scheme_fn]
pub async fn set_select(env: &LEnv, m: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let select_mode = match m.as_str() {
        GREEDY => SelectMode::Greedy,
        PLANNING | ARIES => SelectMode::Planning(Planner::Aries(false)),
        ARIES_OPT => SelectMode::Planning(Planner::Aries(true)),
        UPOM => SelectMode::Planning(Planner::UPOM),
        RAE_PLAN => SelectMode::Planning(Planner::RAEPlan(Default::default())),
        C_CHOICE => SelectMode::Planning(Planner::CChoice(Default::default())),
        HEURISTIC => SelectMode::Heuristic,
        LEARNING => SelectMode::Learning,
        _ => {
            return Err(lruntimeerror!(
                SET_SELECT,
                format!(
                    "Select mode is either {}, {}, {} or {}.",
                    GREEDY, PLANNING, HEURISTIC, LEARNING
                )
            ))
        }
    };

    ctx.options.write().await.set_select_mode(select_mode);
    Ok(())
}

/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
#[async_scheme_fn]
pub async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let _type = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(sym) = &args[0] {
                match sym.as_str() {
                    KEY_STATIC => Some(StateType::Static),
                    KEY_DYNAMIC => Some(StateType::Dynamic),
                    KEY_INNER_WORLD => Some(StateType::InnerWorld),
                    KEY_INSTANCE => Some(StateType::Instance),
                    _ => {
                        return Err(lruntimeerror!(
                            GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                            )
                        ))
                    }
                }
            } else {
                return Err(wrong_type!(GET_STATE, &args[0], KindLValue::Symbol));
            }
        }
        _ => return Err(LRuntimeError::wrong_number_of_args(GET_STATE, args, 0..1)),
    };
    let state = ctx.interface.state.get_state(_type).await;
    Ok(state.into_map())
}

#[async_scheme_fn]
pub async fn get_task_network(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    ctx.interface.agenda.format_task_network().await
}

#[async_scheme_fn]
pub async fn get_type_hierarchy(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    ctx.domain.read().await.types.format_hierarchy()
}

#[async_scheme_fn]
pub async fn get_agenda(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let mut task_filter = TaskFilter::default();

    for arg in args {
        match arg.to_string().as_str() {
            TASK => task_filter.task_type = Some(TaskType::Task),
            COMMAND => task_filter.task_type = Some(TaskType::Command),
            STATUS_PENDING => task_filter.status = Some(ActionStatus::Pending),
            STATUS_ACCEPTED => task_filter.status = Some(ActionStatus::Accepted),
            STATUS_REJECTED => task_filter.status = Some(ActionStatus::Rejected),
            STATUS_RUNNING => task_filter.status = Some(ActionStatus::Running(None)),
            STATUS_SUCCESS => task_filter.status = Some(ActionStatus::Success),
            STATUS_FAILURE => task_filter.status = Some(ActionStatus::Failure),
            STATUS_CANCELLED => task_filter.status = Some(ActionStatus::Cancelled(true)),

            str => {
                return Err(lruntimeerror!(
                    GET_AGENDA,
                    format!(
                        "{} is not a valid filter option, expecting ({}, {}, {}, {}, {}, {})",
                        str,
                        TASK,
                        COMMAND,
                        STATUS_PENDING,
                        STATUS_RUNNING,
                        STATUS_SUCCESS,
                        STATUS_FAILURE
                    )
                ))
            }
        }
    }

    let string = ctx
        .interface
        .agenda
        .format_task_collection(task_filter)
        .await;
    Ok(string.into())
}

//Conversion functions

#[async_scheme_fn]
pub async fn get_resources(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.interface.resources.get_debug().await.into())
}

#[async_scheme_fn]
pub async fn get_monitors(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.interface.monitors.get_debug().await.into())
}

///Get the list of actions in the environment
#[async_scheme_fn]
pub async fn get_commands(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.domain.read().await.get_list_commands())
}

///Get the list of tasks in the environment
#[async_scheme_fn]
pub async fn get_tasks(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.domain.read().await.get_list_tasks())
}

///Get the methods of a given task
#[async_scheme_fn]
pub async fn get_methods(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.domain.read().await.get_list_methods())
}

///Get the list of state functions in the environment
#[async_scheme_fn]
pub async fn get_state_functions(env: &LEnv) -> LValue {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    ctx.domain.read().await.get_list_state_functions()
}

/// Returns the whole RAE environment if no arg et the entry corresponding to the symbol passed in args.
#[async_scheme_fn]
pub async fn get_domain(env: &LEnv, args: &[LValue]) -> LResult {
    let key = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(key) = args[0].clone() {
                Some(key)
            } else {
                return Err(wrong_type!(GET_DOMAIN, &args[0], KindLValue::Symbol));
            }
        }
        _ => return Err(LRuntimeError::wrong_number_of_args(GET_DOMAIN, args, 0..1)),
    };

    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    match key {
        None => Ok(ctx.domain.read().await.to_string().into()),
        Some(key) => Ok(ctx
            .domain
            .read()
            .await
            .get_element_description(key.as_ref())
            .into()),
    }
}

#[async_scheme_fn]
pub async fn get_stats(env: &LEnv) -> LValue {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    ctx.interface.agenda.get_stats().await
}

#[async_scheme_fn]
pub async fn export_stats(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let file = if args.len() == 1 {
        Some(args[0].to_string())
    } else {
        None
    };
    ctx.interface.agenda.export_to_csv(None, file).await;
    Ok(LValue::Nil)
}
