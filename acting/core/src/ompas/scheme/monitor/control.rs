use crate::ompas::interface::job::Job;
use crate::ompas::interface::rae_command::OMPASJob;
use crate::ompas::interface::select_mode::{Planner, SelectMode};
use crate::ompas::interface::trigger_collection::{Response, TaskTrigger, TriggerCollection};
use crate::ompas::manager::acting::filter::ProcessFilter;
use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::acting::ActingManager;
use crate::ompas::manager::monitor::task_check_wait_for;
use crate::ompas::manager::ompas::OMPASManager;
use crate::ompas::manager::platform::platform_config::PlatformConfig;
use crate::ompas::manager::platform::PlatformManager;
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::state_manager::StateType;
use crate::ompas::scheme::exec::ModExec;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::ompas::scheme::monitor::ModMonitor;
use crate::ompas::{rae, TOKIO_CHANNEL_SIZE};
use crate::planning::planner::solver::PMetric;
use ompas_language::exec::state::{DYNAMIC, INNER_DYNAMIC, INNER_STATIC, INSTANCE, STATIC};
use ompas_language::monitor::control::*;
use ompas_language::monitor::model::MOD_MODEL;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_STOP_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_language::select::*;
use ompas_language::supervisor::*;
use ompas_middleware::logger::LogClient;
use ompas_middleware::ProcessInterface;
use sompas_core::{eval_init, get_root_env};
use sompas_macros::*;
use sompas_modules::advanced_math::ModAdvancedMath;
use sompas_modules::io::{LogOutput, ModIO};
use sompas_modules::string::ModString;
use sompas_modules::time::ModTime;
use sompas_modules::utils::ModUtils;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::ImportType::{WithPrefix, WithoutPrefix};
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_type};
use std::mem;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, RwLock};

pub struct ModControl {
    pub(crate) options: OMPASManager,
    pub acting_manager: ActingManager,
    pub log: LogClient,
    pub task_stream: Arc<RwLock<Option<tokio::sync::mpsc::Sender<OMPASJob>>>>,
    pub(crate) platform: PlatformManager,
    pub(crate) tasks_to_execute: Arc<RwLock<Vec<Job>>>,
    pub(crate) triggers: TriggerCollection,
}

impl ModControl {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            options: monitor.options.clone(),
            acting_manager: monitor.acting_manager.clone(),
            log: monitor.log.clone(),
            task_stream: monitor.task_stream.clone(),
            platform: monitor.platform.clone(),
            tasks_to_execute: monitor.tasks_to_execute.clone(),
            triggers: Default::default(),
        }
    }

    pub async fn get_exec_env(&self) -> LEnv {
        let mut env: LEnv = get_root_env().await;
        let log = LogClient::new("eval-ompas", LOG_TOPIC_OMPAS).await;
        env.log = log;

        if let Some(module) = self.platform.module().await {
            //println!("import of platform module");
            env.import_module(module, WithoutPrefix);
        }

        env.import_module(ModUtils::default(), WithoutPrefix);

        env.import_module(ModAdvancedMath::default(), WithoutPrefix);

        env.import_module(ModString::default(), WithPrefix);

        let mut ctx_io = ModIO::default();
        ctx_io.set_log_output(LogOutput::Log(self.log.clone()));

        env.import_module(ctx_io, WithoutPrefix);

        env.import_module(ModTime::new(2), WithoutPrefix);
        env.import_module(ModExec::new(self).await, WithoutPrefix);
        eval_init(&mut env).await;

        let domain_exec_symbols: LEnvSymbols = self.acting_manager.domain.get_exec_env().await;

        env.set_new_top_symbols(domain_exec_symbols);

        env
    }

    pub async fn get_sender(&self) -> Option<tokio::sync::mpsc::Sender<OMPASJob>> {
        self.task_stream.read().await.clone()
    }

    pub async fn reboot(&self) {
        self.acting_manager.clear().await;
        self.triggers.clear().await;
        *self.task_stream.write().await = None;
        self.tasks_to_execute.write().await.clear();
    }
}

impl From<ModControl> for LModule {
    fn from(m: ModControl) -> Self {
        let mut module = LModule::new(m, MOD_CONTROL, DOC_MOD_CONTROL);
        module.add_async_fn(START, start, DOC_START, false);
        module.add_async_fn(
            START_WITH_PLANNER,
            start_with_planner,
            DOC_START_WITH_PLANNER,
            false,
        );
        module.add_async_fn(STOP, stop, DOC_STOP, false);
        module.add_async_fn(
            __DEBUG_OMPAS__,
            __debug_ompas__,
            DOC___DEBUG_OMPAS___,
            false,
        );
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
        module.add_async_fn(_WAIT_TASK, _wait_task, DOC__WAIT_TASK, false);
        module.add_lambda(WAIT_TASK, LAMBDA_WAIT_TASK, DOC_WAIT_TASK);
        module.add_async_fn(GET_TASK_ID, get_task_id, DOC_GET_TASK_ID, false);
        module.add_async_fn(CANCEL_TASK, cancel_task, DOC_CANCEL_TASK, false);

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
            print_process_network,
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
            print_processes,
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
        module.add_async_fn(DUMP_TRACE, dump_trace, DOC_DUMP_TRACE, false);
        module.add_macro(DEBUG_OMPAS, MACRO_DEBUG_OMPAS, DOC_DEBUG_OMPAS);

        module
    }
}

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
pub async fn start(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let acting_manager = ctx.acting_manager.clone();
    acting_manager.clock_manager.reset().await;
    let mut tasks_to_execute: Vec<Job> = vec![];
    mem::swap(
        &mut *ctx.tasks_to_execute.write().await,
        &mut tasks_to_execute,
    );

    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    *ctx.task_stream.write().await = Some(tx);

    let start_result: String = ctx.platform.start(Default::default()).await?;

    let env = ctx.get_exec_env().await;
    let log = ctx.log.clone();

    let receiver_event_update_state = acting_manager.state.subscribe_on_update().await;
    let env_clone = env.clone();
    let monitors = acting_manager.monitor_manager.clone();
    tokio::spawn(async move {
        task_check_wait_for(receiver_event_update_state, monitors, env_clone).await
    });

    tokio::spawn(async move {
        rae(acting_manager, log, env, rx).await;
    });

    for t in tasks_to_execute {
        ctx.task_stream
            .read()
            .await
            .as_ref()
            .unwrap()
            .send(t.into())
            .await
            .expect("error sending job")
    }

    Ok(format!("OMPAS launched successfully. {}", start_result))
}

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
pub async fn start_with_planner(env: &LEnv, opt: bool) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let plan_env: LEnv = ctx.get_plan_env().await;
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let acting_manager = ctx.acting_manager.clone();
    acting_manager
        .start_continuous_planning(plan_env, if opt { Some(PMetric::Makespan) } else { None })
        .await;
    let mut tasks_to_execute: Vec<Job> = vec![];
    mem::swap(
        &mut *ctx.tasks_to_execute.write().await,
        &mut tasks_to_execute,
    );

    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    *ctx.task_stream.write().await = Some(tx);

    let start_result: String = ctx.platform.start(Default::default()).await?;

    let env = ctx.get_exec_env().await;
    let log = ctx.log.clone();

    let receiver_event_update_state = acting_manager.state.subscribe_on_update().await;
    let env_clone = env.clone();
    let monitors = acting_manager.monitor_manager.clone();
    tokio::spawn(async move {
        task_check_wait_for(receiver_event_update_state, monitors, env_clone).await
    });

    tokio::spawn(async move {
        rae(acting_manager, log, env, rx).await;
    });

    for t in tasks_to_execute {
        ctx.task_stream
            .read()
            .await
            .as_ref()
            .unwrap()
            .send(t.into())
            .await
            .expect("error sending job")
    }

    Ok(format!("OMPAS launched successfully. {}", start_result))
}

#[async_scheme_fn]
pub async fn stop(env: &LEnv) {
    let process: ProcessInterface =
        ProcessInterface::new(PROCESS_STOP_OMPAS, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    process.kill(PROCESS_TOPIC_OMPAS).await;
    drop(process);
    ctx.platform.stop().await;

    tokio::time::sleep(Duration::from_secs(1)).await; //hardcoded moment to wait for all process to be killed.
    ctx.reboot().await;
}

/// Sends via a channel a task to execute.
#[async_scheme_fn]
pub async fn __debug_ompas__(env: &LEnv, arg: LValue) -> LResult {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let (tx, mut rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let job = Job::new_debug(tx, arg);

    match ctx.get_sender().await {
        None => Err(LRuntimeError::new(__DEBUG_OMPAS__, "no sender to rae")),
        Some(sender) => {
            tokio::spawn(async move {
                sender
                    .send(job.into())
                    .await
                    .expect("could not send job to rae");
            });

            if let Response::Handle(handle) = rx.recv().await.unwrap()? {
                handle.get_future().await
            } else {
                unreachable!()
            }
        }
    }
}

/// Sends via a channel a task to execute.
#[async_scheme_fn]
pub async fn trigger_task(env: &LEnv, args: &[LValue]) -> Result<usize, LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let (tx, mut rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let task = args[0].to_string();
    if !ctx.acting_manager.domain.is_task(&task).await {
        return Err(LRuntimeError::new(
            TRIGGER_TASK,
            format!("{} is not a task.", task),
        ));
    }

    let job = Job::new_task(tx, args.into());

    match ctx.get_sender().await {
        None => Err(LRuntimeError::new(TRIGGER_TASK, "no sender to rae")),
        Some(sender) => {
            tokio::spawn(async move {
                sender
                    .send(job.into())
                    .await
                    .expect("could not send job to rae");
            });
            let trigger: Response = rx.recv().await.unwrap()?;
            if let Response::Trigger(trigger) = trigger {
                Ok(ctx.triggers.add_task(trigger).await)
            } else {
                unreachable!("trigger-task should receive a TaskTrigger struct.")
            }
        }
    }
}

/// Sends via a channel a task to execute.
#[async_scheme_fn]
pub async fn add_task_to_execute(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let (tx, _) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let task = args[0].to_string();

    if !ctx.acting_manager.domain.is_task(&task).await {
        return Err(LRuntimeError::new(
            ADD_TASK_TO_EXECUTE,
            format!("{} is not a task.", task),
        ));
    }

    let job = Job::new_task(tx, args.into());

    match ctx.get_sender().await {
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
pub async fn _wait_task(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let trigger: Option<TaskTrigger> = ctx.triggers.get_task(task_id).await;
    match trigger {
        Some(trigger) => Ok(LValue::Handle(trigger.get_handle())),
        None => Err(LRuntimeError::new(
            WAIT_TASK,
            format!("{} is not the id of a triggered task", task_id),
        )),
    }
}

#[async_scheme_fn]
pub async fn get_task_id(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let trigger: Option<TaskTrigger> = ctx.triggers.get_task(task_id).await;
    match trigger {
        Some(trigger) => Ok(format!("{:?}", trigger.get_ref()).into()),
        None => Err(LRuntimeError::new(
            WAIT_TASK,
            format!("{} is not the id of a triggered task", task_id),
        )),
    }
}

#[async_scheme_fn]
pub async fn cancel_task(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let trigger: Option<TaskTrigger> = ctx.triggers.get_task(task_id).await;
    match trigger {
        Some(trigger) => trigger.get_handle().interrupt().await,
        None => Err(LRuntimeError::new(
            WAIT_TASK,
            format!("{} is not the id of a triggered task", task_id),
        )),
    }
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

    match ctx
        .platform
        .try_set_config_platform(PlatformConfig::new_string(string))
        .await
    {
        Ok(_) => Ok(LValue::Nil),
        Err(_) => Err(LRuntimeError::new(
            SET_CONFIG_PLATFORM,
            "Could not set config as there is no execution platform.",
        )),
    }
}

#[async_scheme_fn]
pub async fn get_config_platform(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    match &ctx.platform.try_get_config_platform().await {
        Err(_) => Err(LRuntimeError::new(
            GET_CONFIG_PLATFORM,
            "No platform is defined.",
        )),
        Ok(p) => Ok(p.format()),
    }
}

#[async_scheme_fn]
pub async fn get_select(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    ctx.options.get_select_mode().await.to_string()
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

    ctx.options.set_select_mode(select_mode).await;
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
                    STATIC => Some(StateType::Static),
                    DYNAMIC => Some(StateType::Dynamic),
                    INNER_STATIC => Some(StateType::InnerStatic),
                    INNER_DYNAMIC => Some(StateType::Dynamic),
                    INSTANCE => Some(StateType::Instance),
                    _ => {
                        return Err(lruntimeerror!(
                            GET_STATE,
                            format!("was expecting keys {:?}", [STATIC, DYNAMIC])
                        ))
                    }
                }
            } else {
                return Err(wrong_type!(GET_STATE, &args[0], KindLValue::Symbol));
            }
        }
        _ => return Err(LRuntimeError::wrong_number_of_args(GET_STATE, args, 0..1)),
    };
    let state = ctx.acting_manager.state.get_state(_type).await;
    Ok(state.into_map())
}

#[async_scheme_fn]
pub async fn print_process_network(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    ctx.acting_manager.format_task_network().await
}

#[async_scheme_fn]
pub async fn get_type_hierarchy(env: &LEnv) -> String {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    format!("{:?}", ctx.acting_manager.st.get_lattice())
}

#[async_scheme_fn]
pub async fn print_processes(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let mut task_filter = ProcessFilter::default();

    for arg in args {
        match arg.to_string().as_str() {
            ACTION => task_filter.task_type = Some(ProcessKind::Action),
            ARBITRARY => task_filter.task_type = Some(ProcessKind::Arbitrary),
            ACQUIRE => task_filter.task_type = Some(ProcessKind::Acquire),
            METHOD => task_filter.task_type = Some(ProcessKind::Method),
            STATUS_PENDING => task_filter.status = Some(ProcessStatus::Pending),
            STATUS_ACCEPTED => task_filter.status = Some(ProcessStatus::Accepted),
            STATUS_REJECTED => task_filter.status = Some(ProcessStatus::Rejected),
            STATUS_RUNNING => task_filter.status = Some(ProcessStatus::Running(None)),
            STATUS_SUCCESS => task_filter.status = Some(ProcessStatus::Success),
            STATUS_FAILURE => task_filter.status = Some(ProcessStatus::Failure),
            STATUS_CANCELLED => task_filter.status = Some(ProcessStatus::Cancelled(true)),

            str => {
                return Err(lruntimeerror!(
                    GET_AGENDA,
                    format!(
                        "{} is not a valid filter option, expecting ({}, {}, {}, {}, {})",
                        str, ACTION, STATUS_PENDING, STATUS_RUNNING, STATUS_SUCCESS, STATUS_FAILURE
                    )
                ))
            }
        }
    }

    let string = ctx.acting_manager.print_processes(task_filter).await;
    Ok(string.into())
}

//Conversion functions

#[async_scheme_fn]
pub async fn get_resources(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.resource_manager.get_debug().await.into())
}

#[async_scheme_fn]
pub async fn get_monitors(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.monitor_manager.get_debug().await.into())
}

///Get the list of actions in the environment
#[async_scheme_fn]
pub async fn get_commands(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.domain.get_list_commands().await)
}

///Get the list of tasks in the environment
#[async_scheme_fn]
pub async fn get_tasks(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.domain.get_list_tasks().await)
}

///Get the methods of a given task
#[async_scheme_fn]
pub async fn get_methods(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.domain.get_list_methods().await)
}

///Get the list of state functions in the environment
#[async_scheme_fn]
pub async fn get_state_functions(env: &LEnv) -> LValue {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    ctx.acting_manager.domain.get_list_state_functions().await
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
        None => Ok(ctx.acting_manager.domain.format().await.into()),
        Some(key) => Ok(ctx
            .acting_manager
            .domain
            .get_element_description(key.as_ref())
            .await
            .into()),
    }
}

#[async_scheme_fn]
pub async fn get_stats(env: &LEnv) -> LValue {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    ctx.acting_manager.get_stats().await
}

#[async_scheme_fn]
pub async fn export_stats(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let file = if args.len() == 1 {
        Some(args[0].to_string())
    } else {
        None
    };
    ctx.acting_manager.export_to_csv(None, file).await;
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn dump_trace(env: &LEnv) -> Result<(), LRuntimeError> {
    env.get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .dump_trace(None)
        .await;

    Ok(())
}
