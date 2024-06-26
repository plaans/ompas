use crate::ompas::interface::continuous_planning_mode::ContinuousPlanningMode;
use crate::ompas::interface::job::{Job, JobType};
use crate::ompas::interface::rae_command::OMPASJob;
use crate::ompas::interface::select_mode::{AriesConfig, Planner, SelectMode};
use crate::ompas::interface::stat::BenchStat;
use crate::ompas::interface::trigger_collection::{JobCollection, JobHandle, PendingJob, Response};
use crate::ompas::manager::acting::filter::ProcessFilter;
use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::interval::Interval;
use crate::ompas::manager::acting::ActingManager;
use crate::ompas::manager::deliberation::MAX_REACTIVITY;
use crate::ompas::manager::event::{run_event_checker, run_fluent_checker};
use crate::ompas::manager::platform::platform_config::PlatformConfig;
use crate::ompas::manager::platform::PlatformManager;
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::state_update_manager::StateRule;
use crate::ompas::manager::state::StateType;
use crate::ompas::rae;
use crate::ompas::scheme::exec::ModExec;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::ompas::scheme::monitor::ModMonitor;
use crate::planning::planner::solver::PMetric;
use ompas_language::continuous_planning::*;
use ompas_language::exec::state::{DYNAMIC, INNER_DYNAMIC, INNER_STATIC, INSTANCE, STATIC};
use ompas_language::monitor::control::*;
use ompas_language::monitor::model::MOD_MODEL;
use ompas_language::output::{JSON_FORMAT, OMPAS_STATS, YAML_FORMAT};
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_STOP_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_language::select::*;
use ompas_language::supervisor::*;
use ompas_middleware::logger::LogClient;
use ompas_middleware::{Master, ProcessInterface};
use sompas_core::{eval_init, get_root_env};
use sompas_macros::*;
use sompas_modules::io::LogOutput;
use sompas_modules::ModExtendedStd;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, string, wrong_type};
use std::fmt::Write as OtherWrite;
use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::{mpsc, RwLock};

pub struct ModControl {
    pub acting_manager: ActingManager,
    pub log: LogClient,
    pub task_stream: Arc<RwLock<Option<tokio::sync::mpsc::UnboundedSender<OMPASJob>>>>,
    pub(crate) platform: PlatformManager,
    pub(crate) jobs: JobCollection,
    pub empty_env: Arc<LEnv>,
}

impl ModControl {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            acting_manager: monitor.acting_manager.clone(),
            log: monitor.log.clone(),
            task_stream: monitor.task_stream.clone(),
            platform: monitor.platform.clone(),
            jobs: Default::default(),
            empty_env: monitor.empty_env.clone(),
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

        let mut mod_extended_std = ModExtendedStd::default();
        mod_extended_std.set_log_output(LogOutput::Log(self.log.clone()));

        env.import_module(mod_extended_std, WithoutPrefix);
        env.import_module(ModExec::new(self).await, WithoutPrefix);
        eval_init(&mut env).await;

        let domain_exec_symbols: LEnvSymbols =
            self.acting_manager.domain_manager.get_exec_env().await;

        env.set_new_top_symbols(domain_exec_symbols);

        env
    }

    pub async fn get_sender(&self) -> Option<tokio::sync::mpsc::UnboundedSender<OMPASJob>> {
        self.task_stream.read().await.clone()
    }

    pub async fn reboot(&self) {
        self.acting_manager.clear().await;
        self.jobs.clear().await;
        *self.task_stream.write().await = None;
    }
}

impl From<ModControl> for LModule {
    fn from(m: ModControl) -> Self {
        let mut module = LModule::new(m, MOD_CONTROL, DOC_MOD_CONTROL);
        module.add_async_fn(START, start, DOC_START, false);
        // module.add_async_fn(
        //     START_WITH_PLANNER,
        //     start_with_planner,
        //     DOC_START_WITH_PLANNER,
        //     false,
        // );
        module.add_async_fn(STOP, stop, DOC_STOP, false);
        module.add_async_fn(
            __DEBUG_OMPAS__,
            __debug_ompas__,
            DOC___DEBUG_OMPAS___,
            false,
        );
        module.add_async_fn(
            EXEC_TASK,
            exec_task,
            (DOC_EXEC_TASK, DOC_EXEC_TASK_VERBOSE),
            false,
        );
        module.add_async_fn(_WAIT_TASK, _wait_task, DOC__WAIT_TASK, false);
        module.add_lambda(WAIT_TASK, LAMBDA_WAIT_TASK, DOC_WAIT_TASK);
        module.add_async_fn(GET_TASK_ID, get_task_id, DOC_GET_TASK_ID, false);
        module.add_async_fn(CANCEL_TASK, cancel_task, DOC_CANCEL_TASK, false);

        module.add_async_fn(EXEC_COMMAND, exec_command, DOC_EXEC_COMMAND, false);

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
        module.add_async_fn(
            SET_CONTINUOUS_PLANNING,
            set_continuous_planning,
            DOC_SET_CONTINUOUS_PLANNING,
            false,
        );
        module.add_async_fn(
            SET_PLANNER_REACTIVITY,
            set_planner_reactivity,
            DOC_SET_PLANNER_REACTIVITY,
            false,
        );
        module.add_async_fn(
            GET_PLANNER_REACTIVITY,
            get_planner_reactivity,
            DOC_GET_PLANNER_REACTIVITY,
            false,
        );

        module.add_async_fn(
            SET_DELIBERATION_REACTIVITY,
            set_deliberation_reactivity,
            DOC_SET_DELIBERATION_REACTIVITY,
            false,
        );
        module.add_async_fn(
            GET_DELIBERATION_REACTIVITY,
            get_deliberation_reactivity,
            DOC_GET_DELIBERATION_REACTIVITY,
            false,
        );
        module.add_async_fn(
            SET_PRE_COMPUTE_MODELS,
            set_pre_compute_models,
            DOC_SET_PRE_COMPUTE_MODELS,
            false,
        );
        module.add_async_fn(
            GET_PRE_COMPUTE_MODELS,
            get_pre_compute_models,
            DOC_GET_PRE_COMPUTE_MODELS,
            false,
        );

        module.add_async_fn(GET_DOMAIN, get_domain, DOC_GET_SELECT, false);
        module.add_async_fn(GET_STATE, get_state, DOC_GET_STATE, false);
        module.add_async_fn(
            GET_CONFIG_PLATFORM,
            get_config_platform,
            DOC_GET_CONFIG_PLATFORM,
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
        module.add_async_fn(GET_TASK_LIST, get_tasks, DOC_GET_TASK_LIST, false);
        module.add_async_fn(GET_METHOD_LIST, get_methods, DOC_GET_METHOD_LIST, false);
        module.add_async_fn(
            GET_STATE_FUNCTIONS,
            get_state_functions,
            DOC_GET_STATE_FUNCTIONS,
            false,
        );
        module.add_async_fn(GET_STATS, get_stats, DOC_GET_STATS, false);
        module.add_async_fn(EXPORT_STATS, export_stats, DOC_EXPORT_STATS, false);
        module.add_async_fn(EXPORT_TO_CSV, export_to_csv, DOC_EXPORT_TO_CSV, false);

        module.add_async_fn(
            DUMP_ACTING_TREE,
            dump_acting_tree,
            DOC_DUMP_ACTING_TREE,
            false,
        );
        module.add_async_fn(
            START_ACTING_TREE_DISPLAY,
            start_acting_tree_display,
            DOC_START_ACTING_TREE_DISPLAY,
            false,
        );
        module.add_async_fn(
            STOP_ACTING_TREE_DISPLAY,
            stop_acting_tree_display,
            DOC_STOP_ACTING_TREE_DISPLAY,
            false,
        );

        module.add_async_fn(EXPORT_REPORT, export_report, DOC_EXPORT_REPORT, false);
        module.add_async_fn(WAIT_END_ALL, wait_end_all, DOC_WAIT_END_ALL, false);
        module.add_async_fn(BENCH, bench, DOC_BENCH, false);

        // Macros
        module.add_macro(DEBUG_OMPAS, MACRO_DEBUG_OMPAS, DOC_DEBUG_OMPAS);

        module
    }
}

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
async fn start(env: &LEnv) -> Result<String, LRuntimeError> {
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    let plan_env: LEnv = ctx.get_plan_env().await;
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let acting_manager = ctx.acting_manager.clone();
    acting_manager.set_plan_env(plan_env.clone()).await;

    let pendings = ctx.jobs.move_pendings().await;

    let (tx, rx) = mpsc::unbounded_channel();

    let env = ctx.get_exec_env().await;
    let log = ctx.log.clone();
    let log_2 = log.clone();
    let acting_manager_2 = acting_manager.clone();

    let receiver_event_update_state = acting_manager
        .state_manager
        .new_subscriber(StateRule::All)
        .await;
    let event_manager = acting_manager.event_manager.clone();

    *ctx.task_stream.write().await = Some(tx.clone());

    tokio::spawn(
        async move { run_fluent_checker(receiver_event_update_state, event_manager).await },
    );

    let event_manager = acting_manager.event_manager.clone();
    let receiver_event_update_state = acting_manager
        .state_manager
        .new_subscriber(StateRule::All)
        .await;
    let env_clone = env.clone();
    let events = acting_manager.domain_manager.get_events().await;
    tokio::spawn(async move {
        run_event_checker(
            receiver_event_update_state,
            event_manager,
            tx,
            events,
            env_clone,
        )
        .await
    });

    tokio::spawn(async move {
        rae(acting_manager_2, log_2, env, rx).await;
    });

    let start_result: String = ctx.platform.start(Default::default()).await?;

    let continuous_planning_mode = ctx
        .acting_manager
        .deliberation_manager
        .get_continuous_planning_mode()
        .await;

    log.info(format!(
        "Started OMPAS with config: {}",
        match &continuous_planning_mode {
            ContinuousPlanningMode::None => "planner = none",
            ContinuousPlanningMode::Satisfactory => "planner = satisfactory",
            ContinuousPlanningMode::Optimality(_) => "planner = optimality",
        }
    ));
    let sender = ctx.get_sender().await.unwrap();

    {
        let init = acting_manager.domain_manager.get_init().await;
        log.info(format!("Evaluating init: {}", init));
        let (tx, mut rx) = mpsc::unbounded_channel();
        sender
            .send(Job::new_init(tx, init).into())
            .expect("could not send job to rae");
        if let Response::Handle(handle) = rx.recv().await.unwrap()? {
            let _ = handle.get_future().await;
        } else {
            unreachable!("OMPAS's response to init should be a future");
        }
    }

    let start = SystemTime::now();
    acting_manager
        .domain_manager
        .init_planning_domain(
            &plan_env,
            acting_manager.state_manager.get_snapshot().await,
            &acting_manager.st,
            ctx.acting_manager
                .deliberation_manager
                .get_pre_compute_models()
                .await,
        )
        .await;
    log.info(format!(
        "Pre generation of acting models took {:.3} ms",
        start.elapsed().unwrap().as_secs_f64() * 1000.0
    ));

    match &continuous_planning_mode {
        ContinuousPlanningMode::None => {}
        _ => {
            let opt = match continuous_planning_mode {
                ContinuousPlanningMode::None => {
                    unreachable!()
                }
                ContinuousPlanningMode::Satisfactory => None,
                ContinuousPlanningMode::Optimality(pmetric) => Some(pmetric),
            };

            acting_manager.start_planner_manager(plan_env, opt).await;
        }
    }

    let (tx, rx) = tokio::sync::oneshot::channel();
    sender.send(OMPASJob::StartHandlingEvent(tx)).unwrap();
    let _ = rx.await;
    tokio::time::sleep(Duration::from_millis(100)).await;

    for PendingJob { id, r#type, lvalue } in pendings {
        log.info(format!("Sending pending job {}", lvalue));
        let (tx, mut rx) = mpsc::unbounded_channel();
        let job = Job::new(tx, r#type, lvalue);
        let sender = sender.clone();
        tokio::spawn(async move {
            sender.send(job.into()).expect("could not send job to rae");
        });
        let trigger: Response = rx.recv().await.unwrap()?;
        if let Response::Process(process) = trigger {
            ctx.jobs.set_task_process(&id, process).await;
        } else {
            unreachable!("{EXEC_TASK} should receive a TaskProcess")
        }
    }

    Ok(format!("OMPAS launched successfully. {}", start_result))
}

/// Launch main loop of rae in an other asynchronous task.
/*#[async_scheme_fn]
pub async fn start_with_planner(env: &LEnv, opt: bool) -> Result<String, LRuntimeError> {
    _start(env, Some(opt)).await
}*/

#[async_scheme_fn]
pub async fn stop(env: &LEnv) {
    let process: ProcessInterface =
        ProcessInterface::new(PROCESS_STOP_OMPAS, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    process.kill(PROCESS_TOPIC_OMPAS);
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
    let (tx, mut rx) = mpsc::unbounded_channel();
    let job = Job::new_debug(tx, arg);

    match ctx.get_sender().await {
        None => Err(LRuntimeError::new(__DEBUG_OMPAS__, "no sender to rae")),
        Some(sender) => {
            tokio::spawn(async move {
                sender.send(job.into()).expect("could not send job to rae");
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
pub async fn exec_task(env: &LEnv, args: &[LValue]) -> Result<usize, LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let task = args[0].to_string();
    if !ctx.acting_manager.domain_manager.is_task(&task).await {
        return Err(LRuntimeError::new(
            EXEC_TASK,
            format!("{} is not a task.", task),
        ));
    }

    match ctx.get_sender().await {
        None => Ok(ctx.jobs.add_pending_job(JobType::Task, args.into()).await),
        Some(sender) => {
            let (tx, mut rx) = mpsc::unbounded_channel();
            let job = Job::new_task(tx, args.into());

            tokio::spawn(async move {
                sender.send(job.into()).expect("could not send job to rae");
            });
            let trigger: Response = rx.recv().await.unwrap()?;
            if let Response::Process(process) = trigger {
                Ok(ctx.jobs.add_process(process).await)
            } else {
                unreachable!("{} should receive a TaskTrigger struct.", EXEC_TASK)
            }
        }
    }
}

#[async_scheme_fn]
pub async fn _wait_task(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let trigger: Option<JobHandle> = ctx.jobs.get_job(task_id).await;
    match trigger {
        Some(JobHandle::Process(process)) => Ok(LValue::Handle(process.get_handle())),
        Some(JobHandle::Pending(_)) => Ok(string!(format!(
            "{task_id} is a pending task because ompas has been started yet."
        ))),
        None => Err(LRuntimeError::new(
            WAIT_TASK,
            format!("{} is not the id of a triggered task", task_id),
        )),
    }
}

#[async_scheme_fn]
pub async fn get_task_id(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let trigger: Option<JobHandle> = ctx.jobs.get_job(task_id).await;
    match trigger {
        Some(JobHandle::Process(process)) => Ok(format!("{:?}", process.get_ref()).into()),
        Some(JobHandle::Pending(_)) => Ok(string!(format!(
            "{task_id} is a pending task because ompas has been started yet."
        ))),
        None => Err(LRuntimeError::new(
            WAIT_TASK,
            format!("{} is not the id of a triggered task", task_id),
        )),
    }
}

#[async_scheme_fn]
pub async fn cancel_task(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let trigger: Option<JobHandle> = ctx.jobs.get_job(task_id).await;
    match trigger {
        Some(JobHandle::Process(process)) => process.get_handle().interrupt().await,
        Some(JobHandle::Pending(_)) => Ok(string!(format!(
            "Cannot cancel {task_id} because it is still a pending task"
        ))),
        None => Err(LRuntimeError::new(
            WAIT_TASK,
            format!("{} is not the id of a triggered task", task_id),
        )),
    }
}

#[async_scheme_fn]
pub async fn exec_command(env: &LEnv, args: &[LValue]) -> Result<usize, LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let task = args[0].to_string();
    if !ctx.acting_manager.domain_manager.is_command(&task).await {
        return Err(LRuntimeError::new(
            EXEC_COMMAND,
            format!("{} is not a command.", task),
        ));
    }

    match ctx.get_sender().await {
        None => Ok(ctx
            .jobs
            .add_pending_job(JobType::Command, args.into())
            .await),
        Some(sender) => {
            let (tx, mut rx) = mpsc::unbounded_channel();
            let job = Job::new_event(tx, args.into());

            tokio::spawn(async move {
                sender.send(job.into()).expect("could not send job to rae");
            });
            let trigger: Response = rx.recv().await.unwrap()?;
            if let Response::Process(process) = trigger {
                Ok(ctx.jobs.add_process(process).await)
            } else {
                unreachable!("{} should receive a TaskTrigger struct.", EXEC_COMMAND)
            }
        }
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

    ctx.acting_manager
        .deliberation_manager
        .get_select_mode()
        .await
        .to_string()
}

#[async_scheme_fn]
pub async fn set_select(env: &LEnv, m: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let select_mode = match m.as_str() {
        RANDOM => SelectMode::Random,
        GREEDY => SelectMode::Greedy,
        COST => SelectMode::Cost,
        PLANNING | ARIES => SelectMode::Planning(Planner::Aries(AriesConfig::Satisfactory)),
        ARIES_OPT => {
            SelectMode::Planning(Planner::Aries(AriesConfig::Optimality(PMetric::Makespan)))
        }
        UPOM => SelectMode::Planning(Planner::UPOM(Default::default())),
        RAE_PLAN => SelectMode::Planning(Planner::RAEPlan(Default::default())),
        C_CHOICE => SelectMode::Planning(Planner::CChoice(Default::default())),
        _ => {
            return Err(lruntimeerror!(
                SET_SELECT,
                format!(
                    "Select mode is either {}, {}, or {}.",
                    GREEDY, RANDOM, PLANNING,
                )
            ))
        }
    };

    ctx.acting_manager
        .deliberation_manager
        .set_select_mode(select_mode)
        .await;
    Ok(())
}

#[async_scheme_fn]
pub async fn set_continuous_planning(env: &LEnv, m: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let continuous_planning_mode = match m.as_str() {
        NONE => ContinuousPlanningMode::None,
        SATISFACTORY => ContinuousPlanningMode::Satisfactory,
        OPTIMALITY | ARIES => ContinuousPlanningMode::Optimality(PMetric::Makespan),
        _ => {
            return Err(lruntimeerror!(
                SET_CONTINUOUS_PLANNING,
                format!(
                    "Continuous planning mode is either {}, {}, or {}.",
                    NONE, SATISFACTORY, OPTIMALITY
                )
            ))
        }
    };

    ctx.acting_manager
        .deliberation_manager
        .set_continuous_planning_mode(continuous_planning_mode)
        .await;
    Ok(())
}

#[async_scheme_fn]
pub async fn set_planner_reactivity(env: &LEnv, reactivity: LValue) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let planner_reactivity = match reactivity {
        LValue::Symbol(s) => {
            if s.as_str() == "inf" {
                f64::MAX
            } else {
                return Err(LRuntimeError::new(
                    "",
                    format!("Expected Symbol(\"inf\") or number, got {}", s),
                ));
            }
        }
        LValue::Number(n) => f64::from(&n),
        lv => {
            return Err(LRuntimeError::new(
                "",
                format!("Expected Symbol(\"inf\") or number, got {}", lv),
            ))
        }
    };

    ctx.acting_manager
        .deliberation_manager
        .set_planner_reactivity(planner_reactivity);
    Ok(())
}

#[async_scheme_fn]
pub async fn get_planner_reactivity(env: &LEnv) -> Result<LValue, LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let f = ctx
        .acting_manager
        .deliberation_manager
        .get_planner_reactivity();
    if f < MAX_REACTIVITY {
        Ok(f.into())
    } else {
        Ok("inf".into())
    }
}

#[async_scheme_fn]
pub async fn set_deliberation_reactivity(
    env: &LEnv,
    reactivity: LValue,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();

    let deliberation_reactivity = match reactivity {
        LValue::Symbol(s) => {
            if s.as_str() == "inf" {
                f64::MAX
            } else {
                return Err(LRuntimeError::new(
                    "",
                    format!("Expected Symbol(\"inf\") or number, got {}", s),
                ));
            }
        }
        LValue::Number(n) => f64::from(&n),
        lv => {
            return Err(LRuntimeError::new(
                "",
                format!("Expected Symbol(\"inf\") or number, got {}", lv),
            ))
        }
    };

    ctx.acting_manager
        .deliberation_manager
        .set_deliberation_reactivity(deliberation_reactivity);
    Ok(())
}

#[async_scheme_fn]
pub async fn get_deliberation_reactivity(env: &LEnv) -> Result<LValue, LRuntimeError> {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    let f = ctx
        .acting_manager
        .deliberation_manager
        .get_planner_reactivity();
    if f < MAX_REACTIVITY {
        Ok(f.into())
    } else {
        Ok("inf".into())
    }
}

#[async_scheme_fn]
pub async fn set_pre_compute_models(env: &LEnv, value: bool) {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    ctx.acting_manager
        .deliberation_manager
        .set_pre_compute_models(value)
        .await;
}

#[async_scheme_fn]
pub async fn get_pre_compute_models(env: &LEnv) -> bool {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    ctx.acting_manager
        .deliberation_manager
        .get_pre_compute_models()
        .await
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
    let state = ctx.acting_manager.state_manager.get_state(_type).await;
    Ok(state.into_map())
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
            TASK => task_filter.kind = Some(ActingProcessKind::Task),
            COMMAND => task_filter.kind = Some(ActingProcessKind::Command),
            ARBITRARY => task_filter.kind = Some(ActingProcessKind::Arbitrary),
            ACQUIRE => task_filter.kind = Some(ActingProcessKind::Acquire),
            METHOD => task_filter.kind = Some(ActingProcessKind::Method),
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
                        str, TASK, STATUS_PENDING, STATUS_RUNNING, STATUS_SUCCESS, STATUS_FAILURE
                    )
                ))
            }
        }
    }

    let string = ctx.acting_manager.format_processes(task_filter).await;
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
    Ok(ctx.acting_manager.event_manager.get_debug().await.into())
}

///Get the list of actions in the environment
#[async_scheme_fn]
pub async fn get_commands(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.domain_manager.get_list_commands().await)
}

///Get the list of tasks in the environment
#[async_scheme_fn]
pub async fn get_tasks(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.domain_manager.get_list_tasks().await)
}

///Get the methods of a given task
#[async_scheme_fn]
pub async fn get_methods(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    Ok(ctx.acting_manager.domain_manager.get_list_methods().await)
}

///Get the list of state functions in the environment
#[async_scheme_fn]
pub async fn get_state_functions(env: &LEnv) -> LValue {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL).unwrap();
    ctx.acting_manager
        .domain_manager
        .get_list_state_functions()
        .await
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
        None => Ok(ctx.acting_manager.domain_manager.format().await.into()),
        Some(key) => Ok(ctx
            .acting_manager
            .domain_manager
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
pub async fn export_to_csv(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let file = if args.len() == 1 {
        Some(args[0].to_string())
    } else {
        None
    };
    let content = ctx.acting_manager.export_to_csv().await;

    let mut path: PathBuf = Master::get_run_dir();
    path.push(OMPAS_STATS);

    fs::create_dir_all(&path).expect("could not create stats directory");

    path.push(match file {
        Some(f) => f.to_string(),
        None => format!("{}_{}.csv", OMPAS_STATS, Master::get_string_date()),
    });

    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)
        .expect("error creating stat file");

    if file.metadata().unwrap().len() == 0 {
        file.write_all(ActingManager::get_header_stat().as_bytes())
            .expect("could not write to stat file");
    }
    file.write_all(content.as_bytes())
        .unwrap_or_else(|e| panic!("could not write to {}: {}", path.to_str().unwrap(), e));

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn export_stats(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModControl>(MOD_CONTROL)?;
    let mut format = YAML_FORMAT;
    let mut file_name = None;
    match args.len() {
        0 => {}
        1 => {
            let arg: String = (&args[0]).try_into().map_err(|e: LRuntimeError| {
                e.chain("Expected either a format or a file name, e.g. \"stat.yml\"")
            })?;
            match arg.as_str() {
                YAML_FORMAT => {
                    format = YAML_FORMAT;
                }
                JSON_FORMAT => {
                    format = JSON_FORMAT;
                }
                str => {
                    if str.contains(YAML_FORMAT) {
                        format = YAML_FORMAT;
                        file_name = Some(arg);
                    } else if str.contains(JSON_FORMAT) {
                        format = JSON_FORMAT;
                        file_name = Some(arg);
                    } else {
                        return Err(LRuntimeError::new(
                            EXPORT_STATS,
                            "Expected either a format or a file name, e.g. \"stat.yml\"",
                        ));
                    }
                }
            }
        }
        _ => {
            return Err(LRuntimeError::new(
                EXPORT_STATS,
                format!("expected only one argument, got {}", LValue::from(args)),
            ))
        }
    }

    let mut path: PathBuf = Master::get_run_dir();
    path.push(OMPAS_STATS);

    fs::create_dir_all(&path).expect("could not create stats directory");

    path.push(match file_name {
        Some(f) => f.to_string(),
        None => format!("{}_{}.{}", OMPAS_STATS, Master::get_string_date(), format),
    });

    let stat = ctx.acting_manager.get_run_stat().await;

    let content = match format {
        YAML_FORMAT => serde_yaml::to_string(&stat).unwrap(),
        JSON_FORMAT => serde_json::to_string(&stat).unwrap(),
        _ => unreachable!(),
    };

    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open(&path)
        .expect("error creating stat file");

    file.write_all(content.as_bytes())
        .unwrap_or_else(|e| panic!("could not write to {}: {}", path.to_str().unwrap(), e));

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn dump_acting_tree(env: &LEnv) -> Result<(), LRuntimeError> {
    env.get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .dump_trace(Some({
            let mut path = Master::get_run_dir();
            path.push("traces");
            path
        }))
        .await;

    Ok(())
}

#[async_scheme_fn]
pub async fn start_acting_tree_display(env: &LEnv) -> Result<(), LRuntimeError> {
    env.get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .start_acting_tree_display()
        .await;

    Ok(())
}

#[async_scheme_fn]
pub async fn stop_acting_tree_display(env: &LEnv) -> Result<(), LRuntimeError> {
    env.get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .stop_acting_tree_display()
        .await;

    Ok(())
}

pub async fn _export_report(
    env: &LEnv,
    file_name: Option<String>,
    format: &str,
    bench_info: Option<(f64, f64)>,
) -> Result<(), LRuntimeError> {
    let acting_manager = &env.get_context::<ModControl>(MOD_CONTROL)?.acting_manager;

    let mut run_dir = Master::get_run_dir();
    run_dir.push("report");
    fs::create_dir_all(run_dir.clone()).unwrap();

    let state = acting_manager.state_manager.get_state(None).await;
    let mut state_str = "State:\n".to_string();
    for (k, v) in state.inner {
        writeln!(state_str, "- {} = {}", k, v.value).unwrap();
    }

    let mut state_file_path = run_dir.clone();
    state_file_path.push(format!(
        "{}.txt",
        match &file_name {
            Some(f) => f.as_str(),
            None => "state",
        }
    ));
    let mut acting_tree_file = OpenOptions::new()
        .truncate(true)
        .write(true)
        .create(true)
        .open(state_file_path)
        .unwrap();
    acting_tree_file.write_all(state_str.as_bytes()).unwrap();

    let acting_tree = acting_manager.acting_tree_as_dot().await;

    let mut acting_tree_file_path = run_dir.clone();
    acting_tree_file_path.push(format!(
        "{}.dot",
        match &file_name {
            Some(f) => f.as_str(),
            None => "acting_tree",
        }
    ));
    let mut acting_tree_file = OpenOptions::new()
        .truncate(true)
        .write(true)
        .create(true)
        .open(acting_tree_file_path)
        .unwrap();
    acting_tree_file.write_all(acting_tree.as_bytes()).unwrap();

    let mut stats = acting_manager.get_run_stat().await;
    match bench_info {
        None => {}
        Some((min, max)) => {
            let bench_stat = BenchStat {
                min_time: Interval::new(0.0, Some(min)).duration(),
                max_time: Interval::new(0.0, Some(max)).duration(),
            };
            stats.add_stat(bench_stat)
        }
    }

    let mut stats_file_path = run_dir.clone();
    stats_file_path.push(format!(
        "{}.{}",
        match &file_name {
            Some(f) => f.as_str(),
            None => "stats",
        },
        format
    ));
    let mut stats_file = OpenOptions::new()
        .truncate(true)
        .write(true)
        .create(true)
        .open(stats_file_path)
        .unwrap();
    stats_file
        .write_all(
            match format {
                YAML_FORMAT => serde_yaml::to_string(&stats).unwrap(),
                JSON_FORMAT => serde_json::to_string(&stats).unwrap(),
                _ => unreachable!(),
            }
            .as_bytes(),
        )
        .unwrap();

    Ok(())
}

fn extract_file_name(args: &[LValue]) -> Result<(Option<String>, &'static str), LRuntimeError> {
    let mut format = JSON_FORMAT;
    let mut file_name = None;
    match args.len() {
        0 => {}
        1 => {
            let arg: String = (&args[0]).try_into().map_err(|e: LRuntimeError| {
                e.chain("Expected either a format or a file name, e.g. \"stat.yml\"")
            })?;
            match arg.as_str() {
                YAML_FORMAT => {
                    format = YAML_FORMAT;
                }
                JSON_FORMAT => {
                    format = JSON_FORMAT;
                }
                str => {
                    if str.contains(YAML_FORMAT) {
                        format = YAML_FORMAT;
                        file_name = Some(
                            arg.replace(['(', ')'], "")
                                .replace(' ', ".")
                                .replace(&format!(".{}", YAML_FORMAT), ""),
                        );
                    } else if str.contains(JSON_FORMAT) {
                        format = JSON_FORMAT;
                        file_name = Some(
                            arg.replace(['(', ')'], "")
                                .replace(' ', ".")
                                .replace(&format!(".{}", JSON_FORMAT), ""),
                        );
                    } else {
                        let name = arg.replace(['(', ')'], "").replace(' ', ".");
                        file_name = Some(name);
                    }
                }
            }
        }
        _ => {
            return Err(LRuntimeError::new(
                EXPORT_STATS,
                format!("expected only one argument, got {}", LValue::from(args)),
            ))
        }
    }

    Ok((file_name, format))
}

#[async_scheme_fn]
pub async fn export_report(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let (file_name, format) = extract_file_name(args)?;

    _export_report(env, file_name, format, None).await?;

    Ok(())
}

#[async_scheme_fn]
pub async fn wait_end_all(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let timeout: Option<i64> = if args.len() == 1 {
        Some(args[0].clone().try_into()?)
    } else {
        None
    };

    let acting_manager = &env.get_context::<ModControl>(MOD_CONTROL)?.acting_manager;

    let high_level_tasks = acting_manager.get_all_high_level_tasks().await;

    let mut subscriber: Vec<_> = vec![];

    for task in high_level_tasks {
        subscriber.push(acting_manager.subscribe(&task).await);
    }

    let waiter = tokio::spawn(async move {
        for mut sub in subscriber {
            'loop_sub: loop {
                if sub.borrow().is_terminated() {
                    break 'loop_sub;
                }
                let _ = sub.changed().await;
            }
        }
    });

    match timeout {
        None => {
            let _ = waiter.await;
        }
        Some(timeout) => {
            tokio::select! {
                _ = waiter => {

                }
                _ = tokio::time::sleep(Duration::from_secs(timeout as u64)) => {

                }
            }
        }
    }

    Ok(())
}

#[async_scheme_fn]
pub async fn bench(env: &LEnv, min: f64, max: f64, export: String) -> Result<(), LRuntimeError> {
    tokio::time::sleep(Duration::from_secs_f64(min)).await;
    wait_end_all(env, &[(max - min).into()]).await?;
    let (file_name, format) = extract_file_name(&[export.into()])?;
    _export_report(env, file_name, format, Some((min, max))).await?;
    Ok(())
}
