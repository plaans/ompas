use crate::monitor::{CtxRaeUser, MOD_RAE_USER, TOKIO_CHANNEL_SIZE};
use crate::rae;
use ompas_rae_language::*;
use ompas_rae_language::{RAE_GET_AGENDA, RAE_GET_ENV, RAE_GET_STATE};
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::job::{Job, JobType};
use ompas_rae_structs::monitor::task_check_wait_for;
use ompas_rae_structs::rae_options::RAEOptions;
use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::task_state::*;
use ompas_rae_structs::state::task_status::TaskStatus;
use ompas_rae_structs::state::task_status::*;
use ompas_rae_structs::state::world_state::*;
use sompas_macros::*;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_type};
use sompas_utils::task_handler::{subscribe_new_task, EndSignal};
use std::mem;
use std::time::Duration;
use tokio::sync::{broadcast, mpsc};

/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
#[async_scheme_fn]
pub async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
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
                            RAE_GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                            )
                        ))
                    }
                }
            } else {
                return Err(wrong_type!(RAE_GET_STATE, &args[0], KindLValue::Symbol));
            }
        }
        _ => {
            return Err(LRuntimeError::wrong_number_of_args(
                RAE_GET_STATE,
                args,
                0..1,
            ))
        }
    };
    let state = ctx.interface.state.get_state(_type).await;
    Ok(state.into_map())
}

#[async_scheme_fn]
pub async fn get_config_platform(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.get_options()
        .await
        .get_platform_config()
        .unwrap_or_else(|| String::from("no options"))
}

#[async_scheme_fn]
pub async fn get_select(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.get_options().await.get_select_mode().to_string()
}
#[async_scheme_fn]
pub async fn get_task_network(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.interface.agenda.format_task_network().await
}

#[async_scheme_fn]
pub async fn get_type_hierarchy(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.rae_domain.read().await.types.format_hierarchy()
}

#[async_scheme_fn]
pub async fn get_agenda(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut task_filter = TaskFilter::default();

    for arg in args {
        match arg.to_string().as_str() {
            ABSTRACT_TASK => task_filter.task_type = Some(TaskType::AbstractTask),
            ACTION => task_filter.task_type = Some(TaskType::Action),
            STATUS_PENDING => task_filter.status = Some(TaskStatus::Pending),
            STATUS_DONE => task_filter.status = Some(TaskStatus::Done),
            STATUS_FAILURE => task_filter.status = Some(TaskStatus::Failure),
            STATUS_RUNNING => task_filter.status = Some(TaskStatus::Running),
            str => {
                return Err(lruntimeerror!(
                    RAE_GET_AGENDA,
                    format!(
                        "{} is not a valid filter option, expecting ({}, {}, {}, {}, {}, {})",
                        str,
                        ABSTRACT_TASK,
                        ACTION,
                        STATUS_PENDING,
                        STATUS_RUNNING,
                        STATUS_DONE,
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
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.interface.resources.get_debug().await.into())
}

#[async_scheme_fn]
pub async fn get_monitors(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.interface.monitors.get_debug().await.into())
}

///Get the methods of a given task
#[async_scheme_fn]
pub async fn get_methods(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.rae_domain.read().await.get_list_methods())
}

///Get the list of actions in the environment
#[async_scheme_fn]
pub async fn get_actions(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.rae_domain.read().await.get_list_actions())
}

///Get the list of tasks in the environment
#[async_scheme_fn]
pub async fn get_tasks(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.rae_domain.read().await.get_list_tasks())
}

///Get the list of state functions in the environment
#[async_scheme_fn]
pub async fn get_state_function(env: &LEnv) -> LValue {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();
    ctx.rae_domain.read().await.get_list_state_functions()
}

/// Returns the whole RAE environment if no arg et the entry corresponding to the symbol passed in args.
#[async_scheme_fn]
pub async fn get_env(env: &LEnv, args: &[LValue]) -> LResult {
    let key = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(key) = args[0].clone() {
                Some(key)
            } else {
                return Err(wrong_type!(RAE_GET_ENV, &args[0], KindLValue::Symbol));
            }
        }
        _ => return Err(LRuntimeError::wrong_number_of_args(RAE_GET_ENV, args, 0..1)),
    };

    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    match key {
        None => Ok(ctx.rae_domain.read().await.to_string().into()),
        Some(key) => Ok(ctx
            .rae_domain
            .read()
            .await
            .get_element_description(key.as_ref())
            .into()),
    }
}

#[async_scheme_fn]
pub async fn get_stats(env: &LEnv) -> LValue {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.interface.agenda.get_stats().await
}

#[async_scheme_fn]
pub async fn export_stats(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let file = if args.len() == 1 {
        Some(args[0].to_string())
    } else {
        None
    };
    ctx.interface.agenda.export_to_csv(None, file).await;
    Ok(LValue::Nil)
}

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
pub async fn launch(env: &LEnv) -> &str {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();
    let mut tasks_to_execute: Vec<Job> = vec![];
    mem::swap(
        &mut *ctx.tasks_to_execute.write().await,
        &mut tasks_to_execute,
    );

    let options = ctx.get_options().await.clone();

    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    //let (tx_stop, rx_stop) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let (killer, killed) = broadcast::channel(TOKIO_CHANNEL_SIZE);

    *ctx.interface.command_tx.write().await = Some(tx);
    //*ctx.interface.stop_tx.write().await = Some(tx_stop);
    *ctx.interface.killer.write().await = Some(killer.clone());
    let interface = ctx.interface.clone();
    let platform = ctx.platform.clone();

    if let Some(platform) = &platform {
        platform
            .get_ref()
            .write()
            .await
            .init(interface.clone())
            .await;
    }

    let domain: RAEDomain = ctx.rae_domain.read().await.clone();
    let env = ctx.get_exec_env().await;

    let state = ctx.interface.state.clone();
    let receiver_event_update_state = state.subscribe_on_update().await;
    let env_clone = env.clone();
    let monitors = interface.monitors.clone();
    tokio::spawn(async move {
        task_check_wait_for(receiver_event_update_state, killed, monitors, env_clone).await
    });

    tokio::spawn(async move {
        rae(platform, domain, interface, env, rx, &options).await;
    });

    if ctx.interface.log.display {
        ompas_rae_log::display_logger(killer.subscribe(), ctx.interface.log.path.clone());
    }

    tokio::spawn(async move { monitor_rae(killer).await });

    for t in tasks_to_execute {
        ctx.interface
            .command_tx
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

pub async fn monitor_rae(killer: broadcast::Sender<EndSignal>) {
    let mut recv_general = subscribe_new_task();
    let mut killed = killer.subscribe();

    tokio::select! {
        _ = recv_general.recv() => {
            println!("stop rae from general");
            killer
        .send(true)
        .expect("monitor_rae: error sending kill message");
            }
        _ = killed.recv() => {
            println!("stop rae from inside")
        }
    }
}

#[async_scheme_fn]
pub async fn stop(env: &LEnv) {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    match ctx.interface.get_killer().await {
        None => {}
        Some(killer) => {
            killer.send(true).expect("could not broadcast end signal");
            tokio::time::sleep(Duration::from_secs(1)).await; //hardcoded moment to wait for all process to be killed.
            *ctx.interface.command_tx.write().await = None;
            *ctx.interface.killer.write().await = None;
            ctx.interface.state.clear().await;
            ctx.interface.agenda.clear().await;
            ctx.interface.resources.clear().await;
            ctx.interface.monitors.clear().await;
        }
    }
}

#[async_scheme_fn]
pub async fn configure_platform(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_CONFIGURE_PLATFORM,
            args,
            1..usize::MAX,
        ));
    }
    let mut string = String::default();
    for arg in args {
        string.push_str(format!("{} ", arg).as_str())
    }

    let rae_options = RAEOptions::new_with_platform_config(Default::default(), Some(string));
    ctx.set_options(rae_options).await;
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn set_select(env: &LEnv, m: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    let select_mode = match m.as_str() {
        GREEDY => SelectMode::Greedy,
        PLANNING | ARIES => SelectMode::Planning(Planner::Aries, false),
        ARIES_OPT => SelectMode::Planning(Planner::Aries, true),
        UPOM => SelectMode::Planning(Planner::UPOM, false),
        RAE_PLAN => SelectMode::Planning(Planner::RAEPlan(Default::default()), false),
        C_CHOICE => SelectMode::Planning(Planner::CChoice, false),
        HEURISTIC => SelectMode::Heuristic,
        LEARNING => SelectMode::Learning,
        _ => {
            return Err(lruntimeerror!(
                RAE_SET_SELECT,
                format!(
                    "Select mode is either {}, {}, {} or {}.",
                    GREEDY, PLANNING, HEURISTIC, LEARNING
                )
            ))
        }
    };

    ctx.set_select_mode(select_mode).await;
    Ok(())
}

/// Sends via a channel a task to execute.
#[async_scheme_fn]
pub async fn trigger_task(env: &LEnv, args: &[LValue]) -> Result<LAsyncHandler, LRuntimeError> {
    let env = env.clone();

    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();
    let (tx, mut rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let job = Job::new(tx, args.into(), JobType::Task);

    match ctx.interface.get_sender().await {
        None => Err(LRuntimeError::new(RAE_TRIGGER_TASK, "no sender to rae")),
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

    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let (tx, _) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let job = Job::new(tx, args.into(), JobType::Task);

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
