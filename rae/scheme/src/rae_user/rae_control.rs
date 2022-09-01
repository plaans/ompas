use crate::rae_user::{CtxRaeUser, MOD_RAE_USER};
use ompas_rae_core::{run, TOKIO_CHANNEL_SIZE};
use ompas_rae_language::*;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::job::{Job, JobType};
use ompas_rae_structs::monitor::task_check_wait_for;
use ompas_rae_structs::options::*;
use ompas_rae_structs::options::{Planner, RAEOptions, SelectMode};
use sompas_macros::*;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_utils::task_handler::{subscribe_new_task, EndSignal};
use std::mem;
use std::time::Duration;
use tokio::sync::{broadcast, mpsc};

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
        run(platform, domain, interface, env, rx, &options).await;
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
            ctx.interface.mutexes.clear().await;
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
pub async fn configure_select(env: &LEnv, m: String) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    let select_mode = match m.as_str() {
        GREEDY => SelectMode::Greedy,
        PLANNING | ARIES => SelectMode::Planning(Planner::Aries, false),
        ARIES_OPT => SelectMode::Planning(Planner::Aries, true),
        UPOM => SelectMode::Planning(Planner::UPOM, false),
        RAE_PLAN => SelectMode::Planning(Planner::RAEPlan(Default::default()), false),
        HEURISTIC => SelectMode::Heuristic,
        LEARNING => SelectMode::Learning,
        _ => {
            return Err(lruntimeerror!(
                RAE_CONFIGURE_SELECT,
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
