use crate::rae_user::{CtxRaeUser, MOD_RAE_USER};
use ompas_rae_core::monitor::task_check_wait_for;
use ompas_rae_core::{run, TOKIO_CHANNEL_SIZE};
use ompas_rae_language::*;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::job::{Job, JobType};
use ompas_rae_structs::options::*;
use ompas_rae_structs::options::{Planner, RAEOptions, SelectMode};
use sompas_macros::*;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_utils::task_handler::{subscribe_new_task, EndSignal};
use tokio::sync::{broadcast, mpsc};

/// Launch main loop of rae in an other asynchronous task.
#[async_scheme_fn]
pub async fn launch(env: &LEnv) -> &str {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    let options = ctx.get_options().await.clone();

    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let (tx_stop, rx_stop) = mpsc::channel(TOKIO_CHANNEL_SIZE);
    let (tx_killer, killer) = broadcast::channel(TOKIO_CHANNEL_SIZE);

    *ctx.interface.command_tx.write().await = Some(tx);
    *ctx.interface.stop_tx.write().await = Some(tx_stop);
    *ctx.interface.killer.write().await = Some(tx_killer.clone());
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
    tokio::spawn(async move {
        task_check_wait_for(receiver_event_update_state, killer, env_clone).await
    });

    tokio::spawn(async move {
        run(platform, domain, interface, env, rx, &options).await;
    });
    tokio::spawn(async move { monitor_rae(rx_stop, tx_killer).await });
    "rae launched succesfully"
}

pub async fn monitor_rae(
    mut recv_stop: mpsc::Receiver<EndSignal>,
    sender_kill: broadcast::Sender<EndSignal>,
) {
    let mut recv_general = subscribe_new_task();

    tokio::select! {
        _ = recv_general.recv() => {
            println!("stop rae from general")
        }
        _ = recv_stop.recv() => {
            println!("stop rae from repl")
        }
    }

    sender_kill.send(true).expect("error sending kill message");
}

#[async_scheme_fn]
pub async fn stop(env: &LEnv) {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    match ctx.interface.get_stop().await {
        None => {}
        Some(command_tx) => {
            command_tx
                .send(true)
                .await
                .expect("could not broadcast end signal");
            *ctx.interface.command_tx.write().await = None;
            *ctx.interface.stop_tx.write().await = None;
            ctx.interface.state.clear().await;
            ctx.interface.agenda.clear().await;
            *ctx.interface.killer.write().await = None;
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
