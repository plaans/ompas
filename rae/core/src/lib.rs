use crate::contexts::ctx_planning::CtxPlanning;
use crate::error::RaeExecError;
use futures::FutureExt;
use map_macro::set;
use ompas_middleware::LogLevel;
use ompas_middleware::ProcessInterface;
use ompas_rae_language::PROCESS_TOPIC_ACTING;
use ompas_rae_planning::aries::conversion::convert_domain_to_chronicle_hierarchy;
use ompas_rae_planning::aries::structs::{ConversionCollection, ConversionContext};
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::internal_state::OMPASInternalState;
use ompas_rae_structs::rae_command::RAECommand;
use ompas_rae_structs::rae_options::OMPASOptions;
use ompas_rae_structs::select_mode::{Planner, SelectMode};
use sompas_core::{eval, eval_init};
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lswitch::new_interruption_handler;
use std::time::Instant;
use tokio::sync::mpsc::Receiver;

pub mod contexts;
pub mod error;
pub mod exec;
pub mod monitor;

pub type ReactiveTriggerId = usize;

pub const TOKIO_CHANNEL_SIZE: usize = 100;

pub const PROCESS_MAIN_RAE: &str = "__PROCESS_MAIN_RAE__";

/// Main RAE Loop:
/// Receives Job to handle in separate tasks.
pub async fn rae(
    domain: RAEDomain,
    interface: OMPASInternalState,
    mut env: LEnv,
    mut command_rx: Receiver<RAECommand>,
    options: &OMPASOptions,
) {
    let mut process = ProcessInterface::new(PROCESS_MAIN_RAE, set! {PROCESS_TOPIC_ACTING}).await;
    //Ubuntu::
    /*let lvalue: LValue = match options.get_platform_config() {
        None => vec![RAE_LAUNCH_PLATFORM].into(),
        Some(string) => {
            info!("Platform config: {}", string);
            vec![RAE_LAUNCH_PLATFORM.to_string(), string].into()
        }
    };*/

    /*match &platform {
        None => info!("No platform defined"),
        Some(platform) => platform.start().await,
    }*/

    /*let result = eval(&lvalue, &mut env, None).await;

    match result {
        Ok(_) => {}
        Err(e) => error!("{}", e),
    }*/

    let mut select_mode = *options.get_select_mode();

    let cc: Option<ConversionCollection> =
        if matches!(select_mode, SelectMode::Planning(Planner::Aries(_))) {
            let instant = Instant::now();
            match convert_domain_to_chronicle_hierarchy(ConversionContext {
                domain: domain.clone(),
                env: env.clone(),
                state: interface.state.get_snapshot().await,
            }) {
                Ok(r) => {
                    info!(
                        "Conversion time: {:.3} ms",
                        instant.elapsed().as_micros() as f64 / 1000.0
                    );
                    Some(r)
                }
                Err(e) => {
                    select_mode = SelectMode::Greedy;
                    warn!("Cannot plan with the domain...{}", e);
                    None
                }
            }
        } else {
            None
        };

    env.import_module(
        CtxPlanning::new(cc, domain.clone(), env.clone(), select_mode),
        WithoutPrefix,
    );

    eval_init(&mut env).await;

    let mut killers = vec![];

    loop {
        tokio::select! {
            command = command_rx.recv() => {
                match command {
                    None => break,
                    Some(RAECommand::Job(job)) => {
                        info!("new job received!");

                        let mut new_env = env.clone();

                        let job_lvalue = job.core;
                        info!("new triggered task: {}", job_lvalue);
                        //info!("LValue to be evaluated: {}", job_lvalue);

                        let (tx, rx) = new_interruption_handler();
                        killers.push(tx.clone());

                        let future = (Box::pin(async move {
                            let result = eval(&job_lvalue, &mut new_env, Some(rx)).await;
                            match &result {
                                Ok(lv) => info!(
                                    "result of task {}: {}",
                                    job_lvalue,
                                    match lv {
                                        LValue::Err(e) => {
                                            match e.deref() {
                                                LValue::Number(LNumber::Int(i)) => {
                                                    format!("{:?}", RaeExecError::i64_as_err(*i))
                                                }
                                                lv => lv.to_string(),
                                            }
                                        }
                                        lv => lv.to_string(),
                                    }
                                ),
                                Err(e) => error!("Error in asynchronous task: {}", e),
                            }

                            result
                        }) as FutureResult)
                            .shared();

                        let f2 = future.clone();


                        tokio::spawn(f2);


                        match job.sender
                            .try_send(LAsyncHandler::new(future, tx)) {
                            Ok(_) =>{}
                            Err(e) => error!("{}", e.to_string())
                        }
                    }
                }
            }
            _ = process.recv() => {
                process.log("Main loop of rae ended", LogLevel::Info).await;

                for k in &mut killers {
                    k.interrupt().await;
                }
                break process.die().await;
            }
        }
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        //Note: The whole block could be in an async block
    }
}
