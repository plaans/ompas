use crate::error::RaeExecError;
use futures::FutureExt;
use ompas_middleware::LogLevel;
use ompas_middleware::ProcessInterface;
use ompas_rae_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::internal_state::OMPASInternalState;
use ompas_rae_structs::rae_command::RAECommand;
use ompas_rae_structs::rae_options::OMPASOptions;
use sompas_core::{eval, eval_init};
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use std::ops::Deref;
use tokio::sync::mpsc::Receiver;

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
    let mut process =
        ProcessInterface::new(PROCESS_MAIN_RAE, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let log = interface.log.clone();

    eval_init(&mut env).await;

    let mut killers = vec![];

    loop {
        tokio::select! {
            command = command_rx.recv() => {
                match command {
                    None => break,
                    Some(RAECommand::Job(job)) => {
                        log.info("new job received!").await;

                        let mut new_env = env.clone();

                        let job_lvalue = job.core;
                        log.info(format!("new triggered task: {}", job_lvalue)).await;
                        //info!("LValue to be evaluated: {}", job_lvalue);

                        let (tx, rx) = new_interruption_handler();
                        killers.push(tx.clone());
                        let log2 = log.clone();
                        let future = (Box::pin(async move {
                            let result = eval(&job_lvalue, &mut new_env, Some(rx)).await;
                            match &result {
                                Ok(lv) => log2.info(format!(
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
                                )).await,
                                Err(e) => log2.error(format!("Error in asynchronous task: {}", e)).await,
                            }

                            result
                        }) as FutureResult)
                            .shared();

                        let f2 = future.clone();


                        tokio::spawn(f2);


                        match job.sender
                            .try_send(LAsyncHandle::new(future, tx)) {
                            Ok(_) =>{}
                            Err(e) => {
                                log.error(format!("{}", e.to_string())).await;
                            }
                        }
                    }
                }
            }
            _ = process.recv() => {
                process.log("Main loop of rae ended", LogLevel::Info).await;

                for k in &mut killers {
                    k.interrupt().await;
                }
                break ;//process.die().await;
            }
        }
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        //Note: The whole block could be in an async block
    }
}
