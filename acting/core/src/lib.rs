use crate::error::RaeExecError;
use crate::exec::task::ModTask;
use futures::FutureExt;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::logger::LogClient;
use ompas_middleware::LogLevel;
use ompas_middleware::ProcessInterface;
use ompas_structs::job::JobType;
use ompas_structs::rae_command::OMPASJob;
use ompas_structs::trigger_collection::{Response, TaskTrigger};
use sompas_core::{eval, parse};
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

pub const PROCESS_MAIN: &str = "__PROCESS_MAIN__";

/// Main RAE Loop:
/// Receives Job to handle in separate tasks.
pub async fn rae(
    //domain: RAEDomain,
    log: LogClient,
    env: LEnv,
    mut command_rx: Receiver<OMPASJob>,
) {
    let mut process =
        ProcessInterface::new(PROCESS_MAIN, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let mut killers = vec![];

    loop {
        tokio::select! {
            command = command_rx.recv() => {
                match command {
                    None => break,
                    Some(OMPASJob::Job(job)) => {
                        log.info("new job received!").await;

                        let mut new_env = env.clone();

                        let mut arc_task_id = None;

                        let job_type = job.r#type;
                        let job_expr = &job.expr;

                        match job_type {
                            JobType::Task => {
                                log.info(format!("new triggered method: {}", job_expr)).await;
                                let mod_task = ModTask::default();
                                arc_task_id = Some(mod_task.get_pointer());
                                new_env.update_context(mod_task);

                            },
                            JobType::Debug => {
                                log.info(format!("new triggered debug: {}", job_expr)).await;

                            },
                        }



                        let job_lvalue = match parse(job_expr, &mut new_env).await {
                            Ok(l) => l,
                            Err(e) => {
                                job
                                    .sender.try_send(Err(e)).unwrap();
                                continue;
                                }
                        };

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

                        let async_handle = LAsyncHandle::new(future, tx);

                        let response: Response = match job_type {
                            JobType::Task => {
                                Response::Trigger(TaskTrigger::new(arc_task_id.unwrap(), async_handle))
                            }
                            JobType::Debug => {
                                Response::Handle(async_handle)
                            }
                        };


                        match job.sender
                            .try_send(Ok(response)) {
                            Ok(_) =>{}
                            Err(e) => {
                                log.error(e.to_string()).await;
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