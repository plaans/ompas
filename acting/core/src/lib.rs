use crate::error::RaeExecError;
use crate::exec::acting_context::ModActingContext;
use futures::FutureExt;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::logger::LogClient;
use ompas_middleware::LogLevel;
use ompas_middleware::ProcessInterface;
use ompas_structs::acting_manager::process::plan_var::AsCst;
use ompas_structs::acting_manager::process::process_ref::ProcessRef;
use ompas_structs::acting_manager::ActingManager;
use ompas_structs::interface::job::JobType;
use ompas_structs::interface::rae_command::OMPASJob;
use ompas_structs::interface::trigger_collection::{Response, TaskTrigger};
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
    acting_manager: ActingManager,
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
                        log.debug("new job received!").await;

                        let mut new_env = env.clone();

                        let mut pr: ProcessRef = ProcessRef::Id(0);

                        let job_type = job.r#type;
                        let job_expr = &job.expr;



                        let job_lvalue: LValue = match parse(job_expr, &mut new_env).await {
                            Ok(l) => l,
                            Err(e) => {
                                job
                                    .sender.try_send(Err(e)).unwrap();
                                continue;
                                }
                        };

                        match job_type {
                            JobType::Task => {
                                log.debug(format!("new triggered task: {}", job_expr)).await;
                                let vec: Vec<LValue> = job_lvalue.clone().try_into().unwrap();
                                let mut vec_cst = vec![];
                                for e in vec {
                                    vec_cst.push(e.as_cst())
                                }
                                let id: ProcessRef = acting_manager.new_high_level_task(job_lvalue.to_string(),vec_cst).await;
                                let mod_context: ModActingContext = ModActingContext::new(id.clone());
                                pr = id;
                                new_env.update_context(mod_context);

                            },
                            JobType::Debug => {
                                log.debug(format!("new triggered debug: {}", job_expr)).await;
                            },
                        }

                        //info!("LValue to be evaluated: {}", job_lvalue);

                        let (tx, rx) = new_interruption_handler();
                        killers.push(tx.clone());
                        let log2 = log.clone();
                        let pr2 = pr.clone();
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
                                Err(e) => log2.error(format!("Error evaluating task {job_lvalue}({:?}): {e}", pr2)).await,
                            }

                            result
                        }) as FutureResult)
                            .shared();

                        let f2 = future.clone();


                        tokio::spawn(f2);

                        let async_handle = LAsyncHandle::new(future, tx);

                        let response: Response = match job_type {
                            JobType::Task => {
                                Response::Trigger(TaskTrigger::new(pr, async_handle))
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
