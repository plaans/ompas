use crate::model::process_ref::ProcessRef;
use crate::ompas::error::RaeExecError;
use crate::ompas::interface::job::{Job, JobType};
use crate::ompas::interface::rae_command::OMPASJob;
use crate::ompas::interface::trigger_collection::Response;
use crate::ompas::interface::trigger_collection::TaskProcess;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::acting::ActingManager;
use crate::ompas::manager::planning::planner_manager_interface::FilterWatchedProcesses;
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::scheme::exec::acting_context::ModActingContext;
use futures::FutureExt;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::logger::LogClient;
use ompas_middleware::LogLevel;
use ompas_middleware::ProcessInterface;
use sompas_core::{eval, parse};
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lswitch::{new_interruption_handler, InterruptionSender};
use sompas_structs::lvalue::LValue;
use tokio::sync::mpsc::UnboundedReceiver;

pub mod error;
pub mod interface;
pub mod manager;
pub mod scheme;

pub type ReactiveTriggerId = usize;

pub const PROCESS_OMPAS_MAIN: &str = "__PROCESS_OMPAS_MAIN__";

/// Main RAE Loop:
/// Receives Job to handle in separate tasks.
pub async fn rae(
    acting_manager: ActingManager,
    log: LogClient,
    env: LEnv,
    mut command_rx: UnboundedReceiver<OMPASJob>,
) {
    let mut process =
        ProcessInterface::new(PROCESS_OMPAS_MAIN, PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS).await;

    let mut killers = vec![];

    let mut pending_events = vec![];
    let mut handle_events = false;

    loop {
        tokio::select! {
            command = command_rx.recv() => {
                match command {
                    None => break,
                    Some(OMPASJob::Job(job)) => {

                        if job.is_event() && !handle_events {
                            pending_events.push(job)
                        }else {
                            exec_job(job, &log,&acting_manager, env.clone(), &mut killers).await;
                        }

                    }
                    Some(OMPASJob::StartHandlingEvent(sender)) => {
                        for event in pending_events.drain(..) {
                            exec_job(event, &log, &acting_manager, env.clone(), &mut killers).await;
                        }
                        handle_events = true;
                        sender.send(true).unwrap();
                    }
                    Some(OMPASJob::Stop) => {
                        unreachable!()
                    }
                }
            }
            _ = process.recv() => {
                process.log("Main loop of rae ended", LogLevel::Info);

                for k in &mut killers {
                    k.interrupt();
                }
                break ;
            }
        }
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        //Note: The whole block could be in an async block
    }
}

pub async fn exec_job(
    job: Job,
    log: &LogClient,
    acting_manager: &ActingManager,
    mut env: LEnv,
    killers: &mut Vec<InterruptionSender>,
) {
    log.debug("new job received!");

    let mut pr: ProcessRef = ProcessRef::Id(0);

    let job_type = job.r#type;
    let job_expr = &job.expr;

    let job_lvalue: LValue = match parse(job_expr, &mut env).await {
        Ok(l) => l,
        Err(e) => {
            job.sender.send(Err(e)).unwrap();
            return;
        }
    };

    let is_task = job.is_task();

    match job_type {
        JobType::Task => {
            log.debug(format!("new triggered task: {}", job_expr));
            let vec: Vec<LValue> = job_lvalue.clone().try_into().unwrap();
            let mut vec_cst = vec![];
            for e in vec {
                vec_cst.push(e.as_cst().unwrap())
            }
            let id: ProcessRef = acting_manager
                .new_high_level_task(job_lvalue.to_string(), vec_cst)
                .await;

            let mod_context: ModActingContext = ModActingContext::new(id.clone());
            pr = id;
            env.update_context(mod_context);
        }
        JobType::Debug => {
            log.debug(format!("new triggered debug: {}", job_expr));
        }
        JobType::Init => {
            log.debug(format!("Received init to evaluate: {}", job_expr));
        }
        JobType::Command => {
            let vec: Vec<LValue> = job_lvalue.clone().try_into().unwrap();
            let mut vec_cst = vec![];
            for e in vec {
                vec_cst.push(e.as_cst().unwrap())
            }
            let id: ProcessRef = acting_manager
                .new_high_level_command(job_lvalue.to_string(), vec_cst)
                .await;

            let mod_context: ModActingContext = ModActingContext::new(id.clone());
            pr = id;
            env.update_context(mod_context);
        }
        JobType::Event => {
            log.debug(format!("Received event to evaluate: {}", job_expr));
            let mod_context: ModActingContext = ModActingContext::new(ProcessRef::Id(0));
            //pr = id;
            env.update_context(mod_context);
        }
    }

    //info!("LValue to be evaluated: {}", job_lvalue);

    let (tx, rx) = new_interruption_handler();
    killers.push(tx.clone());
    let log2 = log.clone();
    let pr2 = pr.clone();
    let acting_manager_2 = acting_manager.clone();
    let future = (Box::pin(async move {
        let id = acting_manager_2.get_id(pr2.clone()).await.unwrap();
        let watcher = acting_manager_2
            .subscribe_on_plan_update(FilterWatchedProcesses::Some(vec![id]))
            .await;
        if is_task {
            if let Some(mut watcher) = watcher {
                log2.info(format!("({}) Waiting on plan update.", id));

                let duration = acting_manager_2.get_planner_reactivity_duration();

                tokio::select! {
                    _ = tokio::time::sleep(duration) => {
                        log2.info(format!("({}) Going to execute without a plan.", id));
                    }
                    r = watcher.recv() => {
                         let _ = r.unwrap_or_else(|| {
                             eprintln!("error on watcher");
                             vec![]
                        });
                        log2.info(format!("({}) Plan available, going to execute now.", id));
                    }
                }
            }
        }

        let result = eval(&job_lvalue, &mut env, Some(rx)).await;
        match &result {
            Ok(lv) => log2.info(format!(
                "result of job {}: {}",
                job_lvalue,
                RaeExecError::format_err(lv),
            )),
            Err(e) => {
                log2.error(format!("Error evaluating job {job_lvalue}({:?}): {e}", pr2));
                let id = acting_manager_2.get_id(pr2).await.unwrap();
                acting_manager_2
                    .set_status(&id, ProcessStatus::Failure)
                    .await;
            }
        }

        result
    }) as FutureResult)
        .shared();

    let f2 = future.clone();

    tokio::spawn(f2);

    let async_handle = LAsyncHandle::new(future, tx);

    let response: Response = match job_type {
        JobType::Task | JobType::Command => Response::Process(TaskProcess::new(pr, async_handle)),
        JobType::Debug | JobType::Init | JobType::Event => Response::Handle(async_handle),
    };

    match job.sender.send(Ok(response)) {
        Ok(_) => {}
        Err(e) => {
            log.error(e.to_string());
        }
    }
}
