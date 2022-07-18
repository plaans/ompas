use crate::ctx_planning::CtxPlanning;
use log::{error, info, warn};
use monitor::task_check_monitor;
use ompas_rae_language::RAE_LAUNCH_PLATFORM;
use ompas_rae_planning::conversion::convert_domain_to_chronicle_hierarchy;
use ompas_rae_planning::structs::{ConversionCollection, ConversionContext};
use ompas_rae_structs::context::RAEContext;
use ompas_rae_structs::options::SelectMode::Planning;
use ompas_rae_structs::options::{RAEOptions, SelectMode};
use sompas_core::{eval, eval_init};
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::LEnv;
use sompas_structs::lnumber::*;
use sompas_structs::lvalue::LValue;
use std::mem;
use std::ops::Deref;
use std::time::Instant;

use crate::error::RaeExecError;

pub type ReactiveTriggerId = usize;

pub mod ctx_planning;
pub mod error;
pub mod monitor;
pub mod mutex;

#[derive(Debug, Clone)]
pub enum TaskType {
    Task,
    Event,
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {}

pub const TOKIO_CHANNEL_SIZE: usize = 100;

/// Main RAE Loop:
/// Receives Job to handle in separate tasks.
pub async fn rae_run(mut context: RAEContext, options: &RAEOptions, _log: String) {
    let mut receiver = mem::replace(&mut context.job_receiver, None).unwrap();

    //Ubuntu::
    let lvalue: LValue = match options.get_platform_config() {
        None => vec![RAE_LAUNCH_PLATFORM].into(),
        Some(string) => {
            info!("Platform config: {}", string);
            vec![RAE_LAUNCH_PLATFORM.to_string(), string].into()
        }
    };
    let result = eval(&lvalue, &mut context.env, None).await;

    match result {
        Ok(_) => {}
        Err(e) => error!("{}", e),
    }

    let receiver_event_update_state = context.state.subscribe_on_update().await;
    let env_check_wait_on = context.get_exec_env().await;
    tokio::spawn(async move {
        task_check_monitor(receiver_event_update_state, env_check_wait_on).await
    });

    let mut select_mode = *options.get_select_mode();

    let mut env: LEnv = context.get_exec_env().await;
    let cc: Option<ConversionCollection> = if matches!(select_mode, Planning(_, _)) {
        let instant = Instant::now();
        match convert_domain_to_chronicle_hierarchy(ConversionContext {
            domain: context.domain_env.clone(),
            env: context.env.clone(),
            state: context.state.get_snapshot().await,
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

    env.import(
        CtxPlanning::new(
            cc,
            context.domain_env.clone(),
            context.env.clone(),
            select_mode,
        ),
        WithoutPrefix,
    );

    eval_init(&mut env).await;

    loop {
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        //Note: The whole block could be in an async block
        while let Some(job) = receiver.recv().await {
            //println!("new job received: {}", job);

            //let _job_id = &context.agenda.add_job(job.clone());
            info!("new job received!");

            let mut new_env = env.clone();

            tokio::spawn(async move {
                let job_lvalue = job.core;
                info!("new triggered task: {}", job_lvalue);
                //info!("LValue to be evaluated: {}", job_lvalue);
                match eval(&job_lvalue, &mut new_env, None).await {
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
            });
        }
    }
}
