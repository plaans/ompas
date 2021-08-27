#![allow(unused_imports)]

use std::sync::Arc;

use async_recursion::async_recursion;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::{mpsc, Mutex};
use tokio_stream::StreamExt;

use crate::rae::context::{RAEEnv, RAEOptions, SelectOption, RAE_TASK_METHODS_MAP};
//use crate::rae::context::{Action, Method, SelectOption, Status, TaskId};
use crate::rae::module::mod_rae_exec::{Job, JobId, RAE_LAUNCH_PLATFORM};
use crate::rae::ressource_access::wait_on::task_check_wait_on;
use crate::rae::status::async_status_watcher_run;
use log::{error, info, warn};
use ompas_lisp::async_await;
use ompas_lisp::async_await::TaskHandler;
use ompas_lisp::core::{eval, ContextCollection, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::structs::{LError, LValue};
use std::mem;

pub mod agenda;
pub mod context;
pub mod module;
pub mod ressource_access;
mod select_methods;
pub mod state;
pub mod status;

pub type Lisp = String;

pub enum RAEError {
    RetryFailure,
    Other(String),
}

const TOKIO_CHANNEL_SIZE: usize = 65_536; //=2^16

/// Main RAE Loop:
/// Receives Job to handle in separate tasks.
pub async fn rae_run(mut context: RAEEnv, options: &RAEOptions, _log: String) {
    let async_action_status = context.actions_progress.sync.clone();
    let receiver_sync = mem::replace(&mut context.status_watcher, None).unwrap();
    tokio::spawn(async move {
        //println!("starting status watcher");
        async_status_watcher_run(async_action_status, receiver_sync).await;
    });

    //println!("async status watcher");

    let mut receiver = mem::replace(&mut context.job_receiver, None).unwrap();
    //Ubuntu::
    let lvalue: LValue = match options.get_platform_config() {
        None => vec![RAE_LAUNCH_PLATFORM].into(),
        Some(string) => {
            info!("Platform config: {}", string);
            vec![RAE_LAUNCH_PLATFORM.to_string(), string].into()
        }
    };
    let result = eval(&lvalue, &mut context.env, &mut context.ctxs);
    //Windows::
    //let result = eval(&vec![LValue::Symbol("rae-open-com-platform".to_string())].into(), &mut context.env, &mut context.ctxs);
    match result {
        Ok(_) => {} //println!("successfully open com with platform"),
        Err(e) => error!("{}", e),
    }

    let receiver_event_update_state = context.state.subscribe_on_update().await;
    let env_check_wait_on = context.get_eval_env();
    let ctxs_check_wait_on = context.ctxs.clone();
    tokio::spawn(async move {
        task_check_wait_on(
            receiver_event_update_state,
            env_check_wait_on,
            ctxs_check_wait_on,
        )
        .await
    });

    loop {
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        //Note: The whole block could be in an async block
        while let Some(job) = receiver.recv().await {
            //println!("new job received: {}", job);

            //let _job_id = &context.agenda.add_job(job.clone());
            log::info!("new job received!");

            let new_env = context.get_eval_env();
            let new_ctxs = context.ctxs.clone();

            tokio::spawn(async move {
                progress_2(job.core.clone(), new_env, new_ctxs).await;
            });
        }
    }
}

async fn progress_2(job_lvalue: LValue, mut env: LEnv, mut ctxs: ContextCollection) {
    info!("new triggered task: {}", job_lvalue);
    let job_lvalue = LValue::List(vec![job_lvalue]);
    match eval(&job_lvalue, &mut env, &mut ctxs) {
        Ok(lv) => info!("result of task {}: {}", job_lvalue, lv),
        Err(e) => error!("{}", e),
    }
}

fn _select_greedy(env: &RAEEnv, task: &LValue, _params: &[LValue]) -> LValue {
    let methods = env.get_methods_from_task(task);
    //println!("methods: {}", methods);
    if let LValue::List(list) = methods {
        list.first().unwrap_or(&LValue::Nil).clone()
    } else {
        panic!("methods should be a list of methods")
    }
}

///Output the status of the task/event
/// Could be in a terminal, or a log file
pub async fn output(_status: RAEStatus, _log: String) {

    //writes formatted message to log
}

///Takes a task in argument and return the methods
pub fn get_methods() {
    todo!()
}

///Take an instantiated task as argument and return a set of applicable methods
pub fn get_instantiated_methods() {
    todo!()
}

///Select the method that will be run in the RAE
pub fn select_method() {}

pub struct RAEStatus {
    pub task: JobId,
    pub msg: String,
}
