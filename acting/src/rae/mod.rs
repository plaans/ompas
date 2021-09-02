#![allow(unused_imports)]

use std::sync::Arc;

use async_recursion::async_recursion;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::{mpsc, Mutex};
use tokio_stream::StreamExt;

//use crate::rae::context::{Action, Method, SelectOption, Status, TaskId};
use crate::rae::context::actions_progress::async_status_watcher_run;
use crate::rae::context::rae_env::RAEEnv;
use crate::rae::context::ressource_access::wait_on::task_check_wait_on;
use crate::rae::module::mod_rae_exec::{Job, JobId, RAE_LAUNCH_PLATFORM};
use log::{error, info, warn};
use ompas_lisp::core::{eval, ContextCollection, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::structs::{LError, LValue};
use std::mem;

pub mod context;
pub mod module;
mod select_methods;

pub type Lisp = String;

pub enum RAEError {
    RetryFailure,
    Other(String),
}

#[derive(Debug, Default, Clone)]
pub struct RAEOptions {
    select_option: SelectOption,
    platform_config: Option<String>,
}

impl RAEOptions {
    pub fn new(option: SelectOption) -> Self {
        Self {
            select_option: option,
            platform_config: None,
        }
    }

    pub fn set_select_option(&mut self, dr0: usize, nro: usize) {
        self.select_option.set_dr0(dr0);
        self.select_option.set_nr0(nro);
    }

    pub fn get_select_option(&self) -> &SelectOption {
        &self.select_option
    }

    pub fn set_platform_config(&mut self, str: String) {
        self.platform_config = Some(str);
    }

    pub fn get_platform_config(&self) -> Option<String> {
        self.platform_config.clone()
    }
}

#[derive(Default, Debug, Copy, Clone)]
pub struct SelectOption {
    dr0: usize,
    nro: usize,
}

impl SelectOption {
    pub fn new(dr0: usize, nro: usize) -> Self {
        SelectOption { dr0, nro }
    }

    pub fn set_dr0(&mut self, dr0: usize) {
        self.dr0 = dr0;
    }

    pub fn set_nr0(&mut self, nro: usize) {
        self.nro = nro;
    }

    pub fn get_dr0(&self) -> usize {
        self.dr0
    }

    pub fn get_nro(&self) -> usize {
        self.nro
    }
}

pub type ReactiveTriggerId = usize;

#[derive(Debug, Clone)]
pub enum TaskType {
    Task,
    Event,
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {}

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
    //info!("LValue to be evaluated: {}", job_lvalue);
    match eval(&job_lvalue, &mut env, &mut ctxs) {
        Ok(lv) => info!("result of task {}: {}", job_lvalue, lv),
        Err(e) => error!("End of progress_2: {}", e),
    }
}

fn _select_greedy(env: &RAEEnv, task: &LValue, _params: &[LValue]) -> Result<LValue, LError> {
    let methods = env.get_methods_from_task(task)?;
    //println!("methods: {}", methods);
    if let LValue::List(list) = methods {
        Ok(list.first().unwrap_or(&LValue::Nil).clone())
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
