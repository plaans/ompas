#![allow(unused_imports)]

use std::sync::Arc;

use async_recursion::async_recursion;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::{mpsc, Mutex};
use tokio_stream::StreamExt;

use crate::rae::context::{RAEEnv, SelectOption};
//use crate::rae::context::{Action, Method, SelectOption, Status, TaskId};
use crate::rae::log::RAEStatus;
use crate::rae::module::mod_rae_exec::Job;
use crate::rae::refinement::{RefinementStack, StackFrame};
use ompas_lisp::core::{eval, ContextCollection, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::structs::{LError, LValue};
use std::mem;
use crate::rae::status::async_status_watcher_run;

pub mod context;
pub mod log;
pub mod module;
pub mod refinement;
pub mod state;
pub mod status;

pub type Lisp = String;

pub enum RAEError {
    RetryFailure,
    Other(String),
}

pub const TOKIO_CHANNEL_SIZE: usize = 65_536; //=2^16

pub async fn rae_run(mut context: RAEEnv, _select_option: &SelectOption, _log: String) {
    //println!("in rae run!");
    //infinite loop
    //Maybe we can add a system to interrupt, or it stops itself when there is nothing to process
    //We can imagine a system monitoring the stream.
    /*
    (ancient) Algorithm:
    Agenda  empty list
    while True do
        for each new task or event  in the input stream do
            s <- current state;
            m <- RAEplan(s; tau ; nil ; empty)
            if m = failed then output(“failed”, tau )
            else
                Ra <- a new, empty refinement stack
                push (; m; nil; ;) onto Ra
                insert Ra into Agenda
        for each Ra 2 Agenda do
            Progress(Ra)
        if Ra is empty then remove it from Agenda
     */

    /*
    (new) algorithm
    while True do
        parallel:
        {
            for each new task or event  in the input stream do
            s <- current state;
            m <- RAEplan(s; tau ; nil ; empty)
            if m = failed then output(“failed”, tau )
            else
                Ra <- a new, empty refinement stack
                push (; m; nil; ;) onto Ra
                insert Ra into Agenda
                spawn progress (Ra)
        }
        {
            for each Ra 2 Agenda do
                Check progress (Ra)
            if Ra is empty then remove it from Agenda
        }
     */

    let async_action_status = context.actions_progress.sync.clone();
    let receiver_sync = mem::replace(&mut context.status_watcher, None).unwrap();
    tokio::spawn(async move {
        println!("starting status watcher");
        async_status_watcher_run(async_action_status, receiver_sync).await;
    });

    println!("async status watcher");

    let mut receiver = mem::replace(&mut context.job_receiver, None).unwrap();

    //Ubuntu::
    let result = eval(
        &vec![LValue::Symbol("rae-launch-platform".to_string())].into(),
        &mut context.env,
        &mut context.ctxs,
    );
    //Windows::
    //let result = eval(&vec![LValue::Symbol("rae-open-com-platform".to_string())].into(), &mut context.env, &mut context.ctxs);
    match result {
        Ok(_) => println!("successfully open com with platform"),
        Err(e) => eprintln!("{}", e),
    }

    loop {
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack

        //Note: The whole block could be in an async block
        while let Some(job) = receiver.recv().await {
            println!("new job received: {}", job);

            let job_id = &context.agenda.add_job(job.clone());

            let job_core = job.core.clone();
            if let LValue::List(list) = job_core {
                //We make the assumption that the job is an instantiated task
                let label = &list[0];
                let params = &list[1..];
                let task_lambda = context.env.get_symbol(&label.to_string());
                match task_lambda {
                    None => {
                        println!("task \"{}\" not defined in env", label)
                    }
                    Some(_task) => {
                        //println!("task \"{}\" found.", label);
                        let method = select_greedy(&context, label, params);
                        match &method {
                            LValue::Nil => println!("no method available"),
                            LValue::Symbol(sym) => {
                                //println!("method \"{}\" choosed", sym);

                                let mut rs = context.agenda.get_stack(job_id).unwrap().clone();
                                let mut frame = rs.pop().unwrap();
                                frame.method = Some(method.clone());
                                rs.push(frame);
                                context.agenda.set_refinement_stack(job_id, rs.clone());
                                let env = context.env.clone();
                                let ctxs = context.ctxs.clone();

                                //println!("ctxs after clone {:?}", ctxs);
                                tokio::spawn(async move {
                                    progress(env, ctxs, rs).await;
                                });
                            }
                            _ => panic!("should be nil or symbol"),
                        }
                    }
                }
            } else {
                panic!("Job core should be a LValue::List")
            }
        }
        /*tokio::spawn(async move {
            let receiver = context.lock().await.stream.get_ref_receiver();
        });

        let job_id = &context.agenda.add_job(job);
        //let state = get_state().await;
        let m = select(
            state,
            job_id,
            &context.agenda.get_stack(job_id).unwrap(),
            &context.options.select_option,
        )
        .await;
        if m.is_none() {
            output(
                RAEStatus {
                    task: Default::default(),
                    msg: "failed: no method found in the current state".to_string(),
                },
                log.clone(),
            )
            .await;
        } else {
            let mut rs = context.agenda.get_stack(job_id).unwrap().clone();
            let mut frame = rs.pop().unwrap();
            frame.method = m;
            rs.push(frame);
            context.agenda.set_refinement_stack(job_id, rs);
        }*/
        //For each stack we progress until there is a success or failure.
        /*for (i, job_id) in context.agenda.jobs.clone().iter().enumerate() {
            //let state = get_state().await;
            let result = progress(
                context.agenda.get_stack(job_id).unwrap().clone(),
                state,
                context,
            )
            .await;
            match result {
                Ok(optional_stack) => match optional_stack {
                    None => {
                        context.agenda.remove(i);
                        output(
                            RAEStatus {
                                task: Default::default(),
                                msg: "succedded".to_string(),
                            },
                            log.clone(),
                        )
                        .await;
                    }
                    Some(new_stack) => {
                        context.agenda.set_refinement_stack(job_id, new_stack);
                    }
                },
                Err(e) => {
                    if let RAEError::RetryFailure = e {
                        context.agenda.remove(i);
                        output(
                            RAEStatus {
                                task: Default::default(),
                                msg: "failed".to_string(),
                            },
                            log.clone(),
                        )
                        .await;
                    }
                }
            }
        }*/
    }
}

fn select_greedy(env: &RAEEnv, task: &LValue, _params: &[LValue]) -> LValue {
    let methods = env.get_methods_from_task(task);
    //println!("methods: {}", methods);
    if let LValue::List(list) = methods {
        list.first().unwrap_or(&LValue::Nil).clone()
    } else {
        panic!("methods should be a list of methods")
    }
}

pub async fn progress(mut env: LEnv, mut ctxs: ContextCollection, mut rs: RefinementStack) {
    let method = rs.pop().unwrap().method.unwrap();
    //println!("method: {}", method);
    let args = rs.job.core;
    //println!("job core: {}", args);
    let slice_args = if let LValue::List(list) = &args {
        &list[1..]
    } else {
        panic!("")
    };
    //println!("args: {:?}", slice_args);
    let body = cons(&[method, slice_args.into()], &env, &()).unwrap();
    //println!("body: {}", body);
    let result = eval(&body, &mut env, &mut ctxs);
    match result {
        Ok(ok) => println!("{}", ok),
        Err(e) => eprintln!("{}", e),
    }
}

/*///Progress a given refinement stack
pub async fn progress(
    mut rs: RefinementStack,
    mut state: State,
    context: &mut CtxRAE,
) -> Result<Option<RefinementStack>, RAEError> {
    let mut frame = rs.top();
    match frame {
        None => panic!("stack should not be empty!"),
        Some(sf) => match sf.method.as_ref().unwrap().get_step(sf.step) {
            None => panic!("Step should not be empty"),
            Some(step) => match step {
                MethodStep::Action(a) => match &a.id {
                    None => {
                        //trigger the action
                        let id = trigger_action(a, context);
                        a.id = Some(id);
                        return Ok(Some(rs));
                    }
                    Some(id) => match context.get_execution_status(id).unwrap() {
                        Status::Running => return Ok(Some(rs)),
                        Status::Failure => return retry(rs, state, context).await,
                        Status::Done => return next(rs, state, context).await,
                        Status::NotTriggered => {
                            todo!();
                        }
                    },
                },
                MethodStep::Assignment(a) => {
                    //state.update_state(a);
                    return next(rs, state, context).await;
                }
                MethodStep::Task(task) => {
                    let state = get_state().await;
                    let method = select(
                        state.clone(),
                        &task.id,
                        &rs,
                        &context.options.get_select_option(),
                    )
                    .await;
                    match method {
                        None => return retry(rs, state, context).await,
                        Some(_) => rs.push(StackFrame {
                            job_id: sf.job_id,
                            method: Default::default(),
                            step: 1,
                            tried: vec![],
                        }),
                    }
                }
            },
        },
    }

    panic!("should never reach this point");
}

#[derive(Default, Debug, Clone)]
pub struct RetryFailure {}

///Retry a method
#[async_recursion]
pub async fn retry(
    mut rs: RefinementStack,
    state: State,
    context: &mut CtxRAE,
) -> Result<Option<RefinementStack>, RAEError> {
    let mut frame = rs.pop().unwrap(); //we assume that if we retry the stack, it is not empty
    frame.tried.push(frame.method.unwrap());
    let state = get_state().await;
    let method = select(
        state.clone(),
        &frame.job_id,
        &rs,
        &context.options.get_select_option(),
    )
    .await;
    match &method {
        None => {
            if !rs.is_empty() {
                retry(rs, state, context).await
            } else {
                Err(RAEError::RetryFailure)
            }
        }
        Some(m) => {
            rs.push(StackFrame {
                job_id: frame.job_id,
                method: Some(m.clone()),
                step: 1,
                tried: vec![],
            });
            return Ok(Some(rs));
        }
    }
}

pub async fn next(
    mut rs: RefinementStack,
    state: State,
    context: &CtxRAE,
) -> Result<Option<RefinementStack>, RAEError> {
    let mut tuple = rs.pop().unwrap();
    return if !tuple.is_last_step() {
        tuple.increment_step();
        rs.push(tuple);
        Ok(Some(rs))
    } else if rs.is_empty() {
        Ok(None)
    } else {
        panic!("error searching the next step in the method")
    };
}

///Return the actual state
pub async fn get_state() -> State {
    Default::default()
}

///Select the best method to refine a task
/// It can use either RAEPlan, UPOM or an other planner
pub async fn select(
    state: State,
    job_id: &TaskId,
    rs: &RefinementStack,
    select_option: &SelectOption,
) -> Option<Method> {
    None
}

pub fn RAEPlan(state: State, job: Job) {
    todo!()
}*/

///Output the status of the task/event
/// Could be in a terminal, or a log file
pub async fn output(_status: RAEStatus, _log: String) {

    //writes formatted message to log
}
