use crate::rae::context::{CtxRAE, SelectOption};
use crate::rae::state::State;
use crate::rae::log::RAEStatus;
use crate::rae::method::{MethodStep, ActionStatus, Method, RefinementStack, RefinementTuple};
use crate::rae::action::Action;
use crate::rae::task::{TaskId, Task};
use tokio_stream::StreamExt;
use tokio::sync::mpsc::{Sender, Receiver};
use tokio::sync::mpsc;
use async_recursion::async_recursion;


pub mod structs;
pub mod method;
pub mod action;
pub mod context;
pub mod state;
pub mod task;
pub mod lisp;
pub mod log;

pub type Lisp = String;

pub enum RAEError {
    RetryFailure,
    Other(String),
}

pub const TOKIO_CHANNEL_SIZE: usize = 65_536; //=2^16

pub async fn rae_run(context: &mut CtxRAE, select_option: &SelectOption, log: String) {


    //infinite loop
    //Maybe we can add a system to interrupt, or it stops itself when there is nothing to process
    //We can imagine a system monitoring the stream.
    loop {
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack

        //let (sender, mut receiver): (Sender<Task>, Receiver<Task>) =
        //    mpsc::channel(TOKIO_CHANNEL_SIZE);

        //let mut stream  = tokio_stream::iter(receiver);

        //let mut stream = tokio_stream::iter(vec![Task::default()]);
        //tokio::pin!(stream); //was important for streams

        //Note: The whole block could be in an async block
        while let Some(task) = context.stream.recv().await {
            let task_id = &context.agenda.add_task(task);
            let state = get_state().await;
            let m = select(state, task_id, &context.agenda.get_stack(task_id).unwrap(), &context.options.select_option).await;
            if m.is_none() {
                output(RAEStatus {
                    task: Default::default(),
                    msg: "failed: no method found in the current state".to_string()
                }, log.clone()).await;
            }else {
                let rs = RefinementStack::new(RefinementTuple {
                    task_id: task_id.clone(),//to change
                    method: Some(m.unwrap()),
                    step: 1,
                    tried: vec![]
                });
                context.agenda.set_task_refinement_stack(task_id, rs);
            }
        }
        //For each stack we progress until there is a success or failure.
        for (i,task_id) in context.agenda.tasks.clone().iter().enumerate() {
            let state = get_state().await;
            let result = progress(context.agenda.get_stack(task_id).unwrap().clone(), state, context).await;
            match result {
                Ok(optional_stack) => {
                    match optional_stack {
                        None => {
                            context.agenda.remove(i);
                            output(RAEStatus {
                                task: Default::default(),
                                msg: "succedded".to_string()
                            }, log.clone()).await;
                        }
                        Some(new_stack) => {
                            context.agenda.set_task_refinement_stack(task_id, new_stack);
                        }
                    }
                }
                Err(e) => if let RAEError::RetryFailure = e {
                    context.agenda.remove(i);
                    output(RAEStatus {
                        task: Default::default(),
                        msg: "failed".to_string()
                    }, log.clone()).await;
                }
            }
        }
    }
}

///Progress a given refinement stack
pub async fn progress(mut rs: RefinementStack, mut state: State, context : &mut CtxRAE) -> Result<Option<RefinementStack>, RAEError> {
    let tuple = rs.top();
    match tuple {
        None => panic!("stack should not be empty!"),
        Some(rt) => match rt.method.as_ref().unwrap().get_step(rt.step) {
            None => panic!("Step should not be empty"),
            Some(step) => match step {
                MethodStep::Action(a) => match context.get_execution_status(&a.id) {
                        None => {
                            //trigger the action
                            trigger_action(a, context);
                            return Ok(Some(rs))
                        }
                        Some(status) => match status {
                            ActionStatus::Running => {
                                return Ok(Some(rs))
                            }
                            ActionStatus::Failure => {
                                return retry(rs, state, context).await
                            }
                            ActionStatus::Done => {
                                return next(rs, state, context).await
                            }
                            ActionStatus::NotTriggered => {
                                todo!();
                            }
                        }
                    }
                MethodStep::Assignment(a) => {
                    state.update_state(a);
                    return next(rs, state, context).await
                }
                MethodStep::Task(task) => {
                    let state = get_state().await;
                    let method = select(state.clone(), &task.id, &rs, &context.options.select_option).await;
                    match method {
                        None => return retry(rs, state, context).await,
                        Some(_) => {
                            rs.push(RefinementTuple {
                                task_id: task.id,
                                method: Default::default(),
                                step: 1,
                                tried: vec![]
                            })
                        }
                    }
                }
            }
        },
    }

    panic!("should never reach this point");
}

#[derive(Default, Debug, Clone)]
pub struct RetryFailure {

}

///Retry a method
#[async_recursion]
pub async fn retry(mut rs: RefinementStack, state: State, context: &mut CtxRAE) -> Result<Option<RefinementStack>, RAEError> {
    let mut tuple = rs.pop().unwrap(); //we assume that if we retry the stack, it is not empty
    tuple.tried.push(tuple.method.unwrap());
    let state = get_state().await;
    let method = select(state.clone(), &tuple.task_id, &rs, &context.options.select_option).await;
    match &method {
        None => if !rs.is_empty() {
            retry(rs, state, context).await
        } else {
            Err(RAEError::RetryFailure)
        },
        Some(m) => {
            rs.push(RefinementTuple {
                task_id: tuple.task_id,
                method: Some(m.clone()),
                step: 1,
                tried: vec![]
            });
            return Ok(Some(rs))
        }
    }
}

pub async fn next(mut rs: RefinementStack, state: State, context: &CtxRAE) -> Result<Option<RefinementStack>, RAEError> {
    let mut tuple = rs.pop().unwrap();
    return if !tuple.is_last_step() {
        tuple.increment_step();
        rs.push(tuple);
        Ok(Some(rs))
    }
    else if rs.is_empty() {
        Ok(None)
    }else {
        panic!("error searching the next step in the method")
    }
}

///Return the actual state
pub async fn get_state() -> State {
    Default::default()
}


///Select the best method to refine a task
/// It can use either RAEPlan, UPOM or an other planner
pub async fn select(state: State, task_id: &TaskId, rs: &RefinementStack, select_option: &SelectOption) -> Option<Method> {
    None
}

///Output the status of the task/event
/// Could be in a terminal, or a log file
pub async fn output(status: RAEStatus, log: String) {

    //writes formatted message to log
}

pub fn trigger_action(action: Action, context: &mut CtxRAE) {
    //execute the action
    //Add action to list and put status as running
}