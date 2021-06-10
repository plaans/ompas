use crate::rae::structs::{Agenda, Stream, ActionStatus, RAEStatus, RefinementStack, RAEState, Method, RAETask, RAEMethod, RefinementTuple, MethodProgress};
use ompas_lisp::structs::LCoreOperator::DefMacro;
use ompas_lisp::structs::GetModule;
use ompas_modules::doc::Documentation;
use crate::rae::context::{Stream, Agenda, RefinementStack, RefinementTuple, MethodProgress, Progress, CtxRAE, SelectOption};
use crate::rae::state::{RAEState, State};
use crate::rae::log::RAEStatus;
use crate::rae::method::{MethodStep, ActionStatus, Method};
use crate::rae::action::Action;
use crate::rae::task::TaskId;

pub mod structs;
pub mod method;
pub mod action;
pub mod context;
pub mod state;
pub mod task;
pub mod lisp;
pub mod log;

pub type Lisp = String;

pub async fn rae_run(context: &mut CtxRAE, select_option: &SelectOption, log: String) {

    let agenda = context.get_ref_mut_agenda();
    let stream = context.get_ref_mut_stream();
    //infinite loop
    //Maybe we can add a system to interrupt, or it stops itself when there is nothing to process
    //We can imagine a system monitoring the stream.
    loop {

        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        for t in stream.into_iter() {
            let state = get_state().await;
            let m = select(state, t, RefinementTuple, &context.select_option).await;
            if m.is_none() {
                output(RAEStatus {
                    task: Default::default(),
                    msg: "failed: no method found in the current state".to_string()
                }, log.clone()).await;
            }else {
                let ra = RefinementStack::new(RefinementTuple {
                    task_id: 0,//to change
                    method: m.unwrap(),
                    step: 0,
                    tried: vec![]
                });
                //Add the refinement stack to the agenda
                agenda.push(ra);
            }
        }
        //For each stack we progress until there is a success or failure.
        for (i,ra) in agenda.iter_mut().enumerate() {
            let state = get_state().await;
            Progress(ra, state, progress).await;
            if ra.is_empty() {
                agenda.remove(i);
                output(RAEStatus {
                    task: Default::default(),
                    msg: "succedded".to_string()
                }, log.clone()).await;
            }else if ra.is_retrial_failure() {
                agenda.remove(i);
                output(RAEStatus {
                    task: Default::default(),
                    msg: "failed".to_string()
                }, log.clone()).await;
            }
        }

    }

}

///Progress a given refinement stack
pub async fn progress(mut rs: RefinementStack, mut state: State, context : &mut CtxRAE) -> Option<RefinementStack> {
    let tuple = rs.top();
    match tuple {
        None => panic!("stack should not be empty!"),
        Some(rt) => match rt.method.get_step(rt.step) {
            None => panic!("Step should not be empty"),
            Some(step) => match step {
                MethodStep::Action(a) => match progress.get_execution_status(&a.id) {
                        None => {
                            //trigger the action
                            trigger_action(a, context);
                            return Some(rs)
                        }
                        Some(status) => match status {
                            ActionStatus::Running => {
                                return Some(rs)
                            }
                            ActionStatus::Failure => {
                                return retry(rs, state, context)
                            }
                            ActionStatus::Done => {
                                return next(rs, state, context)
                            }
                        }
                    }
                MethodStep::Assignment(a) => {
                    state.update_state(a);
                    return next(rs, state, context)
                }
                MethodStep::Task(task) => {
                    let state = get_state().await;
                    let method = select(state.clone(), &task.id, &rs, &context.select_option).await;
                    match method {
                        None => return retry(ra, state, context),
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
pub async fn retry(mut rs: RefinementStack, state: State, context: &mut CtxRAE) -> Result<RefinementStack, RetryFailure> {
    let mut tuple = rs.pop().unwrap(); //we assume that if we retry the stack, it is not empty
    tuple.tried.push(tuple.method);
    let state = get_state().await;
    let method = select(state.clone(), &tuple.task_id, &rs, &context.options.select_option).await;
    match method {
        None => if !rs.is_empty() {
            retry(rs, state, context)
        } else {
            Err(RetryFailure::default())
        },
        Some(m) => {
            rs.push(RefinementTuple {
                task_id: tuple.task_id,
                method: m,
                step: 1,
                tried: vec![]
            });
            return Ok(rs)
        }
    }
}

pub async fn next(mut ra: RefinementStack, state: State, context: &CtxRAE) -> Option<RefinementStack> {
    let mut tuple = ra.pop().unwrap();
    return if !tuple.is_last_step() {
        tuple.increment_step();
        ra.push(tuple);
        Some(ra)
    }
    else if ra.is_empty() {
        None
    }}

///Return the actual state
pub async fn get_state() -> State {
    Default::default()
}


///Select the best method to refine a task
pub async fn select(state: State, task_id: & TaskId, rs: &RefinementStack, select_option: &SelectOption) -> Option<Method> {
    None
}

///Output the status of the task/event
/// Could be in a terminal, or a log file
pub async fn output(status: RAEStatus, log: String) {

    //writes formatted message to log
}

pub fn trigger_action(action: Action, context: &mut Progress) {
    //execute the action
    //Add action to list and put status as running
}