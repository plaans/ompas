#![allow(unused_imports)]
use crate::rae::context::{Action, Method, SelectOption, Status, TaskId};
use crate::rae::lisp::CtxRAE;
use crate::rae::log::RAEStatus;
use async_recursion::async_recursion;
use tokio::sync::mpsc;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio_stream::StreamExt;

pub mod context;
pub mod job;
pub mod lisp;
pub mod log;
pub mod refinement;
pub mod state;

pub type Lisp = String;

pub enum RAEError {
    RetryFailure,
    Other(String),
}

pub const TOKIO_CHANNEL_SIZE: usize = 65_536; //=2^16
                                              /*
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
                                                  while let Some(job) = context.stream.recv().await {

                                                      let job_id = &context.agenda.add_job(job);
                                                      let state = get_state().await;
                                                      let m = select(state, job_id, &context.agenda.get_stack(job_id).unwrap(), &context.options.select_option).await;
                                                      if m.is_none() {
                                                          output(RAEStatus {
                                                              task: Default::default(),
                                                              msg: "failed: no method found in the current state".to_string()
                                                          }, log.clone()).await;
                                                      }else {
                                                          let mut rs = context.agenda.get_stack(job_id).unwrap().clone();
                                                          let mut frame = rs.pop().unwrap();
                                                          frame.method = m;
                                                          rs.push(frame);
                                                          context.agenda.set_refinement_stack(job_id, rs);
                                                      }
                                                  }
                                                  //For each stack we progress until there is a success or failure.
                                                  for (i,job_id) in context.agenda.jobs.clone().iter().enumerate() {
                                                      let state = get_state().await;
                                                      let result = progress(context.agenda.get_stack(job_id).unwrap().clone(), state, context).await;
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
                                                                      context.agenda.set_refinement_stack(job_id, new_stack);
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
                                                                      return Ok(Some(rs))
                                                                  }
                                                                  Some(id) => match context.get_execution_status(id).unwrap() {
                                                                      Status::Running => {
                                                                          return Ok(Some(rs))
                                                                      }
                                                                      Status::Failure => {
                                                                          return retry(rs, state, context).await
                                                                      }
                                                                      Status::Done => {
                                                                          return next(rs, state, context).await
                                                                      }
                                                                      Status::NotTriggered => {
                                                                          todo!();
                                                                      }
                                                                  }
                                                              }
                                                          MethodStep::Assignment(a) => {
                                                              //state.update_state(a);
                                                              return next(rs, state, context).await
                                                          }
                                                          MethodStep::Task(task) => {
                                                              let state = get_state().await;
                                                              let method = select(state.clone(), &task.id, &rs, &context.options.get_select_option()).await;
                                                              match method {
                                                                  None => return retry(rs, state, context).await,
                                                                  Some(_) => {
                                                                      rs.push(StackFrame {
                                                                          job_id : sf.job_id,
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
                                              let mut frame = rs.pop().unwrap(); //we assume that if we retry the stack, it is not empty
                                              frame.tried.push(frame.method.unwrap());
                                              let state = get_state().await;
                                              let method = select(state.clone(), &frame.job_id, &rs, &context.options.get_select_option()).await;
                                              match &method {
                                                  None => if !rs.is_empty() {
                                                      retry(rs, state, context).await
                                                  } else {
                                                      Err(RAEError::RetryFailure)
                                                  },
                                                  Some(m) => {
                                                      rs.push(StackFrame {
                                                          job_id: frame.job_id,
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
                                              pub async fn select(state: State, job_id: &TaskId, rs: &RefinementStack, select_option: &SelectOption) -> Option<Method> {
                                              None
                                              }

                                              pub fn RAEPlan(state: State, job: Job) {
                                              todo!()
                                              }

                                              ///Output the status of the task/event
                                              /// Could be in a terminal, or a log file
                                              pub async fn output(status: RAEStatus, log: String) {

                                              //writes formatted message to log
                                              }

                                              pub fn trigger_action(action: Action, context: &mut CtxRAE) -> usize {
                                              let id = context.actions_progress.add_action();
                                              //execute the action
                                              //Add action to list and put status as running
                                              id
                                              }*/
