use crate::rae::structs::{Agenda, Stream, ActionStatus, RAEStatus, RefinementStack, RAEState, Method, RAETask, RAEMethod, RefinementTuple, MethodProgress};
use ompas_lisp::structs::LCoreOperator::DefMacro;
use ompas_lisp::structs::GetModule;
use ompas_modules::doc::Documentation;
use crate::rae::progress::{Stream, Agenda, RefinementStack, RefinementTuple, MethodProgress, Progress};
use crate::rae::state::RAEState;
use crate::rae::log::RAEStatus;

pub mod structs;
pub mod method;
pub mod action;
pub mod progress;
pub mod state;
pub mod task;
pub mod lisp;
pub mod log;

pub type Lisp = String;

pub async fn rae_run(progress: &mut Progress, select_option: &SelectOption, log: String) {

    let agenda = progress.get_ref_mut_agenda();
    let stream = progress.get_ref_mut_stream();
    //infinite loop
    //Maybe we can add a system to interrupt, or it stops itself when there is nothing to process
    //We can imagine a system monitoring the stream.
    loop {

        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        for t in stream.into_iter() {
            let state = get_state().await;
            let m = select(state, t, RefinementTuple, select_option).await;
            if m.is_none() {
                output(RAEStatus {
                    task: Default::default(),
                    msg: "failed: no method found in the current state".to_string()
                }, log.clone()).await;
            }else {
                let ra = RefinementStack::new(RefinementTuple {
                    task_id: (),
                    method: m.unwrap(),
                    progress: MethodProgress::default(),
                    tried: vec![]
                });
                //Add the refinement stack to the agenda
                agenda.push(ra);
            }
        }
        //For each stack we progress until there is a success or failure.
        for (i,ra) in agenda.iter_mut().enumerate() {
            let state = get_state().await;
            Progress(ra, state).await;
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
pub async fn Progress(ra: &mut RefinementStack, state: RAEState, progress : &Progress) {
    let tuple = ra.top();
    match tuple {
        None => {}
        Some(rt) => {
            match rt.method.get
        }
    }
}

///Retry a method
pub async fn Retry(ra: &mut RefinementStack) {

}

///Return the actual state
pub async fn get_state() -> RAEState {
    Default::default()
}
pub struct SelectOption {
    dr0: usize,
    nro: usize,
}


///Select the best method to refine a task
pub async fn select(state: RAEState, task: &RAETask, rt: &RefinementTuple, select_option: &SelectOption) -> Option<Method> {
    None
}

///Output the status of the task/event
/// Could be in a terminal, or a log file
pub async fn output(status: RAEStatus, log: String) {

    //writes formatted message to log
}