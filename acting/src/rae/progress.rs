
pub type Agenda = Vec<RefinementStack>;

pub struct Progress {
    pub actions_stack: VecDeque<ActionId>,
    pub action_map : HashMap<ActionId, Action>,
    pub status: HashMap<ActionId, ActionStatus>,
    pub trigger_stack: VecDeque<ReactiveTriggerId>,
    pub trigger_map: HashMap<ReactiveTriggerId, ReactiveTrigger>,
    pub agenda: Agenda,
    pub stream: Stream,
}

//methods to access attributes
impl Progress {
    pub fn get_ref_mut_agenda(&mut self) -> &mut Agenda {
        &mut self.agenda
    }

    pub fn get_ref_mut_stream(&mut self) -> &mut Stream {
        &mut self.stream
    }
}

impl Progress {
    pub fn get_execution_status
}

pub type ReactiveTriggerId = usize;

#[derive(Default, Debug, Clone)]
pub enum ReactiveTrigger {
    Task(RAETask),
    Event(RAEEvent),
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {

}

use std::sync::atomic::AtomicBool;
use std::collections::{VecDeque, HashMap};
use std::sync::{Arc, Mutex};
use crate::rae::structs::{ReactiveTrigger, RAETask, ActionStatus};
use crate::rae::action::{Action, ActionId};
use crate::rae::method::{ActionStatus, Method};
use crate::rae::task::TaskId;

#[derive(Default, Debug, Clone)]
pub struct Stream {
    inner : Arc<Mutex<VecDeque<ReactiveTrigger>>>,
}

impl Stream {
    pub async fn push(&self, rt: ReactiveTrigger) {

    }
}


#[derive(Default, Debug, Clone)]
pub struct RefinementStack {
    inner: VecDeque<RefinementTuple>
}

impl RefinementStack {
    pub fn new(rt: RefinementTuple) -> Self {
        let mut inner = VecDeque::new();
        inner.push_back(rt);
        Self {
            inner,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct RefinementTuple {
    pub task_id : TaskId,
    pub method: Method,
    pub progress : MethodStepId,
    pub tried: Vec<RefinementMethodInstance>
}

#[derive(Default, Debug, Clone)]
pub struct RefinementMethodInstance {

}

pub type MethodStepId = usize;

impl RefinementStack {
    pub fn is_empty(&self) -> bool {
        false
    }

    pub fn is_retrial_failure(&self) -> bool  {
        false
    }

    pub fn push(&mut self, rt: RefinementTuple) {

    }

    pub fn top(&mut self) -> Option<&RefinementTuple> {
        self.inner.front()
    }

    pub fn pop(&mut self) -> RefinementTuple {

    }
}

impl Stream {
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

pub struct StreamIterator<'a> {
    stream : &'a Stream,
    index: usize
}

impl<'a> IntoIterator for &'a Stream {
    type Item = ReactiveTrigger;
    type IntoIter = StreamIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        StreamIterator {
            stream: self,
            index: 0,
        }
    }
}

impl<'a> Iterator for StreamIterator<'a> {
    type Item = ReactiveTrigger;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.stream.inner.len() < self.index {
            Some(self.stream.inner[self.index].clone())
        }else {
            None
        };
        self.index +=1;
        result
    }
}