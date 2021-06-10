
use std::sync::atomic::AtomicBool;
use std::collections::{VecDeque, HashMap};
use std::sync::{Arc, Mutex};
use crate::rae::structs::{ReactiveTrigger, RAETask, ActionStatus};
use crate::rae::action::{Action, ActionId};
use crate::rae::method::{ActionStatus, Method};
use crate::rae::task::{TaskId, Task};
use ompas_lisp::structs::{Module, GetModule};
use ompas_modules::doc::{Documentation, LHelp};

pub struct Agenda {
    pub task_set: Vec<TaskId>,
    map: HashMap<TaskId, Task>,
    next_id: usize
}

impl Agenda {
    pub fn add_task(&mut self, task: Task) -> usize {
        let id = self.get_new_id();
        self.map.insert(id, task);
        self.task_set.push(id);
        id
    }

    fn get_new_id(&mut self) -> usize {
        let result = self.next_id;
        self.next_id+=1;
        result
    }

}

//struct to handle actions trigger, status and updates by other modules as godot
pub struct ActionsProgress {
    stack: VecDeque<ActionId>,
    map : HashMap<ActionId, Action>,
    status: HashMap<ActionId, ActionStatus>,
    next_id : usize,
}

impl ActionsProgress {
    pub fn add_action(&mut self, action: Action) -> usize {
        let id = self.get_new_id();
        self.map.insert(id, action);
        id
    }

    pub fn update_status_action(&mut self, action_id: id, status: ActionStatus) {
        self.status.insert(action_id, status);
    }

    pub fn get_action_status(&self, action_id: id) -> &ActionStatus {
        self.status.get(action_id).unwrap()
    }

    fn get_new_id(&mut self) -> usize {
        let result = self.next_id;
        self.next_id+=1;
        result
    }
}

pub struct RAEOptions {
    pub select_option: SelectOption,
}


pub struct CtxRAE {
    pub stream: Stream,
    pub log: String,
    pub actions_progress : ActionsProgress,
    pub agenda: Agenda,
    pub options: RAEOptions,
}

impl GetModule for CtxRAE {
    fn get_module(self) -> Module {
        todo!()
    }
}

impl Documentation for CtxRAE {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}

pub struct SelectOption {
    dr0: usize,
    nro: usize,
}

//methods to access attributes
impl CtxRAE {
    pub fn get_ref_mut_agenda(&mut self) -> &mut Agenda {
        &mut self.agenda
    }

    pub fn get_ref_mut_stream(&mut self) -> &mut Stream {
        &mut self.stream
    }
}

impl CtxRAE {
    pub fn get_execution_status(&self, action_id : &ActionId) -> Option<&ActionStatus> {
        self.status.get(action_id)
    }
}

pub type ReactiveTriggerId = usize;

#[derive(Default, Debug, Clone)]
pub enum TaskType {
    Task,
    Event,
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {

}


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
    pub step : MethodStepId,
    pub tried: Vec<Method>
}

impl RefinementTuple {
    pub fn increment_step(&mut self) {
        self.step+=1;
    }

    pub fn is_last_step(&self) -> bool{
        self.step == self.method.len()
    }
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
        self.inner.push_front(rt);
    }

    pub fn top(&self) -> Option<&RefinementTuple> {
        self.inner.front()
    }

    pub fn pop(&mut self) -> Option<RefinementTuple> {
        self.inner.pop_front()
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