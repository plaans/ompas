use std::collections::{VecDeque, HashMap};
use std::sync::{Arc};
use crate::rae::action::{Action, ActionId};
use crate::rae::method::{ActionStatus, RefinementStack};
use crate::rae::task::{TaskId, Task};
use ompas_lisp::structs::{Module, GetModule};
use ompas_modules::doc::{Documentation, LHelp};
use tokio::sync::Mutex;
use tokio::sync::mpsc::Receiver;

pub struct Agenda {
    pub tasks: Vec<TaskId>,
    map: HashMap<TaskId, Task>,
    next_id: usize
}

impl Agenda {
    pub fn remove_by_id(&mut self, task_id: &TaskId) {
        todo!()
    }

    pub fn remove(&mut self, index: usize) {
        let task_id = self.tasks[index];
        self.tasks.remove(index);
        self.map.remove(&task_id);

    }


    pub fn add_task(&mut self, task: Task) -> usize {
        let id = self.get_new_id();
        self.map.insert(id, task);
        self.tasks.push(id);
        id
    }

    fn get_new_id(&mut self) -> usize {
        let result = self.next_id;
        self.next_id+=1;
        result
    }

    pub fn set_task_refinement_stack(&mut self, task_id: &TaskId, rs: RefinementStack) {
        self.map.get_mut(task_id).unwrap().refinement_stack = rs;
    }

    pub fn get_stacks(&self) -> Vec<RefinementStack> {
        let result = self.map.values();
        let mut vec = vec![];
        for e in result {
            vec.push(e.refinement_stack.clone())
        }
        vec
    }

    pub fn get_task(&self, task_id: &TaskId) -> Option<&Task> {
        self.map.get(task_id)
    }

    pub fn get_stack(&self, task_id: &TaskId) -> Option<&RefinementStack> {
        match self.get_task(task_id) {
            None => None,
            Some(task) => {
                Some(&task.refinement_stack)
            }
        }
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

    pub fn update_status_action(&mut self, action_id: &ActionId, status: ActionStatus) {
        self.status.insert(*action_id, status);
    }

    pub fn get_action_status(&self, action_id: &ActionId) -> &ActionStatus {
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
    pub stream: Receiver<Task>,
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
/*impl CtxRAE {
    pub fn get_ref_mut_agenda(&mut self) -> &mut Agenda {
        &mut self.agenda
    }

    pub fn get_ref_mut_stream(&mut self) -> &mut Stream {
        &mut self.stream
    }
}*/

impl CtxRAE {
    pub fn get_execution_status(&self, action_id : &ActionId) -> Option<&ActionStatus> {
        self.actions_progress.status.get(action_id)
    }
}

pub type ReactiveTriggerId = usize;

#[derive(Debug, Clone)]
pub enum TaskType {
    Task,
    Event,
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {

}

/*pub struct Stream {
    inner: tokio::stream;
}*/

/*#[derive(Default, Debug, Clone)]
pub struct Stream {
    inner : Arc<Mutex<VecDeque<Task>>>,
}

impl Stream {
    pub async fn push(&self, task: Task) {

    }

    pub async fn pop(&self) -> Option<Task> {
        self.inner.lock().await.pop_front()
    }
}

impl Stream {
    pub async fn is_empty(&self) -> bool {
        self.inner.lock().await.is_empty()
    }
}

pub struct StreamIterator<'a> {
    stream : &'a Stream,
    index: usize
}

impl<'a> IntoIterator for &'a Stream {
    type Item = Task;
    type IntoIter = StreamIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        StreamIterator {
            stream: self,
            index: 0,
        }
    }
}

impl<'a> Iterator for StreamIterator<'a> {
    type Item = Task;

    fn next(&mut self) -> Option<Self::Item> {
        let result = if self.stream.inner.lock().await.len() < self.index {
            Some(self.stream.inner.lock().await[self.index].clone())
        }else {
            None
        };
        self.index +=1;
        result
    }
}*/