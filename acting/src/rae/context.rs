use crate::rae::module::mod_rae_exec::{Job, JobId};
use crate::rae::refinement::{RefinementStack, StackFrame};
use crate::rae::state::{ActionStatus, LState, RAEState};
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{InitLisp, LError, LFn, LLambda, LValue};
use std::collections::{HashMap, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::panic::panic_any;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, RwLock};
use std::thread;
use tokio::sync::mpsc::Receiver;
use crate::rae::status::ActionStatusSync;
use tokio::sync::mpsc;
use crate::rae::TOKIO_CHANNEL_SIZE;

#[derive(Default, Debug)]
pub struct Agenda {
    pub jobs: Vec<JobId>,
    pub stacks: HashMap<JobId, RefinementStack>,
    next_id: usize,
}

impl Agenda {
    pub fn remove_by_id(&mut self, _task_id: &JobId) {
        todo!()
    }

    pub fn remove(&mut self, index: usize) {
        let job_id = self.jobs[index];
        self.jobs.remove(index);
        self.stacks.remove(&job_id);
    }

    pub fn add_job(&mut self, job: Job) -> usize {
        let id = self.get_new_id();
        let mut inner = VecDeque::new();
        inner.push_front(StackFrame {
            job_id: id,
            method: None,
            tried: vec![],
        });
        self.stacks.insert(id, RefinementStack { job, inner });
        self.jobs.push(id);
        id
    }

    fn get_new_id(&mut self) -> usize {
        let result = self.next_id;
        self.next_id += 1;
        result
    }

    pub fn set_refinement_stack(&mut self, job_id: &JobId, rs: RefinementStack) {
        self.stacks.insert(*job_id, rs);
    }

    pub fn get_stacks(&self) -> Vec<&RefinementStack> {
        let mut vec = vec![];
        for e in &self.stacks {
            vec.push(e.1)
        }
        vec
    }

    pub fn get_stack(&self, job_id: &JobId) -> Option<&RefinementStack> {
        self.stacks.get(job_id)
    }
}

pub struct RAEEnv {
    pub job_receiver: Option<Receiver<Job>>,
    pub status_watcher: Option<Receiver<usize>>,
    pub agenda: Agenda,
    pub actions_progress: ActionsProgress,
    pub state: RAEState,
    pub env: LEnv,
    pub ctxs: ContextCollection,
    pub init_lisp: InitLisp,
}

pub const RAE_TASK_METHODS_MAP: &str = "rae-task-methods-map";
pub const RAE_TASK_LIST: &str = "rae-task-list";
pub const RAE_METHOD_LIST: &str = "rae_methods_list";
pub const RAE_ACTION_LIST: &str = "rae_actions_list";
pub const RAE_STATE_FUNCTION_LIST: &str = "rae-state-function-list";
pub const RAE_SYMBOL_TYPE: &str = "rae-symbol-type";
pub const ACTION_TYPE: &str = "action_type";
pub const TASK_TYPE: &str = "task_type";
pub const METHOD_TYPE: &str = "method_type";
pub const STATE_FUNCTION_TYPE: &str = "state_function_type";
//pub const RAE_MAP_TYPE:&str = "rae-map-type";

impl Default for RAEEnv {
    fn default() -> Self {
        let (mut env, ctxs, init_lisp) = LEnv::root();
        env.insert(RAE_ACTION_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_METHOD_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_TASK_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_STATE_FUNCTION_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_SYMBOL_TYPE.to_string(), LValue::Map(Default::default()));
        env.insert(
            RAE_TASK_METHODS_MAP.to_string(),
            LValue::Map(Default::default()),
        );
        Self {
            job_receiver: None,
            agenda: Default::default(),
            actions_progress: Default::default(),
            state: Default::default(),
            env,
            ctxs,
            init_lisp,
            status_watcher: None
        }
    }
}

impl RAEEnv {
    pub fn new(job_receiver: Option<Receiver<Job>>, status_watcher: Option<Receiver<usize>>) -> Self {
        let (mut env, ctxs, init_lisp) = LEnv::root();
        env.insert(RAE_ACTION_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_METHOD_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_TASK_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_STATE_FUNCTION_LIST.to_string(), LValue::List(vec![]));
        env.insert(RAE_SYMBOL_TYPE.to_string(), LValue::Map(Default::default()));
        env.insert(
            RAE_TASK_METHODS_MAP.to_string(),
            LValue::Map(Default::default()),
        );
        Self {
            job_receiver,
            agenda: Default::default(),
            actions_progress: Default::default(),
            state: Default::default(),
            env,
            ctxs,
            init_lisp,
            status_watcher,
        }
    }
}

impl RAEEnv {
    pub fn add_action(&mut self, label: String, value: LValue) -> Result<(), LError> {
        self.insert(label.clone(), value)?;
        let action_list = self.env.get_symbol(RAE_ACTION_LIST).unwrap();
        if let LValue::List(mut list) = action_list {
            list.push(LValue::Symbol(label.clone()));
            self.env
                .set(RAE_ACTION_LIST.to_string(), list.into())
                .expect("list of action should be already defined in environment");
        }

        let symbol_type = self.env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
        if let LValue::Map(mut map) = symbol_type {
            map.insert(
                LValue::Symbol(label),
                LValue::Symbol(ACTION_TYPE.to_string()),
            );
            self.env
                .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                .expect("map of symbol type should be already defined in environment")
        }
        Ok(())
    }

    pub fn add_state_function(&mut self, label: String, value: LValue) -> Result<(), LError> {
        self.insert(label.clone(), value)?;
        let state_function_list = self.env.get_symbol(RAE_STATE_FUNCTION_LIST).unwrap();
        if let LValue::List(mut list) = state_function_list {
            list.push(LValue::Symbol(label.clone()));
            self.env
                .set(RAE_STATE_FUNCTION_LIST.to_string(), list.into())
                .expect("list of state function should be already defined in environment");
        }

        let symbol_type = self.env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
        if let LValue::Map(mut map) = symbol_type {
            map.insert(
                LValue::Symbol(label),
                LValue::Symbol(STATE_FUNCTION_TYPE.to_string()),
            );
            self.env
                .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                .expect("map of symbol type should be already defined in environment")
        }
        Ok(())
    }

    pub fn add_task(&mut self, label: String, value: LValue) -> Result<(), LError> {
        self.insert(label.clone(), value)?;
        let task_list = self.env.get_symbol(RAE_TASK_LIST).unwrap();
        if let LValue::List(mut list) = task_list {
            list.push(LValue::Symbol(label.clone()));
            self.env
                .set(RAE_TASK_LIST.to_string(), list.into())
                .expect("list of task should be already defined in environment");
        }
        let mut map: im::HashMap<LValue, LValue> = self
            .get_element(RAE_TASK_METHODS_MAP)
            .unwrap()
            .try_into()
            .unwrap();
        map.insert(LValue::Symbol(label.clone()), LValue::Nil);

        self.env.set(RAE_TASK_METHODS_MAP.to_string(), map.into())?;

        let symbol_type = self.env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
        if let LValue::Map(mut map) = symbol_type {
            map.insert(LValue::Symbol(label), LValue::Symbol(TASK_TYPE.to_string()));
            self.env
                .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                .expect("map of symbol type should be already defined in environment")
        }
        Ok(())
    }

    pub fn add_method(
        &mut self,
        method_label: String,
        task_label: String,
        value: LValue,
    ) -> Result<(), LError> {
        self.insert(method_label.clone(), value)?;
        let method_list = self.env.get_symbol(RAE_METHOD_LIST).unwrap();
        if let LValue::List(mut list) = method_list {
            list.push(LValue::Symbol(method_label.clone()));
            self.env
                .set(RAE_METHOD_LIST.to_string(), list.into())
                .expect("list of method should be already defined in environment");

            self.add_method_to_task(task_label, method_label.clone())?;
            let symbol_type = self.env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
            if let LValue::Map(mut map) = symbol_type {
                map.insert(
                    LValue::Symbol(method_label),
                    LValue::Symbol(METHOD_TYPE.to_string()),
                );
                self.env
                    .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                    .expect("map of symbol type should be already defined in environment")
            }
        }

        Ok(())
    }

    pub fn insert(&mut self, label: String, value: LValue) -> Result<(), LError> {
        match self.env.get_symbol(&label) {
            None => {
                self.env.insert(label, value);
                Ok(())
            }
            Some(_) => Err(SpecialError(format!(
                "Symbol \"{}\" already defined.",
                label
            ))),
        }
    }

    pub fn add_method_to_task(
        &mut self,
        task_label: String,
        method_label: String,
    ) -> Result<(), LError> {
        let mut map: im::HashMap<LValue, LValue> = self
            .env
            .get_symbol(RAE_TASK_METHODS_MAP)
            .unwrap()
            .try_into()
            .unwrap();

        let list = match map.get(&LValue::Symbol(task_label.clone())) {
            None => {
                return Err(SpecialError(format!(
                    "task \"{}\" is not defined, cannot add method to it.",
                    task_label
                )))
            }
            Some(l) => l,
        };
        let new_list: LValue = match list {
            LValue::List(l) => {
                l.clone().push(method_label.into());
                l.into()
            }
            LValue::Nil => vec![method_label].into(),
            _ => panic!("should be a list or nothing"),
        };
        map.insert(LValue::Symbol(task_label), new_list);
        self.env
            .set(RAE_TASK_METHODS_MAP.into(), map.into())
            .expect("should not return an error");
        Ok(())
    }

    pub fn get_methods_from_task(&self, task: &LValue) -> LValue {
        let task_method_map = self.env.get_ref_symbol(RAE_TASK_METHODS_MAP).unwrap();
        if let LValue::Map(map) = task_method_map {
            let methods = map.get(task).unwrap().clone();
            methods
        } else {
            panic!("this should be a LValue::Map")
        }
    }

    pub fn get_element(&self, label: &str) -> Option<LValue> {
        self.env.get_symbol(label)
    }

    pub fn pretty_debug(&self, key: Option<String>) -> String {
        let mut string = String::new();
        if let Some(_label) = key {
            todo!()
        } else {
            let state_function_symbol = self.env.get_symbol(RAE_STATE_FUNCTION_LIST).unwrap();
            let state_function_list: Vec<LValue> = state_function_symbol.try_into().unwrap();

            let action_list_symbol = self.env.get_symbol(RAE_ACTION_LIST).unwrap();
            let action_list: Vec<LValue> = action_list_symbol.try_into().unwrap();
            let task_list_symbol = self.env.get_symbol(RAE_TASK_LIST).unwrap();
            let task_list: Vec<LValue> = task_list_symbol.try_into().unwrap();
            let map_task_method: im::HashMap<LValue, LValue> = self
                .env
                .get_symbol(RAE_TASK_METHODS_MAP)
                .unwrap()
                .try_into()
                .unwrap();
            string.push_str("RAEEnv: \n");
            string.push_str("\tState Function(s)\n");
            for state_function in state_function_list {
                let state_function_label: String = state_function.try_into().unwrap();
                let state_function_body = self.env.get_symbol(&state_function_label).unwrap();
                string.push_str(
                    format!("\t\t- {}: {}\n", state_function_label, state_function_body).as_str(),
                );
            }
            string.push('\n');
            string.push_str("\t*Action(s): \n");
            for action in action_list {
                let action_label: String = action.try_into().unwrap();
                let action_body = self.env.get_symbol(&action_label).unwrap();
                string.push_str(format!("\t\t- {}: {}\n", action_label, action_body).as_str());
            }
            string.push('\n');
            string.push_str("\t*Task(s): \n");
            for task in task_list {
                let task_label: String = task.clone().try_into().unwrap();
                let task_body = self.env.get_symbol(&task_label).unwrap();
                string.push_str(format!("\t\t-{}: {}\n", task_label, task_body).as_str());
                string.push_str("\t\t*Method(s): \n");
                let methods = map_task_method.get(&task);
                match methods {
                    None => string.push_str("\t\t\t nil\n"),
                    Some(methods) => {
                        let methods: Vec<LValue> = methods.try_into().unwrap();
                        for m in methods {
                            let method_label: String = m.try_into().unwrap();
                            let method = self.env.get_symbol(&method_label).unwrap();
                            string
                                .push_str(format!("\t\t\t-{}: {}\n", method_label, method).as_str())
                        }
                    }
                }
            }
        }
        string
    }
}

pub type ActionLabel = String;
pub type ActionId = usize;
pub type MethodLabel = String;
pub type MethodId = usize;
pub type TaskLabel = String;
pub type TaskId = usize;

//struct to handle actions trigger, status and updates by other modules as godot
#[derive(Default, Debug, Clone)]
pub struct ActionsProgress {
    pub status: Arc<RwLock<HashMap<ActionId, Status>>>,
    pub sync: ActionStatusSync,
    next_id: Arc<AtomicUsize>,
}

impl ActionsProgress {
    pub fn declare_new_watcher(&self, action_id: &usize) -> Receiver<bool> {
        let (sender, receiver) = mpsc::channel(TOKIO_CHANNEL_SIZE);
        self.sync.add_action_to_watch(*action_id, sender);
        receiver
    }
}

#[derive(Clone, Debug)]
pub enum Status {
    Pending,
    Running,
    Failure,
    Done,
}

impl ActionsProgress {
    pub fn get_status(&self, id: &ActionId) -> Option<Status> {
        self.status.read().unwrap().get(id).cloned()
    }

    pub fn add_action(&self) -> usize {
        let id = self.get_new_id();
        self.status
            .write()
            .unwrap()
            .insert(id, Status::Pending);
        id
    }

    pub fn update_status_action(&self, action_id: &ActionId, status: Status) {
        self.status.write().unwrap().insert(*action_id, status);
    }

    pub fn get_action_status(&self, action_id: &ActionId) -> Status {
        self.status.read().unwrap().get(action_id).unwrap().clone()
    }

    pub fn get_new_id(&self) -> usize {
        let result = self.next_id.load(Ordering::Relaxed);
        let new_value = result + 1;
        self.next_id.store(new_value, Ordering::Relaxed);
        result
    }
}

#[derive(Debug, Default)]
pub struct RAEOptions {
    select_option: SelectOption,
}

impl RAEOptions {
    pub fn new(option: SelectOption) -> Self {
        Self {
            select_option: option,
        }
    }

    pub fn set_select_option(&mut self, dr0: usize, nro: usize) {
        self.select_option.set_dr0(dr0);
        self.select_option.set_nr0(nro);
    }

    pub fn get_select_option(&self) -> &SelectOption {
        &self.select_option
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
