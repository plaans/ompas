use crate::rae::job::{Job, JobId};
use crate::rae::method::{RefinementStack, StackFrame};
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{LError, LLambda};
use std::collections::{HashMap, VecDeque};
use std::fmt::{Display, Formatter};

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
            step: 1,
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

#[derive(Default, Debug, Clone)]
pub struct RAEEnv {
    actions: ActionCollection,
    methods: MethodCollection,
    tasks: TaskCollection,
}

impl RAEEnv {
    pub fn get_env(&self, label: Option<String>) -> String {
        let mut string = String::new();
        if let Some(_label) = label {
            todo!()
        } else {
            string.push_str("RAEEnv: \n\n");
            string.push_str("\t*Actions: \n\n");
            for e in &self.actions.inner {
                string.push_str(format!("\t\t- {}\n", e.1).as_str());
            }
            string.push('\n');
            string.push_str("\t*Tasks: \n\n");
            for e in &self.tasks.inner {
                string.push_str(
                    format!("\t\t-{}: {}\n", self.tasks.get_label(e.0).unwrap(), e.1).as_str(),
                );
                string.push_str("\t\t*Methods: \n");
                for m in self
                    .tasks
                    .get_methods(&e.0)
                    .cloned()
                    .unwrap()
                    .iter()
                    .map(|id| self.methods.get_method_by_id(id).unwrap())
                {
                    string.push_str(
                        format!("\t\t\t-{}: {}\n", self.methods.get_label(&m.id).unwrap(), m)
                            .as_str(),
                    )
                }
            }
        }
        string
    }

    pub fn add_action(&mut self, label: ActionLabel, body: LLambda) {
        self.actions.add_action(label, body);
    }

    pub fn add_task(&mut self, label: TaskLabel, body: LLambda) {
        self.tasks.add_task(label, body);
    }

    pub fn add_method(
        &mut self,
        method_label: MethodLabel,
        task_label: &str,
        body: LLambda,
    ) -> Result<(), LError> {
        let method_id = self.methods.add_method(method_label, body);
        self.tasks.add_method_to_task(task_label, method_id)
    }

    pub fn get_methods(&mut self, task_label: &str) -> Vec<MethodId> {
        self.tasks
            .methods
            .get(self.tasks.get_id(task_label).unwrap())
            .cloned()
            .unwrap()
    }
}

pub type ActionLabel = String;
pub type ActionId = usize;

#[derive(Default, Debug, Clone)]
pub struct ActionCollection {
    labels: HashMap<ActionLabel, ActionId>,
    inner: HashMap<ActionId, Action>,
    next_id: usize,
}

impl ActionCollection {
    pub fn get_id(&self, label: &str) -> Option<&ActionId> {
        self.labels.get(label)
    }

    pub fn get_action_by_id(&self, id: &ActionId) -> Option<&Action> {
        self.inner.get(id)
    }

    pub fn get_action_by_label(&self, label: &str) -> Option<&Action> {
        match self.get_id(label) {
            None => None,
            Some(id) => self.inner.get(id),
        }
    }

    pub fn add_action(&mut self, label: ActionLabel, action: LLambda) -> usize {
        let id = self.get_next_id();
        let action = Action { id, lambda: action };
        self.inner.insert(id, action);
        self.labels.insert(label, id);
        id
    }

    fn get_next_id(&mut self) -> ActionId {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

#[derive(Debug, Clone)]
pub struct Action {
    pub id: ActionId,
    pub lambda: LLambda,
}

impl Display for Action {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "id: {}\n body: {}", self.id, self.lambda)
    }
}

pub type MethodLabel = String;
pub type MethodId = usize;

#[derive(Default, Debug, Clone)]
pub struct MethodCollection {
    labels: HashMap<MethodLabel, MethodId>,
    reverse_map_labels: HashMap<MethodId, MethodLabel>,
    inner: HashMap<MethodId, Method>,
    next_id: usize,
}

impl MethodCollection {
    pub fn get_id(&self, label: &str) -> Option<&MethodId> {
        self.labels.get(label)
    }

    pub fn get_method_by_id(&self, id: &MethodId) -> Option<&Method> {
        self.inner.get(id)
    }

    pub fn get_action_by_label(&self, label: &str) -> Option<&Method> {
        match self.get_id(label) {
            None => None,
            Some(id) => self.inner.get(id),
        }
    }

    pub fn add_method(&mut self, label: MethodLabel, method: LLambda) -> usize {
        let id = self.get_next_id();
        let method = Method { id, lambda: method };
        self.inner.insert(id, method);
        self.labels.insert(label.clone(), id);
        self.reverse_map_labels.insert(id, label);
        id
    }

    pub fn get_label(&self, id: &MethodId) -> Option<&MethodLabel> {
        self.reverse_map_labels.get(id)
    }

    fn get_next_id(&mut self) -> MethodId {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    id: MethodId,
    lambda: LLambda,
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "id: {}\n body: {}", self.id, self.lambda)
    }
}

pub type TaskLabel = String;
pub type TaskId = usize;

#[derive(Default, Debug, Clone)]
pub struct TaskCollection {
    labels: HashMap<TaskLabel, TaskId>,
    reverse_map_labels: HashMap<TaskId, TaskLabel>,
    inner: HashMap<TaskId, Task>,
    methods: HashMap<TaskId, Vec<MethodId>>,
    next_id: usize,
}

impl TaskCollection {
    pub fn get_label(&self, id: &TaskId) -> Option<&TaskLabel> {
        self.reverse_map_labels.get(id)
    }

    pub fn get_id(&self, label: &str) -> Option<&TaskId> {
        self.labels.get(label)
    }

    pub fn get_task_by_id(&self, id: &TaskId) -> Option<&Task> {
        self.inner.get(id)
    }

    pub fn get_task_by_label(&self, label: &str) -> Option<&Task> {
        match self.get_id(label) {
            None => None,
            Some(id) => self.inner.get(id),
        }
    }

    pub fn add_method_to_task(&mut self, label: &str, method_id: MethodId) -> Result<(), LError> {
        let id = self.get_id(label).cloned();
        match id {
            None => Err(SpecialError(
                "task label corresponds to nothing in the environment".to_string(),
            )),
            Some(id) => {
                self.methods
                    .get_mut(&id)
                    .expect("should not panic")
                    .push(method_id);
                Ok(())
            }
        }
    }

    pub fn get_methods(&self, id: &TaskId) -> Option<&Vec<MethodId>> {
        self.methods.get(id)
    }

    pub fn add_task(&mut self, label: TaskLabel, task: LLambda) -> usize {
        let id = self.get_next_id();
        let task = Task { id, lambda: task };
        self.inner.insert(id, task);
        self.methods.insert(id, vec![]);
        self.labels.insert(label.clone(), id);
        self.reverse_map_labels.insert(id, label);
        id
    }

    fn get_next_id(&mut self) -> TaskId {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}

#[derive(Debug, Clone)]
pub struct Task {
    pub id: TaskId,
    pub lambda: LLambda,
}

impl Display for Task {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "id: {}\n body: {}", self.id, self.lambda)
    }
}

//struct to handle actions trigger, status and updates by other modules as godot
#[derive(Default, Debug)]
pub struct ActionsProgress {
    status: HashMap<ActionId, Status>,
    next_id: usize,
}

#[derive(Clone, Debug)]
pub enum Status {
    NotTriggered,
    Running,
    Failure,
    Done,
}

impl ActionsProgress {
    pub fn get_status(&self, id: &ActionId) -> Option<&Status> {
        self.status.get(id)
    }

    pub fn add_action(&mut self) -> usize {
        let id = self.get_new_id();
        self.status.insert(id, Status::NotTriggered);
        id
    }

    pub fn update_status_action(&mut self, action_id: &ActionId, status: Status) {
        self.status.insert(*action_id, status);
    }

    pub fn get_action_status(&self, action_id: &ActionId) -> &Status {
        self.status.get(action_id).unwrap()
    }

    fn get_new_id(&mut self) -> usize {
        let result = self.next_id;
        self.next_id += 1;
        result
    }
}

pub struct RAEOptions {
    select_option: SelectOption,
    exec_command: String,
}

impl RAEOptions {
    pub fn new(option: SelectOption, exec: String) -> Self {
        Self {
            select_option: option,
            exec_command: exec,
        }
    }

    pub fn set_select_option(&mut self, dr0: usize, nro: usize) {
        self.select_option.set_dr0(dr0);
        self.select_option.set_nr0(nro);
    }

    pub fn get_select_option(&self) -> &SelectOption {
        &self.select_option
    }

    pub fn set_exec_command(&mut self, sym: String) {
        self.exec_command = sym;
    }

    pub fn get_exec_command(&self) -> &String {
        &self.exec_command
    }
}

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

//methods to access attributes
/*impl CtxRAE {
    pub fn get_ref_mut_agenda(&mut self) -> &mut Agenda {
        &mut self.agenda
    }

    pub fn get_ref_mut_stream(&mut self) -> &mut Stream {
        &mut self.stream
    }
}*/

pub type ReactiveTriggerId = usize;

#[derive(Debug, Clone)]
pub enum TaskType {
    Task,
    Event,
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {}

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
