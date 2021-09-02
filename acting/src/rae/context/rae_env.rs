use crate::rae::context::actions_progress::ActionsProgress;
use crate::rae::context::agenda::Agenda;
use crate::rae::context::rae_state::RAEState;
use crate::rae::module::mod_rae_exec::{Job, JobId};
use crate::rae::TOKIO_CHANNEL_SIZE;
use im::HashMap;
use log::Level::Debug;
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::structs::LCoreOperator::Define;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{InitLisp, LError, LFn, LLambda, LValue};
use ompas_utils::blocking_async;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::panic::panic_any;
use std::ptr::write_bytes;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use tokio::sync::mpsc::Receiver;
use tokio::sync::{mpsc, RwLock};

pub const RAE_TASK_METHODS_MAP: &str = "rae-task-methods-map";
pub const RAE_TASK_LIST: &str = "rae-task-list";
pub const RAE_METHOD_LIST: &str = "rae_methods_list";
pub const RAE_ACTION_LIST: &str = "rae_actions_list";
pub const RAE_STATE_FUNCTION_LIST: &str = "rae-state-function-list";
pub const RAE_SYMBOL_TYPE: &str = "rae-symbol-type";
pub const RAE_METHOD_PARAMETERS_GENERATOR_MAP: &str = "rae-method-instances-generator-map";
pub const RAE_METHOD_PRE_CONDITIONS_MAP: &str = "rae-method-pre-conditions-map";
pub const RAE_METHODS_EFFECTS_MAP: &str = "rae-method-effects-map";
pub const ACTION_TYPE: &str = "action_type";
pub const TASK_TYPE: &str = "task_type";
pub const METHOD_TYPE: &str = "method_type";
pub const STATE_FUNCTION_TYPE: &str = "state_function_type";
pub const LAMBDA_TYPE: &str = "lambda_type";

#[derive(Debug, Clone)]
pub struct Method {
    task_label: String,
    lambda_pre_conditions: LValue,
    lambda_effects: LValue,
    lambda_instances_generator: LValue,
    lambda_body: LValue,
}

impl Method {
    pub fn new(
        task_label: String,
        conds: LValue,
        effects: LValue,
        generator: LValue,
        body: LValue,
    ) -> Self {
        Self {
            task_label,
            lambda_pre_conditions: conds,
            lambda_effects: effects,
            lambda_instances_generator: generator,
            lambda_body: body,
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "-task: {}\n\
            -pre-conditions: {}\n\
            -effects: {}\n\
            -generator: {}\n\
            -body: {}\n",
            self.task_label,
            self.lambda_pre_conditions,
            self.lambda_effects,
            self.lambda_instances_generator,
            self.lambda_body
        )
    }
}
#[derive(Debug, Clone)]
pub struct Task {
    body: LValue,
    methods: Vec<String>,
}

impl Task {
    pub fn new(body: LValue) -> Self {
        Self {
            body,
            methods: vec![],
        }
    }

    pub fn get_body(&self) -> &LValue {
        &self.body
    }

    pub fn get_methods(&self) -> &Vec<String> {
        &self.methods
    }
}

impl Display for Task {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str_methods: String = '('.into();
        for (index, e) in self.methods.iter().enumerate() {
            str_methods.push_str(e.as_str());
            if index < self.methods.len() - 1 {
                str_methods.push(',');
            }
        }
        str_methods.push(')');

        write!(
            f,
            "-body: {}\n\
             -methods: {}\n",
            self.body, str_methods
        )
    }
}

#[derive(Default, Debug, Clone)]
pub struct DomainEnv {
    tasks: HashMap<String, Task>,
    methods: HashMap<String, Method>,
    state_functions: HashMap<String, LValue>,
    actions: HashMap<String, LValue>,
    lambdas: HashMap<String, LValue>,
    map_symbol_type: HashMap<LValue, LValue>,
}

//Getter
impl DomainEnv {
    pub fn get_tasks(&self) -> &HashMap<String, Task> {
        &self.tasks
    }

    pub fn get_methods(&self) -> &HashMap<String, Method> {
        &self.methods
    }

    pub fn get_state_functions(&self) -> &HashMap<String, LValue> {
        &self.state_functions
    }

    pub fn get_actions(&self) -> &HashMap<String, LValue> {
        &self.actions
    }

    pub fn get_lambdas(&self) -> &HashMap<String, LValue> {
        &self.lambdas
    }
}

//Adder
impl DomainEnv {
    pub fn add_task(&mut self, label: String, value: Task) {
        self.tasks.insert(label.clone(), value);
        self.map_symbol_type.insert(label.into(), TASK_TYPE.into());
    }

    pub fn add_method(&mut self, label: String, value: Method) -> Result<(), LError> {
        match self.tasks.get_mut(&value.task_label) {
            None => Err(SpecialError(
                "DomainEnv::add_method",
                format!(
                    "Cannot add method {} because task {} does not exist.",
                    label, value.task_label
                ),
            )),
            Some(task) => {
                task.methods.push(label.clone());
                self.methods.insert(label.clone(), value);
                self.map_symbol_type
                    .insert(label.into(), METHOD_TYPE.into());
                Ok(())
            }
        }
    }

    pub fn add_state_function(&mut self, label: String, value: LValue) {
        self.state_functions.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label.into(), STATE_FUNCTION_TYPE.into());
    }

    pub fn add_action(&mut self, label: String, value: LValue) {
        self.actions.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label.into(), ACTION_TYPE.into());
    }

    pub fn add_lambda(&mut self, label: String, value: LValue) {
        self.lambdas.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label.into(), LAMBDA_TYPE.into());
    }
}

//List of symbols
impl DomainEnv {
    pub fn get_list_tasks(&self) -> LValue {
        self.tasks
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_methods(&self) -> LValue {
        self.methods
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_state_functions(&self) -> LValue {
        self.state_functions
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_actions(&self) -> LValue {
        self.actions
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_list_lambdas(&self) -> LValue {
        self.lambdas
            .keys()
            .map(|k| LValue::from(k.clone()))
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_map_symbol_type(&self) -> LValue {
        self.map_symbol_type.clone().into()
    }
}

//Displayer
impl DomainEnv {
    pub fn print_tasks(&self) -> String {
        let mut str = "*Tasks:\n".to_string();
        for (label, value) in &self.tasks {
            str.push_str(format!("\t-{}: {}\n", label, value).as_str())
        }
        str
    }

    pub fn print_methods(&self) -> String {
        let mut str = "*Methods:\n".to_string();
        for (label, value) in &self.methods {
            str.push_str(format!("\t-{}: {}\n", label, value).as_str())
        }
        str
    }

    pub fn print_state_functions(&self) -> String {
        let mut str = "*State-Functions:\n".to_string();
        for (label, value) in &self.state_functions {
            str.push_str(format!("\t-{}: {}\n", label, value).as_str())
        }
        str
    }

    pub fn print_actions(&self) -> String {
        let mut str = "*Actions:\n".to_string();
        for (label, value) in &self.actions {
            str.push_str(format!("\t-{}: \n{}\n", label, value).as_str())
        }
        str
    }

    pub fn print_lambdas(&self) -> String {
        let mut str = "*Lambdas:\n".to_string();
        for (label, value) in &self.lambdas {
            str.push_str(format!("\t-{}: {}\n", label, value).as_str())
        }
        str
    }
}

impl Display for DomainEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "Domain defined in RAE:\n".to_string();

        str.push_str(format!("\n{}", self.print_tasks()).as_str());
        str.push_str(format!("\n{}", self.print_methods()).as_str());
        str.push_str(format!("\n{}", self.print_state_functions()).as_str());
        str.push_str(format!("\n{}", self.print_actions()).as_str());
        str.push_str(format!("\n{}", self.print_lambdas()).as_str());

        write!(f, "{}", str)
    }
}

impl From<DomainEnv> for LEnv {
    fn from(domain_env: DomainEnv) -> Self {
        let mut env = LEnv::empty();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_effects: HashMap<LValue, LValue> = Default::default();
        let mut map_method_generator: HashMap<LValue, LValue> = Default::default();

        //Add all tasks to env:
        for (label, task) in domain_env.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in domain_env.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
            map_method_pre_conditions.insert(label.into(), method.lambda_pre_conditions.clone());
            map_method_effects.insert(label.into(), method.lambda_effects.clone());
            map_method_generator.insert(label.into(), method.lambda_instances_generator.clone());
        }

        //Add all actions to env:
        for (label, action) in domain_env.get_actions() {
            env.insert(label.clone(), action.clone());
        }

        //Add all state functions to env:
        for (label, state_function) in domain_env.get_state_functions() {
            env.insert(label.clone(), state_function.clone());
        }

        //Add all lambdas to env:

        env.insert(RAE_ACTION_LIST.to_string(), domain_env.get_list_actions());
        env.insert(RAE_METHOD_LIST.to_string(), domain_env.get_list_methods());
        env.insert(RAE_TASK_LIST.to_string(), domain_env.get_list_tasks());
        env.insert(
            RAE_STATE_FUNCTION_LIST.to_string(),
            domain_env.get_list_state_functions(),
        );
        env.insert(
            RAE_SYMBOL_TYPE.to_string(),
            domain_env.get_map_symbol_type(),
        );
        env.insert(
            RAE_METHOD_PARAMETERS_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );
        env.insert(RAE_TASK_METHODS_MAP.to_string(), map_task_method.into());

        env.insert(
            RAE_METHOD_PRE_CONDITIONS_MAP.to_string(),
            map_method_pre_conditions.into(),
        );
        env.insert(
            RAE_METHODS_EFFECTS_MAP.to_string(),
            map_method_effects.into(),
        );

        env
    }
}

pub struct RAEEnv {
    pub job_receiver: Option<Receiver<Job>>,
    pub status_watcher: Option<Receiver<usize>>,
    pub agenda: Agenda,
    pub actions_progress: ActionsProgress,
    pub state: RAEState,
    pub env: LEnv,
    pub domain_env: DomainEnv,
    pub ctxs: ContextCollection,
    pub init_lisp: InitLisp,
}

impl RAEEnv {
    pub fn get_eval_env(&self) -> LEnv {
        //TODO: modify it to build the env from methods structs and other structs
        let mut env: LEnv = self.domain_env.clone().into();
        env.set_outer(self.env.clone());
        env
    }
}

//pub const RAE_MAP_TYPE:&str = "rae-map-type";

impl Default for RAEEnv {
    fn default() -> Self {
        let (env, ctxs, init_lisp) = LEnv::root();
        /*let mut domain_env = LEnv::empty();
        domain_env.insert(RAE_ACTION_LIST.to_string(), LValue::List(vec![]));
        domain_env.insert(RAE_METHOD_LIST.to_string(), LValue::List(vec![]));
        domain_env.insert(RAE_TASK_LIST.to_string(), LValue::List(vec![]));
        domain_env.insert(RAE_STATE_FUNCTION_LIST.to_string(), LValue::List(vec![]));
        domain_env.insert(RAE_SYMBOL_TYPE.to_string(), LValue::Map(Default::default()));
        domain_env.insert(
            RAE_METHOD_PARAMETERS_GENERATOR_MAP.to_string(),
            LValue::Map(Default::default()),
        );
        domain_env.insert(
            RAE_TASK_METHODS_MAP.to_string(),
            LValue::Map(Default::default()),
        );
        domain_env.insert(
            RAE_METHOD_SCORE_GENERATOR_MAP.to_string(),
            LValue::Map(Default::default()),
        );*/
        Self {
            job_receiver: None,
            agenda: Default::default(),
            actions_progress: Default::default(),
            state: Default::default(),
            env,
            domain_env: Default::default(),
            ctxs,
            init_lisp,
            status_watcher: None,
        }
    }
}

impl RAEEnv {
    #[allow(clippy::field_reassign_with_default)]
    pub fn new(
        job_receiver: Option<Receiver<Job>>,
        status_watcher: Option<Receiver<usize>>,
    ) -> Self {
        let mut env = RAEEnv::default();
        env.job_receiver = job_receiver;
        env.status_watcher = status_watcher;
        env
    }
}

impl RAEEnv {
    pub fn add_action(&mut self, label: String, value: LValue) -> Result<(), LError> {
        self.domain_env.add_action(label, value);

        /*self.insert(label.clone(), value)?;
        let action_list = self.domain_env.get_symbol(RAE_ACTION_LIST).unwrap();
        if let LValue::List(mut list) = action_list {
            list.push(LValue::Symbol(label.clone()));
            self.domain_env
                .set(RAE_ACTION_LIST.to_string(), list.into())
                .expect("list of action should be already defined in environment");
        }

        let symbol_type = self.domain_env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
        if let LValue::Map(mut map) = symbol_type {
            map.insert(
                LValue::Symbol(label),
                LValue::Symbol(ACTION_TYPE.to_string()),
            );
            self.domain_env
                .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                .expect("map of symbol type should be already defined in environment")
        }*/
        Ok(())
    }

    pub fn add_state_function(&mut self, label: String, value: LValue) -> Result<(), LError> {
        self.domain_env.add_state_function(label, value);

        /*self.insert(label.clone(), value)?;
        let state_function_list = self.domain_env.get_symbol(RAE_STATE_FUNCTION_LIST).unwrap();
        if let LValue::List(mut list) = state_function_list {
            list.push(LValue::Symbol(label.clone()));
            self.domain_env
                .set(RAE_STATE_FUNCTION_LIST.to_string(), list.into())
                .expect("list of state function should be already defined in environment");
        }

        let symbol_type = self.domain_env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
        if let LValue::Map(mut map) = symbol_type {
            map.insert(
                LValue::Symbol(label),
                LValue::Symbol(STATE_FUNCTION_TYPE.to_string()),
            );
            self.domain_env
                .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                .expect("map of symbol type should be already defined in environment")
        }*/
        Ok(())
    }

    pub fn add_task(&mut self, label: String, body: LValue) -> Result<(), LError> {
        self.domain_env.add_task(label, Task::new(body));

        /*self.insert(label.clone(), value)?;
        let task_list = self.domain_env.get_symbol(RAE_TASK_LIST).unwrap();
        if let LValue::List(mut list) = task_list {
            list.push(LValue::Symbol(label.clone()));
            self.domain_env
                .set(RAE_TASK_LIST.to_string(), list.into())
                .expect("list of task should be already defined in environment");
        }
        let mut map: im::HashMap<LValue, LValue> = self
            .domain_env
            .get_symbol(RAE_TASK_METHODS_MAP)
            .unwrap()
            .try_into()
            .unwrap();
        map.insert(LValue::Symbol(label.clone()), LValue::Nil);

        self.domain_env
            .set(RAE_TASK_METHODS_MAP.to_string(), map.into())?;

        let symbol_type = self.domain_env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
        if let LValue::Map(mut map) = symbol_type {
            map.insert(LValue::Symbol(label), LValue::Symbol(TASK_TYPE.to_string()));
            self.domain_env
                .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                .expect("map of symbol type should be already defined in environment")
        }*/
        Ok(())
    }

    pub fn add_method(
        &mut self,
        method_label: String,
        task_label: String,
        conds: LValue,
        effects: LValue,
        generator: LValue,
        body: LValue,
    ) -> Result<(), LError> {
        let method = Method::new(task_label, conds, effects, generator, body);

        self.domain_env.add_method(method_label, method);

        /*
        self.insert(method_label.clone(), value)?;
        let method_list = self.domain_env.get_symbol(RAE_METHOD_LIST).unwrap();
        if let LValue::List(mut list) = method_list {
            list.push(LValue::Symbol(method_label.clone()));
            self.domain_env
                .set(RAE_METHOD_LIST.to_string(), list.into())
                .expect("list of method should be already defined in environment");

            self.add_method_to_task(task_label, method_label.clone())?;
            let symbol_type = self.domain_env.get_symbol(RAE_SYMBOL_TYPE).unwrap();
            if let LValue::Map(mut map) = symbol_type {
                map.insert(
                    LValue::Symbol(method_label),
                    LValue::Symbol(METHOD_TYPE.to_string()),
                );
                self.domain_env
                    .set(RAE_SYMBOL_TYPE.to_string(), map.into())
                    .expect("map of symbol type should be already defined in environment")
            }
        }*/

        Ok(())
    }

    /*pub fn insert(&mut self, label: String, value: LValue) -> Result<(), LError> {
        match self.domain_env.get_symbol(&label) {
            None => {
                self.domain_env.insert(label, value);
                Ok(())
            }
            Some(_) => Err(SpecialError(
                "RAEEnv::insert",
                format!("Symbol \"{}\" already defined.", label),
            )),
        }
    }*/

    /*pub fn add_method_to_task(
        &mut self,
        task_label: String,
        method_label: String,
    ) -> Result<(), LError> {
        let mut map: im::HashMap<LValue, LValue> = self
            .domain_env
            .get_symbol(RAE_TASK_METHODS_MAP)
            .unwrap()
            .try_into()
            .unwrap();

        let list = match map.get(&LValue::Symbol(task_label.clone())) {
            None => {
                return Err(SpecialError(
                    "RAEEnv::add_method_to_task",
                    format!(
                        "task \"{}\" is not defined, cannot add method to it.",
                        task_label
                    ),
                ))
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
        self.domain_env
            .set(RAE_TASK_METHODS_MAP.into(), map.into())
            .expect("should not return an error");
        Ok(())
    }

    pub fn add_parameter_generator_to_method(
        &mut self,
        method_label: LValue,
        lambda_generator: LValue,
    ) -> Result<(), LError> {
        //println!("setting parameters to method");
        let mut map: im::HashMap<LValue, LValue> = self
            .domain_env
            .get_symbol(RAE_METHOD_PARAMETERS_GENERATOR_MAP)
            .unwrap()
            .try_into()
            .unwrap();

        map.insert(method_label, lambda_generator);
        self.domain_env
            .set(RAE_METHOD_PARAMETERS_GENERATOR_MAP.into(), map.into())
            .expect("should not return an error");
        Ok(())
    }

    pub fn add_score_generator_to_task(
        &mut self,
        method_label: LValue,
        lambda_generator: LValue,
    ) -> Result<(), LError> {
        let mut map: im::HashMap<LValue, LValue> = self
            .domain_env
            .get_symbol(RAE_METHOD_SCORE_GENERATOR_MAP)
            .unwrap()
            .try_into()
            .unwrap();

        map.insert(method_label, lambda_generator);
        self.domain_env
            .set(RAE_METHOD_SCORE_GENERATOR_MAP.into(), map.into())
            .expect("should not return an error");
        Ok(())
    }*/

    pub fn get_methods_from_task(&self, task: &LValue) -> Result<LValue, LError> {
        let label: String = task.try_into()?;
        match self.domain_env.get_tasks().get(&*label) {
            None => Ok(LValue::Nil),
            Some(task) => Ok(task.methods.clone().into()),
        }
    }

    /*pub fn get_element(&self, label: &str) -> Option<LValue> {
        self.env.get_symbol(label)
    }

    pub fn pretty_debug(&self, key: Option<String>) -> String {
        let mut string = String::new();
        if let Some(_label) = key {
            todo!()
        } else {
            let state_function_symbol =
                self.domain_env.get_symbol(RAE_STATE_FUNCTION_LIST).unwrap();
            let state_function_list: Vec<LValue> = state_function_symbol.try_into().unwrap();

            let action_list_symbol = self.domain_env.get_symbol(RAE_ACTION_LIST).unwrap();
            let action_list: Vec<LValue> = action_list_symbol.try_into().unwrap();
            let task_list_symbol = self.domain_env.get_symbol(RAE_TASK_LIST).unwrap();
            let task_list: Vec<LValue> = task_list_symbol.try_into().unwrap();
            let map_task_method: im::HashMap<LValue, LValue> = self
                .domain_env
                .get_symbol(RAE_TASK_METHODS_MAP)
                .unwrap()
                .try_into()
                .unwrap();
            let method_parameters_map: im::HashMap<LValue, LValue> = self
                .domain_env
                .get_symbol(RAE_METHOD_PARAMETERS_GENERATOR_MAP)
                .unwrap()
                .try_into()
                .unwrap();
            string.push_str("RAEEnv: \n");
            string.push_str("\tState Function(s)\n");
            for state_function in state_function_list {
                let state_function_label: String = state_function.try_into().unwrap();
                let state_function_body =
                    self.domain_env.get_symbol(&state_function_label).unwrap();
                string.push_str(
                    format!("\t\t- {}: {}\n", state_function_label, state_function_body).as_str(),
                );
            }
            string.push('\n');
            string.push_str("\t*Action(s): \n");
            for action in action_list {
                let action_label: String = action.try_into().unwrap();
                let action_body = self.domain_env.get_symbol(&action_label).unwrap();
                string.push_str(format!("\t\t- {}: {}\n", action_label, action_body).as_str());
            }
            string.push('\n');
            string.push_str("\t*Task(s): \n");
            for task in task_list {
                let task_label: String = task.clone().try_into().unwrap();
                let task_body = self.domain_env.get_symbol(&task_label).unwrap();
                string.push_str(format!("\t\t-{}: {}\n", task_label, task_body).as_str());
                string.push_str("\t\t*Method(s): \n");
                let methods = map_task_method.get(&task);
                match methods {
                    None => string.push_str("\t\t\t nil\n"),
                    Some(methods) => {
                        let methods: Vec<LValue> = methods.try_into().unwrap();
                        for m in methods {
                            let method_label: String = m.try_into().unwrap();
                            let method = self.domain_env.get_symbol(&method_label).unwrap();
                            let parameters =
                                match method_parameters_map.get(&method_label.clone().into()) {
                                    None => LValue::Nil,
                                    Some(s) => s.clone(),
                                };
                            string.push_str(
                                format!(
                                    "\t\t\t-{}: \n -body: {}\n -parameters: {}\n",
                                    method_label, method, parameters
                                )
                                .as_str(),
                            )
                        }
                    }
                }
            }
        }
        string
    }*/
}
