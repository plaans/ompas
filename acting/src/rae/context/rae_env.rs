use crate::rae::context::actions_progress::ActionsProgress;
use crate::rae::context::agenda::Agenda;
use crate::rae::context::rae_state::RAEState;
use crate::rae::module::init_simu_env;
use crate::rae::module::mod_rae_exec::{Job, JobId};
use crate::rae::module::mod_rae_sim_interface::CtxRaeSimInterface;
use crate::rae::TOKIO_CHANNEL_SIZE;
use im::HashMap;
use log::Level::Debug;
use ompas_lisp::core::ImportType::WithoutPrefix;
use ompas_lisp::core::{import, ContextCollection, LEnv};
use ompas_lisp::structs::LCoreOperator::Define;
use ompas_lisp::structs::LError::SpecialError;
use ompas_lisp::structs::{InitLisp, LError, LFn, LLambda, LValue};
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
pub const RAE_METHOD_TYPES_MAP: &str = "rae-method-types-map";
pub const RAE_METHOD_SCORE_MAP: &str = "rae-method-score-map";
pub const RAE_METHOD_GENERATOR_MAP: &str = "rae-method-generator-map";
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
    lambda_score: LValue,
    types: Vec<String>,
    lambda_body: LValue,
}

impl Method {
    pub fn new(
        task_label: String,
        types: Vec<String>,
        conds: LValue,
        effects: LValue,
        score: LValue,
        body: LValue,
    ) -> Self {
        Self {
            task_label,
            lambda_pre_conditions: conds,
            lambda_effects: effects,
            lambda_score: score,
            types,
            lambda_body: body,
        }
    }
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "-task: {}\n\
            -types: {}\n\
            -pre-conditions: {}\n\
            -effects: {}\n\
            -score: {}\n\
            -body: {}\n",
            self.task_label,
            {
                let mut str = '('.to_string();
                for (i, t) in self.types.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    str.push_str(t);
                }
                str.push(')');
                str
            },
            self.lambda_pre_conditions
                .pretty_print("pre-conditions: ".len()),
            self.lambda_effects.pretty_print("effects: ".len()),
            self.lambda_score.pretty_print("score: ".len()),
            self.lambda_body.pretty_print("body: ".len())
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
            self.body.pretty_print("body: ".len()),
            str_methods
        )
    }
}

#[derive(Debug, Clone)]
pub struct DualExpression {
    exec: LValue,
    sim: LValue,
}

impl DualExpression {
    pub fn new(exec: LValue, sim: LValue) -> Self {
        Self { exec, sim }
    }
}

impl Display for DualExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "exec: {}\nsimu: {}",
            self.exec.pretty_print("exec: ".len()),
            self.sim.pretty_print("simu: ".len())
        )
    }
}

pub type Action = DualExpression;
pub type StateFunction = DualExpression;

#[derive(Default, Debug, Clone)]
pub struct DomainEnv {
    tasks: HashMap<String, Task>,
    methods: HashMap<String, Method>,
    state_functions: HashMap<String, StateFunction>,
    actions: HashMap<String, Action>,
    lambdas: HashMap<String, LValue>,
    map_symbol_type: HashMap<LValue, LValue>,
}

impl DomainEnv {
    pub fn get_element_description(&self, label: String) -> String {
        match self.map_symbol_type.get(&label.clone().into()) {
            None => format!("Keyword {} is not defined in the domain.", label),
            Some(symbol_type) => {
                if let LValue::Symbol(s) = symbol_type {
                    match s.as_str() {
                        TASK_TYPE => self.tasks.get(&label).unwrap().to_string(),
                        METHOD_TYPE => self.methods.get(&label).unwrap().to_string(),
                        STATE_FUNCTION_TYPE => {
                            self.state_functions.get(&label).unwrap().to_string()
                        }
                        LAMBDA_TYPE => self.lambdas.get(&label).unwrap().to_string(),
                        ACTION_TYPE => self.actions.get(&label).unwrap().to_string(),
                        _ => panic!("There should no other type of symbol_type"),
                    }
                } else {
                    panic!("A symbol_type must be LValue::Symbol")
                }
            }
        }
    }
}

//Getter
impl DomainEnv {
    pub fn get_tasks(&self) -> &HashMap<String, Task> {
        &self.tasks
    }

    pub fn get_methods(&self) -> &HashMap<String, Method> {
        &self.methods
    }

    pub fn get_state_functions(&self) -> &HashMap<String, StateFunction> {
        &self.state_functions
    }

    pub fn get_actions(&self) -> &HashMap<String, Action> {
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

    pub fn add_state_function(&mut self, label: String, value: StateFunction) {
        self.state_functions.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label.into(), STATE_FUNCTION_TYPE.into());
    }

    pub fn add_action(&mut self, label: String, value: Action) {
        self.actions.insert(label.clone(), value);
        self.map_symbol_type
            .insert(label.into(), ACTION_TYPE.into());
    }

    pub fn add_action_sample_fn(&mut self, label: String, value: LValue) -> Result<(), LError> {
        match self.actions.get_mut(&label) {
            None => Err(SpecialError(
                "add_action_sample_fn",
                format!("Action {} is not defined", label),
            )),
            Some(action) => {
                //println!("updating sim of {} with {}", label, value);
                action.sim = value;
                Ok(())
            }
        }
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
        let mut str = "*TASKS:\n".to_string();
        for (label, value) in &self.tasks {
            str.push_str(format!("\t-{}:\n{}\n", label, value).as_str())
        }
        str
    }

    pub fn print_methods(&self) -> String {
        let mut str = "*METHODS:\n".to_string();
        for (label, value) in &self.methods {
            str.push_str(format!("\t-{}:\n{}\n", label, value).as_str())
        }
        str
    }

    pub fn print_state_functions(&self) -> String {
        let mut str = "*STATE-FUNCTIONS:\n\n".to_string();
        for (label, value) in &self.state_functions {
            str.push_str(format!("\t-{}:\n{}\n\n", label, value).as_str())
        }
        str
    }

    pub fn print_actions(&self) -> String {
        let mut str = "*ACTIONS:\n".to_string();
        for (label, value) in &self.actions {
            str.push_str(format!("\t-{}:\n{}\n\n", label, value).as_str())
        }
        str
    }

    pub fn print_lambdas(&self) -> String {
        let mut str = "*LAMBDAS:\n".to_string();
        for (label, value) in &self.lambdas {
            str.push_str(format!("\t-{}:\n{}\n", label, value.pretty_print(0)).as_str())
        }
        str
    }
}

impl Display for DomainEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "*** Domain defined in RAE ***\n".to_string();

        str.push_str(format!("\n{}", self.print_tasks()).as_str());
        str.push_str(format!("\n{}", self.print_methods()).as_str());
        str.push_str(format!("\n{}", self.print_state_functions()).as_str());
        str.push_str(format!("\n{}", self.print_actions()).as_str());
        str.push_str(format!("\n{}", self.print_lambdas()).as_str());

        write!(f, "{}", str)
    }
}

impl DomainEnv {
    pub fn get_exec_env(&self) -> LEnv {
        let mut env = LEnv::empty();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_effects: HashMap<LValue, LValue> = Default::default();
        let mut map_method_score: HashMap<LValue, LValue> = Default::default();
        let mut map_method_types: HashMap<LValue, LValue> = Default::default();

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
            map_method_pre_conditions.insert(label.into(), method.lambda_pre_conditions.clone());
            map_method_effects.insert(label.into(), method.lambda_effects.clone());
            map_method_score.insert(label.into(), method.lambda_score.clone());
            map_method_types.insert(label.into(), method.types.clone().into());
        }

        //Add all actions to env:
        for (label, action) in self.get_actions() {
            env.insert(label.clone(), action.exec.clone());
        }

        //Add all state functions to env:
        for (label, state_function) in self.get_state_functions() {
            env.insert(label.clone(), state_function.exec.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        env.insert(RAE_ACTION_LIST.to_string(), self.get_list_actions());
        env.insert(RAE_METHOD_LIST.to_string(), self.get_list_methods());
        env.insert(RAE_TASK_LIST.to_string(), self.get_list_tasks());
        env.insert(
            RAE_STATE_FUNCTION_LIST.to_string(),
            self.get_list_state_functions(),
        );
        env.insert(RAE_SYMBOL_TYPE.to_string(), self.get_map_symbol_type());
        /*env.insert(
            RAE_METHOD_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );*/
        env.insert(RAE_TASK_METHODS_MAP.to_string(), map_task_method.into());
        env.insert(RAE_METHOD_TYPES_MAP.to_string(), map_method_types.into());
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

    pub fn get_sim_env(&self) -> LEnv {
        let mut env = LEnv::empty();
        let mut map_task_method: HashMap<LValue, LValue> = Default::default();
        let mut map_method_pre_conditions: HashMap<LValue, LValue> = Default::default();
        let mut map_method_effects: HashMap<LValue, LValue> = Default::default();
        let mut map_method_score: HashMap<LValue, LValue> = Default::default();
        let mut map_method_types: HashMap<LValue, LValue> = Default::default();

        //Add all tasks to env:
        for (label, task) in self.get_tasks() {
            env.insert(label.clone(), task.get_body().clone());
            map_task_method.insert(label.into(), task.get_methods().into());
        }

        //Add all methods to env:

        for (label, method) in self.get_methods() {
            env.insert(label.clone(), method.lambda_body.clone());
            map_method_pre_conditions.insert(label.into(), method.lambda_pre_conditions.clone());
            map_method_effects.insert(label.into(), method.lambda_effects.clone());
            map_method_score.insert(label.into(), method.lambda_score.clone());
            map_method_types.insert(label.into(), method.types.clone().into());
        }

        //Add all actions to env:
        for (label, action) in self.get_actions() {
            env.insert(label.clone(), action.sim.clone());
        }

        //Add all state functions to env:
        for (label, state_function) in self.get_state_functions() {
            env.insert(label.clone(), state_function.sim.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in self.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

        env.insert(RAE_ACTION_LIST.to_string(), self.get_list_actions());
        env.insert(RAE_METHOD_LIST.to_string(), self.get_list_methods());
        env.insert(RAE_TASK_LIST.to_string(), self.get_list_tasks());
        env.insert(
            RAE_STATE_FUNCTION_LIST.to_string(),
            self.get_list_state_functions(),
        );
        env.insert(RAE_SYMBOL_TYPE.to_string(), self.get_map_symbol_type());
        /*env.insert(
            RAE_METHOD_GENERATOR_MAP.to_string(),
            map_method_generator.into(),
        );*/
        env.insert(RAE_TASK_METHODS_MAP.to_string(), map_task_method.into());
        env.insert(RAE_METHOD_TYPES_MAP.to_string(), map_method_types.into());

        env.insert(
            RAE_METHOD_PRE_CONDITIONS_MAP.to_string(),
            map_method_pre_conditions.into(),
        );
        env.insert(
            RAE_METHODS_EFFECTS_MAP.to_string(),
            map_method_effects.into(),
        );

        env.insert(RAE_METHOD_SCORE_MAP.to_string(), map_method_score.into());

        env
    }
}

/*impl From<DomainEnv> for LEnv {
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
            env.insert(label.clone(), action.exec.clone());
        }

        //Add all state functions to env:
        for (label, state_function) in domain_env.get_state_functions() {
            env.insert(label.clone(), state_function.exec.clone());
        }

        //Add all lambdas to env:
        for (label, lambda) in domain_env.get_lambdas() {
            env.insert(label.clone(), lambda.clone())
        }

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
            RAE_METHOD_GENERATOR_MAP.to_string(),
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
}*/

pub struct RAEEnv {
    pub job_receiver: Option<Receiver<Job>>,
    pub status_watcher: Option<Receiver<usize>>,
    pub agenda: Agenda,
    pub actions_progress: ActionsProgress,
    pub state: RAEState,
    pub env: LEnv,
    pub domain_env: DomainEnv,
    pub ctxs: ContextCollection,
}

impl RAEEnv {
    pub async fn get_exec_env(&self) -> (LEnv, ContextCollection) {
        let mut domain_exec_env: LEnv = self.domain_env.get_exec_env();
        let domain_sim_env = self.domain_env.get_sim_env();
        let mut exec_env = self.env.clone();
        let mut exec_ctxs = self.ctxs.clone();
        let (sim_env, sim_ctxs) = init_simu_env(None).await;
        let mut ctx_rae_sim_interface = CtxRaeSimInterface::new(sim_env, sim_ctxs);
        ctx_rae_sim_interface.add_domain_sim(domain_sim_env);

        import(
            &mut exec_env,
            &mut exec_ctxs,
            ctx_rae_sim_interface,
            WithoutPrefix,
        )
        .await
        .expect("error loading ctx_rae_sim_interface");

        domain_exec_env.set_outer(exec_env);
        (domain_exec_env, exec_ctxs)
    }

    pub fn get_sim_env(&self) -> LEnv {
        let mut env: LEnv = self.domain_env.get_sim_env();
        env.set_outer(self.env.clone());
        env
    }
}

//pub const RAE_MAP_TYPE:&str = "rae-map-type";

impl RAEEnv {
    #[allow(clippy::field_reassign_with_default)]
    pub async fn new(
        job_receiver: Option<Receiver<Job>>,
        status_watcher: Option<Receiver<usize>>,
    ) -> Self {
        let (env, ctxs) = LEnv::root().await;
        Self {
            job_receiver,
            agenda: Default::default(),
            actions_progress: Default::default(),
            state: Default::default(),
            env,
            domain_env: Default::default(),
            ctxs,
            status_watcher,
        }
    }
}

impl RAEEnv {
    pub fn add_action(&mut self, label: String, value: Action) -> Result<(), LError> {
        self.domain_env.add_action(label, value);

        Ok(())
    }

    pub fn add_action_sample_fn(&mut self, label: String, value: LValue) -> Result<(), LError> {
        self.domain_env.add_action_sample_fn(label, value)
    }

    pub fn add_state_function(
        &mut self,
        label: String,
        value: StateFunction,
    ) -> Result<(), LError> {
        self.domain_env.add_state_function(label, value);

        Ok(())
    }

    pub fn add_task(&mut self, label: String, body: LValue) -> Result<(), LError> {
        self.domain_env.add_task(label, Task::new(body));

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn add_method(
        &mut self,
        method_label: String,
        task_label: String,
        types: Vec<String>,
        conds: LValue,
        effects: LValue,
        score: LValue,
        body: LValue,
    ) -> Result<(), LError> {
        let method = Method::new(task_label, types, conds, effects, score, body);

        self.domain_env.add_method(method_label, method)?;

        Ok(())
    }

    pub fn add_lambda(&mut self, label: String, value: LValue) {
        self.domain_env.add_lambda(label, value);
    }

    pub fn get_methods_from_task(&self, task: &LValue) -> Result<LValue, LError> {
        let label: String = task.try_into()?;
        match self.domain_env.get_tasks().get(&*label) {
            None => Ok(LValue::Nil),
            Some(task) => Ok(task.methods.clone().into()),
        }
    }
}
