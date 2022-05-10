use crate::agenda::Agenda;
use crate::domain::action::Action;
use crate::domain::method::Method;
use crate::domain::parameters::Parameters;
use crate::domain::state_function::StateFunction;
use crate::domain::task::Task;
use crate::domain::RAEDomain;
use crate::job::Job;
use crate::state::world_state::WorldState;
use sompas_core::get_root_env;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::convert::TryInto;
use tokio::sync::mpsc::Receiver;

pub const RAE_TASK_METHODS_MAP: &str = "rae-task-methods-map";
pub const RAE_TASK_LIST: &str = "rae-task-list";
pub const RAE_METHOD_LIST: &str = "rae-methods-list";
pub const RAE_ACTION_LIST: &str = "rae-actions-list";
pub const RAE_STATE_FUNCTION_LIST: &str = "rae-state-function-list";
pub const RAE_SYMBOL_TYPE: &str = "rae-symbol-type";
pub const RAE_METHOD_TYPES_MAP: &str = "rae-method-types-map";
pub const RAE_METHOD_SCORE_MAP: &str = "rae-method-score-map";
pub const RAE_METHOD_GENERATOR_MAP: &str = "rae-method-generator-map";
pub const RAE_METHOD_PRE_CONDITIONS_MAP: &str = "rae-method-pre-conditions-map";
pub const RAE_ACTION_MODEL_MAP: &str = "rae-action-model-map";
pub const ACTION_TYPE: &str = "action_type";
pub const TASK_TYPE: &str = "task_type";
pub const METHOD_TYPE: &str = "method_type";
pub const STATE_FUNCTION_TYPE: &str = "state_function_type";
pub const LAMBDA_TYPE: &str = "lambda_type";

pub const TUPLE_TYPE: &str = "tuple";
pub const TYPE_LIST: &str = "tlist";

pub type TypeId = usize;

pub struct RAEContext {
    pub job_receiver: Option<Receiver<Job>>,
    pub agenda: Agenda,
    pub state: WorldState,
    pub env: LEnv,
    pub domain_env: RAEDomain,
}

impl RAEContext {
    pub async fn get_exec_env(&self) -> LEnv {
        let domain_exec_symbols: LEnvSymbols = self.domain_env.get_exec_env();
        let mut exec_env = self.env.clone();

        exec_env.set_new_top_symbols(domain_exec_symbols);
        exec_env
    }

    pub fn get_sim_env(&self) -> LEnv {
        let symbols: LEnvSymbols = self.domain_env.get_sim_env();
        let mut env = self.env.clone();
        env.set_new_top_symbols(symbols);
        env
    }

    #[allow(clippy::field_reassign_with_default)]
    pub async fn new(job_receiver: Option<Receiver<Job>>) -> Self {
        let env = get_root_env().await;
        Self {
            job_receiver,
            agenda: Default::default(),
            state: Default::default(),
            env,
            domain_env: Default::default(),
        }
    }

    pub fn add_action(&mut self, label: String, value: Action) -> Result<(), LRuntimeError> {
        self.domain_env.add_action(label, value);

        Ok(())
    }

    pub fn add_action_sample_fn(
        &mut self,
        label: String,
        value: LValue,
    ) -> Result<(), LRuntimeError> {
        self.domain_env.add_action_sample_fn(label, value)
    }

    pub fn add_state_function(
        &mut self,
        label: String,
        value: StateFunction,
    ) -> Result<(), LRuntimeError> {
        self.domain_env.add_state_function(label, value);

        Ok(())
    }

    pub fn add_task(
        &mut self,
        label: String,
        body: LValue,
        parameters: Parameters,
    ) -> Result<(), LRuntimeError> {
        self.domain_env
            .add_task(label.clone(), Task::new(label, body, parameters));

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn add_method(
        &mut self,
        method_label: String,
        task_label: String,
        parameters: Parameters,
        conds: LValue,
        score: LValue,
        body: LValue,
    ) -> Result<(), LRuntimeError> {
        let method = Method::new(task_label, parameters, conds, score, body);

        self.domain_env.add_method(method_label, method)?;

        Ok(())
    }

    pub fn add_lambda(&mut self, label: String, value: LValue) {
        self.domain_env.add_lambda(label, value);
    }

    pub fn get_methods_from_task(&self, task: &LValue) -> Result<LValue, LRuntimeError> {
        let label: String = task.try_into()?;
        match self.domain_env.get_tasks().get(&*label) {
            None => Ok(LValue::Nil),
            Some(task) => Ok(task.methods.clone().into()),
        }
    }

    pub fn add_type(&mut self, t: String, p: Option<String>) {
        self.domain_env.add_type(t, p)
    }

    pub fn get_parents(&self, t: &str) -> Vec<String> {
        self.domain_env.get_parents(t)
    }

    pub fn format_type_hierarchy(&self) -> String {
        self.domain_env.types.format_hierarchy()
    }
}
