use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::event::Event;
use crate::model::acting_domain::method::Method;
use crate::model::acting_domain::model::{ActingModel, ModelKind};
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::acting_domain::task::Task;
use crate::model::acting_domain::OMPASDomain;
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use sompas_structs::lenv::{LEnv, LEnvSymbols};
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct DomainManager {
    acting_domain: Arc<RwLock<OMPASDomain>>,
}

impl DomainManager {
    pub async fn init_planning_domain(
        &self,
        env: &LEnv,
        state: WorldStateSnapshot,
        st: &RefSymTable,
    ) {
        self.acting_domain
            .write()
            .await
            .init_acting_model_collection(env, state, st)
            .await;
    }
}

impl From<OMPASDomain> for DomainManager {
    fn from(value: OMPASDomain) -> Self {
        Self {
            acting_domain: Arc::new(RwLock::new(value)),
        }
    }
}

impl DomainManager {
    pub async fn get_inner(&self) -> OMPASDomain {
        self.acting_domain.read().await.clone()
    }

    pub async fn get_exec_env(&self) -> LEnvSymbols {
        self.acting_domain.read().await.get_exec_env()
    }

    pub async fn get_convert_env(&self) -> LEnvSymbols {
        self.acting_domain.read().await.get_convert_env()
    }

    pub async fn is_task(&self, label: &str) -> bool {
        self.acting_domain.read().await.is_task(label)
    }

    pub async fn is_command(&self, label: &str) -> bool {
        self.acting_domain.read().await.is_command(label)
    }

    pub async fn get_list_tasks(&self) -> LValue {
        self.acting_domain.read().await.get_list_tasks()
    }

    pub async fn get_list_methods(&self) -> LValue {
        self.acting_domain.read().await.get_list_methods()
    }

    pub async fn get_list_state_functions(&self) -> LValue {
        self.acting_domain.read().await.get_list_state_functions()
    }

    pub async fn get_list_commands(&self) -> LValue {
        self.acting_domain.read().await.get_list_commands()
    }

    pub async fn format(&self) -> String {
        self.acting_domain.read().await.to_string()
    }

    pub async fn get_element_description(&self, label: &str) -> String {
        self.acting_domain
            .read()
            .await
            .get_element_description(label)
    }

    pub async fn add_state_function(
        &self,
        label: String,
        value: StateFunction,
    ) -> Result<(), LRuntimeError> {
        self.acting_domain
            .write()
            .await
            .add_state_function(label, value)
    }

    pub async fn add_command(&self, label: String, value: Command) -> Result<(), LRuntimeError> {
        self.acting_domain.write().await.add_command(label, value)
    }

    pub async fn add_task(&self, label: String, value: Task) -> Result<(), LRuntimeError> {
        self.acting_domain.write().await.add_task(label, value)
    }

    pub async fn add_method(&self, label: String, value: Method) -> Result<(), LRuntimeError> {
        self.acting_domain.write().await.add_method(label, value)
    }

    pub async fn add_command_model(
        &self,
        label: String,
        value: LValue,
        kind: ModelKind,
    ) -> Result<(), LRuntimeError> {
        self.acting_domain
            .write()
            .await
            .add_command_model(label, value, kind)
    }

    pub async fn add_task_model(
        &self,
        label: String,
        value: LValue,
        kind: ModelKind,
    ) -> Result<(), LRuntimeError> {
        self.acting_domain
            .write()
            .await
            .add_task_model(label, value, kind)
    }
    pub async fn add_env(&self, label: String, value: LValue) {
        self.acting_domain.write().await.add_env(label, value)
    }

    pub async fn add_lambda(&self, label: String, value: LValue) {
        self.acting_domain.write().await.add_lambda(label, value)
    }

    pub async fn add_init(&self, body: LValue) {
        self.acting_domain.write().await.add_init(body)
    }

    pub async fn add_event(&self, label: String, value: Event) {
        self.acting_domain.write().await.events.insert(label, value);
    }

    pub async fn get_event(&self, label: &str) -> Option<Event> {
        self.acting_domain.read().await.events.get(label).cloned()
    }

    pub async fn get_events(&self) -> HashMap<String, Event> {
        self.acting_domain.read().await.events.clone()
    }

    pub async fn remove_event(&self, label: &str) {
        self.acting_domain.write().await.events.remove(label);
    }

    pub async fn get_command(&self, label: &str) -> Option<Command> {
        self.acting_domain.read().await.commands.get(label).cloned()
    }

    pub async fn get_method(&self, label: &str) -> Option<Method> {
        self.acting_domain.read().await.methods.get(label).cloned()
    }

    pub async fn get_task(&self, label: &str) -> Option<Task> {
        self.acting_domain.read().await.tasks.get(label).cloned()
    }

    pub async fn get_state_function(&self, label: &str) -> Option<StateFunction> {
        self.acting_domain
            .read()
            .await
            .state_functions
            .get(label)
            .cloned()
    }

    pub async fn get_init(&self) -> LValue {
        self.acting_domain.read().await.get_init().clone()
    }

    pub async fn remove_command(&self, label: &str) {
        self.acting_domain.write().await.commands.remove(label);
    }

    pub async fn remove_state_function(&self, label: &str) {
        self.acting_domain
            .write()
            .await
            .state_functions
            .remove(label);
    }

    pub async fn remove_task(&self, label: &str) {
        let mut locked = self.acting_domain.write().await;
        let task = locked.tasks.remove(label);
        if let Some(task) = task {
            for m in task.get_methods() {
                locked.methods.remove(m);
            }
        }
    }

    pub async fn remove_method(&self, label: &str) {
        let mut inner = self.acting_domain.write().await;
        if let Some(method) = inner.methods.remove(label) {
            inner
                .tasks
                .get_mut(&method.task_label)
                .unwrap()
                .remove_method(label);
        }
    }
}

//Methods linked to the acting model collection
impl DomainManager {
    pub async fn get_command_model(&self, label: &str) -> Option<ActingModel> {
        self.acting_domain
            .read()
            .await
            .acting_model_collection
            .as_ref()
            .and_then(|amc| amc.get_command_model(&label).cloned())
    }

    pub async fn get_task_model(&self, label: &str) -> Option<ActingModel> {
        self.acting_domain
            .read()
            .await
            .acting_model_collection
            .as_ref()
            .and_then(|amc| amc.get_task_model(&label).cloned())
    }

    pub async fn get_method_model(&self, label: &str) -> Option<ActingModel> {
        self.acting_domain
            .read()
            .await
            .acting_model_collection
            .as_ref()
            .and_then(|amc| amc.get_method_model(&label).cloned())
    }

    pub async fn try_instantiate_task(
        &self,
        args: &[Option<Cst>],
        st: &RefSymTable,
    ) -> Option<ActingModel> {
        self.acting_domain
            .read()
            .await
            .acting_model_collection
            .as_ref()
            .and_then(|amc| amc.try_instantiate_task(args, st))
    }

    pub async fn try_instantiate_command(
        &self,
        args: &[Option<Cst>],
        st: &RefSymTable,
    ) -> Option<ActingModel> {
        self.acting_domain
            .read()
            .await
            .acting_model_collection
            .as_ref()
            .and_then(|amc| amc.try_instantiate_command(args, st))
    }

    pub async fn try_instantiate_method(
        &self,
        args: &[Option<Cst>],
        st: &RefSymTable,
    ) -> Option<ActingModel> {
        self.acting_domain
            .read()
            .await
            .acting_model_collection
            .as_ref()
            .and_then(|amc| amc.try_instantiate_method(args, st))
    }
}
