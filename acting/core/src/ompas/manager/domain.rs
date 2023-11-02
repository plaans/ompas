use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::method::Method;
use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::acting_domain::task::Task;
use crate::model::acting_domain::OMPASDomain;
use sompas_structs::lenv::LEnvSymbols;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Default, Clone)]
pub struct DomainManager {
    inner: Arc<RwLock<OMPASDomain>>,
}

impl From<OMPASDomain> for DomainManager {
    fn from(value: OMPASDomain) -> Self {
        Self {
            inner: Arc::new(RwLock::new(value)),
        }
    }
}

impl DomainManager {
    pub async fn get_inner(&self) -> OMPASDomain {
        self.inner.read().await.clone()
    }

    pub async fn get_exec_env(&self) -> LEnvSymbols {
        self.inner.read().await.get_exec_env()
    }

    pub async fn get_convert_env(&self) -> LEnvSymbols {
        self.inner.read().await.get_convert_env()
    }

    pub async fn is_task(&self, label: &str) -> bool {
        self.inner.read().await.is_task(label)
    }

    pub async fn is_command(&self, label: &str) -> bool {
        self.inner.read().await.is_command(label)
    }

    pub async fn get_list_tasks(&self) -> LValue {
        self.inner.read().await.get_list_tasks()
    }

    pub async fn get_list_methods(&self) -> LValue {
        self.inner.read().await.get_list_methods()
    }

    pub async fn get_list_state_functions(&self) -> LValue {
        self.inner.read().await.get_list_state_functions()
    }

    pub async fn get_list_commands(&self) -> LValue {
        self.inner.read().await.get_list_commands()
    }

    pub async fn format(&self) -> String {
        self.inner.read().await.to_string()
    }

    pub async fn get_element_description(&self, label: &str) -> String {
        self.inner.read().await.get_element_description(label)
    }

    pub async fn add_state_function(
        &self,
        label: String,
        value: StateFunction,
    ) -> Result<(), LRuntimeError> {
        self.inner.write().await.add_state_function(label, value)
    }

    pub async fn add_command(&self, label: String, value: Command) -> Result<(), LRuntimeError> {
        self.inner.write().await.add_command(label, value)
    }

    pub async fn add_task(&self, label: String, value: Task) -> Result<(), LRuntimeError> {
        self.inner.write().await.add_task(label, value)
    }

    pub async fn add_method(&self, label: String, value: Method) -> Result<(), LRuntimeError> {
        self.inner.write().await.add_method(label, value)
    }

    pub async fn add_command_model(
        &self,
        label: String,
        value: LValue,
        kind: ModelKind,
    ) -> Result<(), LRuntimeError> {
        self.inner
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
        self.inner.write().await.add_task_model(label, value, kind)
    }
    pub async fn add_env(&self, label: String, value: LValue) {
        self.inner.write().await.add_env(label, value)
    }

    pub async fn add_lambda(&self, label: String, value: LValue) {
        self.inner.write().await.add_lambda(label, value)
    }

    pub async fn get_command(&self, label: &str) -> Option<Command> {
        self.inner.read().await.commands.get(label).cloned()
    }

    pub async fn get_method(&self, label: &str) -> Option<Method> {
        self.inner.read().await.methods.get(label).cloned()
    }

    pub async fn get_task(&self, label: &str) -> Option<Task> {
        self.inner.read().await.tasks.get(label).cloned()
    }

    pub async fn remove_command(&self, label: &str) {
        self.inner.write().await.commands.remove(label);
    }

    pub async fn remove_state_function(&self, label: &str) {
        self.inner.write().await.state_functions.remove(label);
    }

    pub async fn remove_task(&self, label: &str) {
        let mut locked = self.inner.write().await;
        let task = locked.tasks.remove(label);
        if let Some(task) = task {
            for m in task.get_methods() {
                locked.methods.remove(m);
            }
        }
    }

    pub async fn remove_method(&self, label: &str) {
        let mut inner = self.inner.write().await;
        if let Some(method) = inner.methods.remove(label) {
            inner.tasks.get_mut(&method.task_label).unwrap().remove_method(label);
        }
    }
}
