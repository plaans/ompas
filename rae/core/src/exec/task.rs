use ompas_rae_language::exec::task::*;
use ompas_rae_structs::ActionId;
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;

pub const CTX_TASK: &str = "CtxTask";

#[derive(Default, Clone)]
pub struct ModTask {
    task_id: Arc<RwLock<Option<usize>>>,
}

impl ModTask {
    pub fn new(task_id: usize) -> Self {
        Self {
            task_id: Arc::new(RwLock::new(Some(task_id))),
        }
    }

    pub fn get_pointer(&self) -> Arc<RwLock<Option<usize>>> {
        self.task_id.clone()
    }

    pub async fn get_task_id(&self) -> Option<ActionId> {
        *self.task_id.read().await
    }

    pub async fn set_task_id(&self, id: usize) {
        *self.task_id.write().await = Some(id)
    }
}

impl From<ModTask> for Context {
    fn from(m: ModTask) -> Self {
        Context::new(m, MOD_TASK)
    }
}

impl From<ModTask> for LModule {
    fn from(m: ModTask) -> Self {
        let mut module = LModule::new(m, MOD_TASK, DOC_MOD_TASK);
        module.add_async_mut_fn(DEFINE_TASK_ID, define_task_id, DOC_DEFINE_TASK_ID);
        module.add_async_fn(GET_TASK_ID, get_task_id, DOC_GET_TASK_ID, false);
        module
    }
}

#[async_scheme_fn]
pub async fn define_task_id(env: &mut LEnv, task_id: usize) {
    let ctx = env.get_context::<ModTask>(MOD_TASK).unwrap();
    if ctx.task_id.write().await.is_some() {
        env.update_context(ModTask::new(task_id));
    } else {
        *ctx.task_id.write().await = Some(task_id);
    }
}

#[async_scheme_fn]
pub async fn get_task_id(env: &LEnv) -> LValue {
    let ctx = env.get_context::<ModTask>(MOD_TASK).unwrap();
    match ctx.get_task_id().await {
        Some(task_id) => task_id.into(),
        None => LValue::Nil,
    }
}
