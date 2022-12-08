use ompas_rae_language::exec::task::*;
use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;

pub const CTX_TASK: &str = "CtxTask";

#[derive(Default, Copy, Clone)]
pub struct ModTask {
    pub parent_id: Option<usize>,
}

impl ModTask {
    pub fn new(parent_id: usize) -> Self {
        Self {
            parent_id: Some(parent_id),
        }
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
        module.add_mut_fn(
            DEFINE_PARENT_TASK,
            define_parent_task,
            DOC_DEFINE_PARENT_TASK,
        );
        module
    }
}

#[scheme_fn]
pub fn define_parent_task(env: &mut LEnv, parent_id: usize) {
    env.update_context(ModTask::new(parent_id));
}
