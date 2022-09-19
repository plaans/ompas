use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::LEnv;

pub const CTX_TASK: &str = "CtxTask";
pub const DEFINE_PARENT_TASK: &str = "define-parent-task";

#[derive(Default, Copy, Clone)]
pub struct CtxTask {
    pub parent_id: Option<usize>,
}

impl CtxTask {
    pub fn new(parent_id: usize) -> Self {
        Self {
            parent_id: Some(parent_id),
        }
    }
}

#[scheme_fn]
pub fn define_parent_task(env: &mut LEnv, parent_id: usize) {
    env.import_context(Context::new(CtxTask::new(parent_id)), CTX_TASK);
}
