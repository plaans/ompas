use ompas_language::exec::context::*;
use ompas_middleware::ProcessId;
use ompas_structs::supervisor::process_ref::{Label, MethodLabel, ProcessRef};
use ompas_structs::supervisor::ActingProcessId;
use ompas_structs::ActionId;
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;

pub const CTX_ACTING_CONTEXT: &str = "CtxActingContext";

#[derive(Default, Clone)]
pub struct ModContext {
    pub process_ref: ProcessRef,
}

impl ModContext {
    pub fn new(process_ref: ProcessRef) -> Self {
        Self { process_ref }
    }
}

impl From<ModContext> for Context {
    fn from(m: ModContext) -> Self {
        Context::new(m, MOD_CONTEXT)
    }
}

impl From<ModContext> for LModule {
    fn from(m: ModContext) -> Self {
        let mut module = LModule::new(m, MOD_CONTEXT, DOC_MOD_CONTEXT);
        module.add_async_mut_fn(DOC_DEF_PROCESS_ID, def_process_id, DOC_DEF_PROCESS_ID);
        module.add_async_mut_fn(DEF_LABEL, def_label, DOC_DEF_LABEL);
        module
    }
}

pub async fn def_process_id(env: &mut LEnv, id: ActingProcessId) {
    env.update_context(ModContext::new(ProcessRef::Id(id)))
}

pub const ARBITRARY: &str = ompas_language::exec::ARBITRARY;
pub const ACQUIRE: &str = ompas_language::exec::resource::ACQUIRE;
pub const COMMAND: &str = "command";
pub const SUBTASK: &str = "subtask";

#[async_scheme_fn]
pub async fn def_label(env: &mut LEnv, kind: String, id: usize) {
    let ctx = env.get_context::<ModContext>(MOD_CONTEXT).unwrap().clone();
    let label = match kind.as_str() {
        ARBITRARY => MethodLabel::Arbitrary(id),
        ACQUIRE => MethodLabel::Acquire(id),
        COMMAND => MethodLabel::Command(id),
        SUBTASK => MethodLabel::Subtask(id),
    };

    let process_ref = match ctx.process_ref {
        ProcessRef::Id(id) => ProcessRef::Relative(id, label.into()),
        ProcessRef::Relative(id, labels) => {
            let mut new_labels = labels.clone();
            match labels.last().unwrap() {
                Label::Method(_) | Label::MethodProcess(MethodLabel::Subtask(_)) => {
                    new_labels.push(label.into());
                    ProcessRef::Relative(id, new_labels)
                }
                _ => {
                    new_labels.pop();
                    new_labels.push(label.into());
                    ProcessRef::Relative(id, new_labels);
                }
            }
        }
    };

    env.update_context(ModContext::new(process_ref))
}
