use ompas_language::exec::acting_context::*;
use ompas_structs::acting_manager::process::process_ref::{Label, ProcessRef};
use ompas_structs::acting_manager::ActingProcessId;
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;

pub const CTX_ACTING_CONTEXT: &str = "CtxActingContext";

#[derive(Default, Clone)]
pub struct ModActingContext {
    pub process_ref: ProcessRef,
}

impl ModActingContext {
    pub fn new(process_ref: ProcessRef) -> Self {
        Self { process_ref }
    }
}

impl From<ModActingContext> for Context {
    fn from(m: ModActingContext) -> Self {
        Context::new(m, MOD_ACTING_CONTEXT)
    }
}

impl From<ModActingContext> for LModule {
    fn from(m: ModActingContext) -> Self {
        let mut module = LModule::new(m, MOD_ACTING_CONTEXT, DOC_MOD_CONTEXT);
        module.add_async_mut_fn(DEF_PROCESS_ID, def_process_id, DOC_DEF_PROCESS_ID);
        module.add_async_mut_fn(DEF_LABEL, def_label, DOC_DEF_LABEL);
        module
    }
}

#[async_scheme_fn]
pub async fn def_process_id(env: &mut LEnv, id: ActingProcessId) {
    env.update_context(ModActingContext::new(ProcessRef::Id(id)))
}

pub const ARBITRARY: &str = ompas_language::exec::ARBITRARY;
pub const ACQUIRE: &str = ompas_language::exec::resource::ACQUIRE;
pub const COMMAND: &str = "command";
pub const SUBTASK: &str = "subtask";

#[async_scheme_fn]
pub async fn def_label(env: &mut LEnv, kind: String, id: usize) {
    let ctx = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)
        .unwrap()
        .clone();
    let label = match kind.as_str() {
        ARBITRARY => Label::Arbitrary(id),
        ACQUIRE => Label::Acquire(id),
        COMMAND | SUBTASK => Label::Action(id),
        _ => panic!("wrong label definition"),
    };

    let process_ref = match ctx.process_ref {
        ProcessRef::Id(id) => ProcessRef::Relative(id, vec![label.into()]),
        ProcessRef::Relative(id, labels) => {
            let mut new_labels = labels.clone();
            match labels.last().unwrap() {
                Label::Refinement(_) | Label::Action(_) => {
                    new_labels.push(label.into());
                }
                _ => {
                    new_labels.pop();
                    new_labels.push(label.into());
                }
            }
            ProcessRef::Relative(id, new_labels)
        }
    };

    env.update_context(ModActingContext::new(process_ref));
}
