use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::manager::acting::ActingProcessId;
use ompas_language::exec::acting_context::*;
use sompas_macros::scheme_fn;
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
        module.add_mut_fn(DEF_PROCESS_ID, def_process_id, DOC_DEF_PROCESS_ID);
        module.add_mut_fn(DEF_LABEL, def_label, DOC_DEF_LABEL);
        module
    }
}

#[scheme_fn]
pub fn def_process_id(env: &mut LEnv, id: ActingProcessId) {
    env.update_context(ModActingContext::new(ProcessRef::Id(id)))
}

pub const ARBITRARY: &str = ompas_language::supervisor::ARBITRARY;
pub const ACQUIRE: &str = ompas_language::supervisor::ACQUIRE;
pub const COMMAND: &str = ompas_language::supervisor::COMMAND;
pub const TASK: &str = ompas_language::supervisor::TASK;

#[scheme_fn]
pub fn def_label(env: &mut LEnv, kind: String, id: usize) {
    let ctx = env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)
        .unwrap()
        .clone();
    let label = match kind.as_str() {
        ARBITRARY => Label::Arbitrary(id),
        ACQUIRE => Label::ResourceAcquisition(id),
        COMMAND => Label::Command(id),
        TASK => Label::Task(id),
        _ => panic!("wrong label definition"),
    };

    let process_ref = match ctx.process_ref {
        ProcessRef::Id(id) => ProcessRef::Relative(id, vec![label]),
        ProcessRef::Relative(id, labels) => {
            let mut new_labels = labels.clone();
            match labels.last().unwrap() {
                Label::Refinement(..) | Label::Task(_) => {
                    new_labels.push(label);
                }
                _ => {
                    new_labels.pop();
                    new_labels.push(label);
                }
            }
            ProcessRef::Relative(id, new_labels)
        }
    };

    env.update_context(ModActingContext::new(process_ref));
}
