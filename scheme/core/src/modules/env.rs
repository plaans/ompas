use ompas_middleware::Master;
use sompas_language::env::*;
use sompas_macros::{async_scheme_fn, scheme_fn};
use sompas_structs::documentation::DocCollection;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::{LValue, Sym};

#[derive(Default)]
pub struct ModEnv {}

impl From<ModEnv> for LModule {
    fn from(m: ModEnv) -> LModule {
        let mut module = LModule::new(m, MOD_ENV, DOC_MOD_ENV);
        module.add_fn(GET_KEYS, get_keys, DOC_GET_KEYS, false);
        module.add_fn(GET_MACROS, get_macros, GET_MACROS, false);
        module.add_fn(GET_MACRO, get_macro, DOC_GET_MACRO, false);
        module.add_fn(GET_CONTEXTS, get_contexts, DOC_GET_CONTEXTS, false);
        module.add_fn(HELP, help, (DOC_HELP, DOC_HELP_VERBOSE), false);
        module.add_async_fn(
            GET_PROCESS_HIERARCHY,
            get_process_hierarchy,
            DOC_GET_PROCESS_HIERARCHY,
            false,
        );
        module
    }
}

/// Returns a list of all the keys present in the environment
#[scheme_fn]
fn get_keys(env: &LEnv) -> Vec<LValue> {
    env.keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
}

/// Return the list of macros present in the environment
#[scheme_fn]
fn get_macros(env: &LEnv) -> Vec<LValue> {
    env.macros()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
}

/// Return the expression of a given macro
#[scheme_fn]
fn get_macro(env: &LEnv, m: Sym) -> LValue {
    match env.get_macro(&m).cloned() {
        Some(l) => l.into(),
        None => LValue::Nil,
    }
}

/// Return a list of help elements
/// Takes 0 or 1 parameter.
/// 0 parameter: gives the list of all the functions
/// 1 parameter: write the help of

#[scheme_fn]
fn help(env: &LEnv, args: &[LValue]) -> Result<String, LRuntimeError> {
    let documentation: DocCollection = env.get_documentation();

    match args.len() {
        0 => Ok(documentation.to_string()),
        1 => match &args[0] {
            LValue::Fn(fun) => Ok(documentation.get(fun.get_label())),
            LValue::Symbol(s) => Ok(documentation.get(s)),
            LValue::Primitive(co) => Ok(documentation.get(&co.to_string())),
            lv => Err(LRuntimeError::wrong_type(HELP, lv, KindLValue::Symbol)),
        },
        _ => Err(LRuntimeError::wrong_number_of_args(HELP, args, 0..1)),
    }
}

/// Return the list of all modules loaded in the environment
#[scheme_fn]
fn get_contexts(env: &LEnv) -> String {
    let list = env.get_contexts_labels();
    let mut str = '{'.to_string();
    for (i, s) in list.iter().enumerate() {
        if i != 0 {
            str.push(',')
        }
        str.push_str(s)
    }

    str.push(')');

    str
}

/// Return the list of processes and process topics along their dependencies
#[async_scheme_fn]
pub async fn get_process_hierarchy() -> String {
    Master::format_process_hierarchy().await
}
