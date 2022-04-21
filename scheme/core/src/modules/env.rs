use sompas_language::HELP;
use sompas_macros::scheme_fn;
use sompas_structs::documentation::Documentation;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::string;
use sompas_structs::typelvalue::KindLValue;

/// Returns a list of all the keys present in the environment
#[scheme_fn]
pub fn env_get_keys(env: &LEnv) -> Vec<LValue> {
    env.keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into()
}

#[scheme_fn]
pub fn env_get_macros(env: &LEnv) -> Vec<LValue> {
    env.macros()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
}

#[scheme_fn]
pub fn env_get_macro(env: &LEnv, m: Sym) -> LValue {
    match env.get_macro(&m).cloned() {
        Some(l) => l.into(),
        None => LValue::Nil,
    }
}

///print the help
/// Takes 0 or 1 parameter.
/// 0 parameter: gives the list of all the functions
/// 1 parameter: write the help of

pub fn help(env: &LEnv, args: &[LValue]) -> LResult {
    let documentation: Documentation = env.get_documentation();

    match args.len() {
        0 => Ok(documentation.get_all().into()),
        1 => match &args[0] {
            LValue::Fn(fun) => Ok(string!(documentation.get(fun.get_label()))),
            LValue::Symbol(s) => Ok(string!(documentation.get(s))),
            LValue::CoreOperator(co) => Ok(string!(documentation.get(&co.to_string()))),
            lv => Err(LRuntimeError::wrong_type(HELP, &lv, KindLValue::Symbol)),
        },
        _ => Err(LRuntimeError::wrong_number_of_args(HELP, args, 0..1)),
    }
}

#[scheme_fn]
pub fn get_list_modules(env: &LEnv) -> LValue {
    let list = env.get_list_modules();
    let mut str = '{'.to_string();
    for (i, s) in list.iter().enumerate() {
        if i != 0 {
            str.push(',')
        }
        str.push_str(s)
    }

    str.push(')');

    string!(str)
}
