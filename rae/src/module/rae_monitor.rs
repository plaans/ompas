use crate::context::mutex;
use crate::context::rae_state::{
    StateType, KEY_DYNAMIC, KEY_INNER_WORLD, KEY_INSTANCE, KEY_STATIC,
};
use crate::context::ressource_access::monitor;
use crate::module::{CtxRae, MOD_RAE};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_utils::dyn_async;

pub const RAE_GET_STATE: &str = "get-state";
pub const RAE_GET_STATUS: &str = "get-status";
pub const RAE_GET_AGENDA: &str = "get-agenda";
pub const RAE_GET_METHODS: &str = "get-methods";
pub const RAE_GET_ACTIONS: &str = "get-actions";
pub const RAE_GET_SYMBOL_TYPE: &str = "get-symbol-type";
pub const RAE_GET_TASKS: &str = "get-tasks";
pub const RAE_GET_STATE_FUNCTIONS: &str = "get-state-functions";
pub const RAE_GET_ENV: &str = "get-env";
pub const RAE_GET_MUTEXES: &str = "get-mutexes";
pub const RAE_GET_MONITORS: &str = "get-monitors";

pub const DOC_RAE_GET_METHODS: &str = "Returns the list of all defined methods in RAE environment";
pub const DOC_RAE_GET_ACTIONS: &str = "Returns the list of all defined actions in RAE environment";
pub const DOC_RAE_GET_CONFIG_PLATFORM: &str = "Get the actual value of the config of the platform";
pub const DOC_RAE_GET_AGENDA: &str =
    "Get the actual agenda with for each task the current refinement stack.";
pub const DOC_RAE_GET_STATE: &str = "Returns the current state";
pub const DOC_RAE_GET_STATUS: &str = "Returns the current status of actions";
pub const DOC_RAE_GET_ENV: &str = "Returns the whole environment.";

pub const RAE_GET_CONFIG_PLATFORM: &str = "get-config-platform";
pub const RAE_GET_CONFIG_SELECT: &str = "get-config-select";
pub const DOC_RAE_GET_SYMBOL_TYPE: &str =
    "Returns the type of the symbol as defined in RAE environment";
pub const DOC_RAE_GET_SYMBOL_TYPE_VERBOSE: &str = "Types:\n\
                                           \t-state-function\n\
                                           \t-action\n\
                                           \t-task\n\
                                           \t-method";
pub const DOC_RAE_GET_TASKS: &str = "Returns the list of all defined tasks in RAE environment";
pub const DOC_RAE_GET_STATE_FUNCTIONS: &str =
    "Returns the list of all defined state-functions in RAE environment";

/// Returns all the status of the actions pretty printed
#[macro_rules_attribute(dyn_async!)]
pub async fn get_status<'a>(_: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let arc_rae_env = ctx.get_rae_env();
    let rae_env = arc_rae_env.read().await;
    let status = rae_env.actions_progress.status.read().await;
    let mut string = "Actions Status:\n".to_string();
    for element in status.iter() {
        string.push_str(format!("{}:{}\n", element.0, element.1).as_str())
    }

    Ok(string.into())
}

/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
#[macro_rules_attribute(dyn_async!)]
pub async fn get_state<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let _type = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(sym) = &args[0] {
                match sym.as_str() {
                    KEY_STATIC => Some(StateType::Static),
                    KEY_DYNAMIC => Some(StateType::Dynamic),
                    KEY_INNER_WORLD => Some(StateType::InnerWorld),
                    KEY_INSTANCE => Some(StateType::Instance),
                    _ => {
                        return Err(SpecialError(
                            RAE_GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                            ),
                        ))
                    }
                }
            } else {
                return Err(WrongType(
                    RAE_GET_STATE,
                    args[0].clone(),
                    (&args[0]).into(),
                    TypeLValue::Symbol,
                ));
            }
        }
        _ => {
            return Err(WrongNumberOfArgument(
                RAE_GET_STATE,
                args.into(),
                args.len(),
                0..1,
            ))
        }
    };
    let state = ctx.get_rae_env().read().await.state.get_state(_type).await;
    Ok(state.into_map())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn get_config_platform<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    if !args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_GET_CONFIG_PLATFORM,
            args.into(),
            args.len(),
            0..0,
        ));
    }
    Ok(LValue::String(
        ctx.get_options()
            .await
            .get_platform_config()
            .unwrap_or_else(|| String::from("no options")),
    ))
}

#[macro_rules_attribute(dyn_async!)]
pub async fn get_config_select<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    if !args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_GET_CONFIG_PLATFORM,
            args.into(),
            args.len(),
            0..0,
        ));
    }
    Ok(LValue::String(
        ctx.get_options().await.get_select_mode().to_string(),
    ))
}
#[macro_rules_attribute(dyn_async!)]
pub async fn get_agenda<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let all = match args.len() {
        0 => false,
        1 => {
            let arg = &args[0];
            &LValue::Symbol("all".to_string()) == arg
        }
        _ => false,
    };
    let string = ctx.get_rae_env().read().await.agenda.display(all).await;
    Ok(string.into())
}

//Conversion functions

#[macro_rules_attribute(dyn_async!)]
pub async fn get_mutexes<'a>(_: &'a [LValue], _: &'a LEnv) -> LResult {
    Ok(mutex::get_debug().await.into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn get_monitors<'a>(_: &'a [LValue], _: &'a LEnv) -> LResult {
    Ok(monitor::get_debug().await.into())
}

///Get the methods of a given task
#[macro_rules_attribute(dyn_async!)]
pub async fn get_methods<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    Ok(ctx.get_rae_env().read().await.domain_env.get_list_methods())
}

///Get the list of actions in the environment
#[macro_rules_attribute(dyn_async!)]
pub async fn get_actions<'a>(_: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    Ok(ctx.get_rae_env().read().await.domain_env.get_list_actions())
}

///Get the list of tasks in the environment
#[macro_rules_attribute(dyn_async!)]
pub async fn get_tasks<'a>(_: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    Ok(ctx.get_rae_env().read().await.domain_env.get_list_tasks())
}

///Get the list of state functions in the environment
#[macro_rules_attribute(dyn_async!)]
pub async fn get_state_function<'a>(_: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    Ok(ctx
        .get_rae_env()
        .read()
        .await
        .domain_env
        .get_list_state_functions())
}

/// Returns the whole RAE environment if no arg et the entry corresponding to the symbol passed in args.
#[macro_rules_attribute(dyn_async!)]
pub async fn get_env<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let key = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(key) = args[0].clone() {
                Some(key)
            } else {
                return Err(WrongType(
                    RAE_GET_ENV,
                    args[0].clone(),
                    args[0].clone().into(),
                    TypeLValue::Symbol,
                ));
            }
        }
        _ => {
            return Err(WrongNumberOfArgument(
                RAE_GET_ENV,
                args.into(),
                args.len(),
                0..1,
            ))
        }
    };

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    match key {
        None => Ok(ctx.get_rae_env().read().await.domain_env.to_string().into()),
        Some(key) => Ok(ctx
            .get_rae_env()
            .read()
            .await
            .domain_env
            .get_element_description(key)
            .into()),
    }
}
