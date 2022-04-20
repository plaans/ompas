use crate::rae_user::{CtxRae, MOD_RAE};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_rae_language::{RAE_GET_AGENDA, RAE_GET_CONFIG_PLATFORM, RAE_GET_ENV, RAE_GET_STATE};
use ompas_rae_structs::exec_context::mutex;
use ompas_rae_structs::exec_context::rae_state::*;
use ompas_rae_structs::exec_context::ressource_access::monitor;
use ompas_rae_structs::refinement::task_collection::*;
use sompas_structs::lenv::LEnv;
use sompas_structs::lerror::LResult;
use sompas_structs::lerror::LRuntimeError::*;
use sompas_structs::lvalue::LValue;
use sompas_structs::typelvalue::KindLValue;
use sompas_utils::dyn_async;

/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
#[macro_rules_attribute(dyn_async!)]
pub async fn get_state<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
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
                        return Err(Anyhow(
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
                    KindLValue::Symbol,
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
pub async fn get_task_network<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let string = ctx
        .get_rae_env()
        .read()
        .await
        .agenda
        .format_task_network()
        .await;
    Ok(string.into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn get_type_hierarchy<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let string = ctx.env.read().await.format_type_hierarchy();
    Ok(string.into())
}

#[macro_rules_attribute(dyn_async!)]
pub async fn get_agenda<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let mut task_filter = TaskFilter::default();

    for arg in args {
        match arg.to_string().as_str() {
            ABSTRACT_TASK => task_filter.task_type = Some(TaskType::AbstractTask),
            ACTION => task_filter.task_type = Some(TaskType::Action),
            STATUS_PENDING => task_filter.status = Some(TaskStatus::Pending),
            STATUS_DONE => task_filter.status = Some(TaskStatus::Done),
            STATUS_FAILURE => task_filter.status = Some(TaskStatus::Failure),
            STATUS_RUNNING => task_filter.status = Some(TaskStatus::Running),
            str => {
                return Err(Anyhow(
                    RAE_GET_AGENDA,
                    format!(
                        "{} is not a valid filter option, expecting ({}, {}, {}, {}, {}, {})",
                        str,
                        ABSTRACT_TASK,
                        ACTION,
                        STATUS_PENDING,
                        STATUS_RUNNING,
                        STATUS_DONE,
                        STATUS_FAILURE
                    ),
                ))
            }
        }
    }

    let string = ctx
        .get_rae_env()
        .read()
        .await
        .agenda
        .format_task_collection(task_filter)
        .await;
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
pub async fn get_actions<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    Ok(ctx.get_rae_env().read().await.domain_env.get_list_actions())
}

///Get the list of tasks in the environment
#[macro_rules_attribute(dyn_async!)]
pub async fn get_tasks<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    Ok(ctx.get_rae_env().read().await.domain_env.get_list_tasks())
}

///Get the list of state functions in the environment
#[macro_rules_attribute(dyn_async!)]
pub async fn get_state_function<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
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
                    KindLValue::Symbol,
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

#[macro_rules_attribute(dyn_async!)]
pub async fn get_stats<'a>(_: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    Ok(ctx.env.read().await.agenda.get_stats().await)
}

#[macro_rules_attribute(dyn_async!)]
pub async fn export_stats<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let file = if args.len() == 1 {
        Some(args[0].to_string())
    } else {
        None
    };
    ctx.env.read().await.agenda.export_to_csv(None, file).await;
    Ok(LValue::Nil)
}
