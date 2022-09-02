use crate::rae_user::{CtxRaeUser, MOD_RAE_USER};
use ompas_rae_language::{RAE_GET_AGENDA, RAE_GET_ENV, RAE_GET_STATE};
use ompas_rae_structs::state::task_state::*;
use ompas_rae_structs::state::task_status::TaskStatus;
use ompas_rae_structs::state::task_status::*;
use ompas_rae_structs::state::world_state::*;
use sompas_macros::*;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{lruntimeerror, wrong_type};
/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
#[async_scheme_fn]
pub async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
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
                        return Err(lruntimeerror!(
                            RAE_GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                            )
                        ))
                    }
                }
            } else {
                return Err(wrong_type!(RAE_GET_STATE, &args[0], KindLValue::Symbol));
            }
        }
        _ => {
            return Err(LRuntimeError::wrong_number_of_args(
                RAE_GET_STATE,
                args,
                0..1,
            ))
        }
    };
    let state = ctx.interface.state.get_state(_type).await;
    Ok(state.into_map())
}

#[async_scheme_fn]
pub async fn get_config_platform(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.get_options()
        .await
        .get_platform_config()
        .unwrap_or_else(|| String::from("no options"))
}

#[async_scheme_fn]
pub async fn get_config_select(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.get_options().await.get_select_mode().to_string()
}
#[async_scheme_fn]
pub async fn get_task_network(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.interface.agenda.format_task_network().await
}

#[async_scheme_fn]
pub async fn get_type_hierarchy(env: &LEnv) -> String {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.rae_domain.read().await.types.format_hierarchy()
}

#[async_scheme_fn]
pub async fn get_agenda(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
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
                return Err(lruntimeerror!(
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
                    )
                ))
            }
        }
    }

    let string = ctx
        .interface
        .agenda
        .format_task_collection(task_filter)
        .await;
    Ok(string.into())
}

//Conversion functions

#[async_scheme_fn]
pub async fn get_resources(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.interface.resources.get_debug().await.into())
}

#[async_scheme_fn]
pub async fn get_monitors(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.interface.monitors.get_debug().await.into())
}

///Get the methods of a given task
#[async_scheme_fn]
pub async fn get_methods(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.rae_domain.read().await.get_list_methods())
}

///Get the list of actions in the environment
#[async_scheme_fn]
pub async fn get_actions(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.rae_domain.read().await.get_list_actions())
}

///Get the list of tasks in the environment
#[async_scheme_fn]
pub async fn get_tasks(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    Ok(ctx.rae_domain.read().await.get_list_tasks())
}

///Get the list of state functions in the environment
#[async_scheme_fn]
pub async fn get_state_function(env: &LEnv) -> LValue {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();
    ctx.rae_domain.read().await.get_list_state_functions()
}

/// Returns the whole RAE environment if no arg et the entry corresponding to the symbol passed in args.
#[async_scheme_fn]
pub async fn get_env(env: &LEnv, args: &[LValue]) -> LResult {
    let key = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(key) = args[0].clone() {
                Some(key)
            } else {
                return Err(wrong_type!(RAE_GET_ENV, &args[0], KindLValue::Symbol));
            }
        }
        _ => return Err(LRuntimeError::wrong_number_of_args(RAE_GET_ENV, args, 0..1)),
    };

    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    match key {
        None => Ok(ctx.rae_domain.read().await.to_string().into()),
        Some(key) => Ok(ctx
            .rae_domain
            .read()
            .await
            .get_element_description(key.as_ref())
            .into()),
    }
}

#[async_scheme_fn]
pub async fn get_stats(env: &LEnv) -> LValue {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    ctx.interface.agenda.get_stats().await
}

#[async_scheme_fn]
pub async fn export_stats(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let file = if args.len() == 1 {
        Some(args[0].to_string())
    } else {
        None
    };
    ctx.interface.agenda.export_to_csv(None, file).await;
    Ok(LValue::Nil)
}
