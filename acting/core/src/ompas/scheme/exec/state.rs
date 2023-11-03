use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::monitor::MonitorManager;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::manager::state::{StateManager, StateType};
use crate::ompas::scheme::exec::resource::resources;
use crate::ompas::scheme::exec::ModExec;
use futures::FutureExt;
use ompas_language::exec::resource::LOCKED;
use ompas_language::exec::state::*;
use sompas_core::eval;
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::interrupted;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use sompas_structs::{lruntimeerror, wrong_type};
use std::time::Duration;

pub struct ModState {
    pub state: StateManager,
    pub monitors: MonitorManager,
    pub domain: DomainManager,
}

impl ModState {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            state: exec.acting_manager.state_manager.clone(),
            monitors: exec.acting_manager.monitor_manager.clone(),
            domain: exec.acting_manager.domain_manager.clone(),
        }
    }

    pub fn new_from_snapshot(state: WorldStateSnapshot) -> Self {
        Self {
            state: state.into(),
            monitors: MonitorManager::from(ClockManager::default()),
            domain: DomainManager::default(),
        }
    }
}

impl From<ModState> for Context {
    fn from(m: ModState) -> Self {
        Context::new(m, MOD_STATE)
    }
}

impl From<ModState> for LModule {
    fn from(m: ModState) -> Self {
        let mut module = LModule::new(m, MOD_STATE, DOC_MOD_STATE);
        module.add_async_fn(ASSERT, assert, DOC_ASSERT, false);
        module.add_async_fn(ASSERT_SHORT, assert, DOC_ASSERT_SHORT, false);
        module.add_async_fn(EFFECT, effect, DOC_EFFECT, false);
        module.add_async_fn(
            TRANSITIVE_EFFECT,
            transitive_effect,
            DOC_TRANSITIVE_EFFECT,
            false,
        );
        module.add_async_fn(RETRACT, retract, DOC_RETRACT, false);
        module.add_async_fn(RETRACT_SHORT, retract, DOC_RETRACT_SHORT, false);
        module.add_async_fn(READ_STATE, read_state, DOC_READ_STATE, false);
        module.add_async_fn(
            READ_STATIC_STATE,
            read_static_state,
            DOC_READ_STATIC_STATE,
            true,
        );
        module.add_async_fn(INSTANCE, instance, DOC_INSTANCE, true);
        module.add_async_fn(INSTANCES, instances, DOC_INSTANCES, true);
        module.add_async_fn(__WAIT_FOR__, __wait_for__, DOC___WAIT_FOR__, false);

        //Macros
        module.add_macro(RUN_MONITORING, MACRO_RUN_MONITORING, DOC_RUN_MONITORING);

        //Lambdas
        module.add_lambda(WAIT_FOR, LAMBDA_WAIT_FOR, DOC_WAIT_FOR);
        module.add_lambda(MONITOR, LAMBDA_MONITOR, DOC_MONITOR);
        module
    }
}

///Add a fact to fact state
#[async_scheme_fn]
async fn assert(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    if args.len() < 2 {
        Err(LRuntimeError::wrong_number_of_args(
            ASSERT,
            args,
            2..usize::MAX,
        ))?
    }
    let state = env.get_context::<ModState>(MOD_STATE)?;
    let key: LValue = if args.len() > 2 {
        args[0..args.len() - 1].into()
    } else {
        args[0].clone()
    };
    let value = args.last().unwrap();
    state
        .state
        .add_value_with_date(key.try_into()?, value.try_into()?)
        .await;
    Ok(())
}

#[async_scheme_fn]
async fn effect(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    if args.len() < 2 {
        Err(LRuntimeError::wrong_number_of_args(
            EFFECT,
            args,
            2..usize::MAX,
        ))?
    }
    let state = env.get_context::<ModState>(MOD_STATE)?;
    let key: LValue = if args.len() > 2 {
        args[0..args.len() - 1].into()
    } else {
        args[0].clone()
    };
    let value = args.last().unwrap();
    state
        .state
        .add_value_with_date(key.try_into()?, value.try_into()?)
        .await;
    Ok(())
}

#[async_scheme_fn]
async fn transitive_effect(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    if args.len() < 3 {
        return Err(LRuntimeError::wrong_number_of_args(
            TRANSITIVE_EFFECT,
            args,
            3..usize::MAX,
        ));
    }
    let state = env.get_context::<ModState>(MOD_STATE)?;
    let duration: f64 = args.first().unwrap().try_into()?;

    let (key, first): (LValueS, _) = if args.len() > 3 {
        (
            LValue::from(&args[1..args.len() - 1]).try_into()?,
            args[1].to_string(),
        )
    } else {
        (args[1].clone().try_into()?, args[1].to_string())
    };

    let intermediate_result = match state.domain.get_state_function(&first).await {
        Some(sf) => LValueS::from(state.state.get_unk_of_type(sf.result_debug.as_str()).await),
        None => LValueS::from(UNKNOWN),
    };

    let value: LValueS = args.last().unwrap().try_into()?;

    state
        .state
        .add_value_with_date(key.clone(), intermediate_result)
        .await;
    tokio::time::sleep(Duration::from_micros((duration * 1_000_000.0) as u64)).await;
    state.state.add_value_with_date(key, value).await;
    Ok(())
}

///Retract a fact to state
#[async_scheme_fn]
async fn retract(env: &LEnv, key: LValueS, value: LValueS) -> Result<(), LRuntimeError> {
    let state = env.get_context::<ModState>(MOD_STATE)?;

    state.state.retract_fact(key, value).await
}

#[async_scheme_fn]
async fn read_state(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            READ_STATE,
            args,
            1..usize::MAX,
        ));
    }

    let key: LValue = if args.len() > 1 {
        args.into()
    } else {
        args[0].clone()
    };

    let key: LValueS = key.try_into()?;

    let ctx = env.get_context::<ModState>(MOD_STATE)?;
    let state = ctx.state.get_state(None).await;

    let result: LValue = match state.get(&key) {
        None => LValue::Nil,
        Some(f) => f.value.clone().into(),
    };
    Ok(result)

    //Ok(map.get(&key).cloned().unwrap_or(UNKNOWN.into()))
}

#[async_scheme_fn]
async fn read_static_state(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            READ_STATE,
            args,
            1..usize::MAX,
        ));
    }

    let key: LValue = if args.len() > 1 {
        args.into()
    } else {
        args[0].clone()
    };

    let key: LValueS = key.try_into()?;

    let ctx = env.get_context::<ModState>(MOD_STATE)?;

    let state = ctx.state.get_state(Some(StateType::Static)).await;

    let result: LValue = match state.get(&key) {
        None => LValue::Nil,
        Some(f) => f.value.clone().into(),
    };
    Ok(result)
}

///2 args: check if an instance is of a certain type
#[async_scheme_fn]
pub async fn instance(env: &LEnv, object: String, r#type: String) -> LResult {
    let state = &env.get_context::<ModState>(MOD_STATE)?.state;

    Ok(state.instance(&object, &r#type).await)
}

#[async_scheme_fn]
pub async fn instances(env: &LEnv, r#type: String) -> LResult {
    let state = &env.get_context::<ModState>(MOD_STATE)?.state;
    Ok(state.instances(&r#type).await)
}

#[async_scheme_fn]
async fn get_facts(env: &LEnv) -> LResult {
    let mut state: im::HashMap<LValue, LValue> = get_state(env, &[]).await?.try_into()?;
    let locked: Vec<LValue> = resources(env, &[]).await?.try_into()?;

    for e in locked {
        state.insert(vec![LOCKED.into(), e].into(), LValue::True);
    }

    Ok(state.into())
}

#[async_scheme_fn]
async fn get_state(env: &LEnv, args: &[LValue]) -> LResult {
    let ctx = env.get_context::<ModState>(MOD_STATE)?;

    let _type = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(sym) = &args[0] {
                match sym.as_str() {
                    STATIC => Some(StateType::Static),
                    DYNAMIC => Some(StateType::Dynamic),
                    INNER_STATIC => Some(StateType::Static),
                    INNER_DYNAMIC => Some(StateType::Dynamic),
                    INSTANCE => Some(StateType::Instance),
                    _ => {
                        return Err(lruntimeerror!(
                            GET_STATE,
                            format!(
                                "was expecting keys {:?}",
                                [STATIC, DYNAMIC, INNER_STATIC, INNER_DYNAMIC, INSTANCE]
                            )
                        ))
                    }
                }
            } else {
                return Err(wrong_type!(GET_STATE, &args[0], KindLValue::Symbol));
            }
        }
        _ => return Err(LRuntimeError::wrong_number_of_args(GET_STATE, args, 0..1)),
    };

    let state = ctx.state.get_state(_type).await.into_map();
    Ok(state)
}

#[async_scheme_fn]
async fn __wait_for__(env: &LEnv, lv: LValue) -> Result<LAsyncHandle, LRuntimeError> {
    let (tx, mut rx) = new_interruption_handler();
    let ctx = env.get_context::<ModState>(MOD_STATE)?;
    let monitors = ctx.monitors.clone();

    let mut env = env.clone();
    let f: LFuture = (Box::pin(async move {
        //println!("wait-for: {}", lv);
        if let LValue::True = eval(&lv, &mut env, None).await? {
            Ok(LValue::Nil)
        } else {
            let handler = monitors.add_waiter(lv.clone()).await;
            let id = *handler.id();
            //println!("wait-for: waiting on {}", lv);
            tokio::select! {
                _ = rx.recv() => {
                        //println!("wait-for: waiter no longer needed");
                        monitors.remove_waiter(id).await;
                        Ok(interrupted!())
                }
                _ = handler.recv() => {
                    //println!("success for waiter");
                    Ok(LValue::Nil)
                }
            }
        }
    }) as FutureResult)
        .shared();

    tokio::spawn(f.clone());

    Ok(LAsyncHandle::new(f, tx))
}
