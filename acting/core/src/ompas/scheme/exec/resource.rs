use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId};
use crate::ompas::manager::resource::{
    Capacity, Quantity, ResourceHandler, ResourceManager, WaitAcquire, WaiterPriority,
};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::StateManager;
use crate::ompas::scheme::exec::acting_context::{def_label, ModActingContext};
use crate::ompas::scheme::exec::mode::RAEMode;
use crate::ompas::scheme::exec::ModExec;
use futures::FutureExt;
use macro_rules_attribute::macro_rules_attribute;
use ompas_language::exec::acting_context::MOD_ACTING_CONTEXT;
use ompas_language::exec::mode::*;
use ompas_language::exec::resource::*;
use ompas_language::sym_table::TYPE_OBJECT;
use ompas_middleware::logger::LogClient;
use ompas_utils::dyn_async;
use ompas_utils::other::generic_race;
use rand::{thread_rng, Rng};
use sompas_core::modules::map::get_map;
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::{FutureResult, LFuture};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror};
use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};

pub struct ModResource {
    resource_manager: ResourceManager,
    state_manager: StateManager,
    acting_manager: ActingManager,
    log: LogClient,
}

impl From<ModResource> for Context {
    fn from(value: ModResource) -> Self {
        Context::new(value, MOD_RESOURCE)
    }
}

impl ModResource {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            resource_manager: exec.acting_manager.resource_manager.clone(),
            state_manager: exec.acting_manager.state_manager.clone(),
            acting_manager: exec.acting_manager.clone(),
            log: exec.log.clone(),
        }
    }

    pub fn new_for_sim(resource_manager: ResourceManager, state_manager: StateManager) -> Self {
        Self {
            resource_manager,
            state_manager,
            acting_manager: Default::default(),
            log: Default::default(),
        }
    }
}

impl From<ModResource> for LModule {
    fn from(m: ModResource) -> Self {
        let mut module = LModule::new(m, MOD_RESOURCE, DOC_MOD_RESOURCE);
        module.add_async_fn(
            NEW_RESOURCE,
            new_resource,
            (DOC_NEW_RESOURCE, DOC_NEW_RESOURCE_VERBOSE),
            false,
        );
        module.add_async_fn(__ACQUIRE__, __acquire__, DOC___ACQUIRE__, false);
        module.add_async_fn(
            __ACQUIRE_IN_LIST__,
            __acquire_in_list__,
            DOC___ACQUIRE_IN_LIST__,
            false,
        );
        module.add_async_fn(RELEASE, release, DOC_RELEASE, false);
        module.add_async_fn(IS_LOCKED, is_locked, DOC_IS_LOCKED, false);
        module.add_async_fn(RESOURCES, resources, DOC_RESOURCES, false);
        module.add_lambda(ACQUIRE, LAMBDA_ACQUIRE, DOC_ACQUIRE);
        module.add_lambda(ACQUIRE_IN_LIST, LAMBDA_ACQUIRE_IN_LIST, DOC_ACQUIRE_IN_LIST);
        module
    }
}

#[async_scheme_fn]
pub async fn new_resource(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModResource>(MOD_RESOURCE)?;

    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(NEW_RESOURCE, args, 1..2))?
        .try_into()?;

    let capacity: Capacity = match args.get(1) {
        None => 1,
        Some(lv) => lv.try_into()?,
    };

    if LValue::Nil == ctx.state_manager.instance(&label, TYPE_OBJECT).await {
        ctx.state_manager.add_instance(&label, TYPE_OBJECT).await
    }

    ctx.resource_manager
        .new_resource(label, Some(capacity))
        .await;
    Ok(())
}

///Lock a resource
/// Waits on the resource until its his turn in the queue list
#[async_scheme_fn]
pub async fn __acquire__(env: &LEnv, args: &[LValue]) -> Result<LAsyncHandle, LRuntimeError> {
    let ctx = env.get_context::<ModResource>(MOD_RESOURCE)?;

    let resources = ctx.resource_manager.clone();

    let (tx, mut rx) = new_interruption_handler();

    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE, args, 1..2))?
        .try_into()?;
    //init of capacity and
    let (quantity, priority): (Quantity, WaiterPriority) = match args.len() {
        1 => (Quantity::All, WaiterPriority::Execution(0)),
        2 => {
            if let LValue::List(l) = &args[1] {
                if l.get(0).unwrap() == &PRIORITY.into() && l.len() == 2 {
                    let priority = l.get(1).unwrap().try_into()?;
                    (Quantity::All, WaiterPriority::Execution(priority))
                } else {
                    Err(LRuntimeError::new("", ""))?
                }
            } else {
                let quantity = Quantity::Some(args[1].borrow().try_into()?);
                (quantity, WaiterPriority::Execution(0))
            }
        }
        3 => {
            let quantity = Quantity::Some(args[1].borrow().try_into()?);
            let priority = if let LValue::List(l) = &args[1] {
                if l.get(0).unwrap() == &PRIORITY.into() && l.len() == 2 {
                    WaiterPriority::Execution(l.get(1).unwrap().try_into()?)
                } else {
                    Err(LRuntimeError::new("", ""))?
                }
            } else {
                Err(LRuntimeError::new("", ""))?
            };

            (quantity, priority)
        }
        _ => {
            panic!()
        }
    };

    let mode = *env.get_context::<RAEMode>(CTX_RAE_MODE)?;
    if mode == RAEMode::Simu {
        let f: LFuture = (Box::pin(async move {
            let mut wait = resources.acquire(&label, quantity, priority).await?;

            let rh: ResourceHandler = tokio::select! {
                _ = rx.recv() => {
                    resources.remove_waiter(wait).await;
                    return Ok(LValue::Err(LValue::Nil.into()))
                }
                rh = wait.recv() => {
                    rh
                }
            };

            let rc = resources.clone();
            let (tx, mut rx) = new_interruption_handler();

            let f: LFuture = (Box::pin(async move {
                rx.recv().await;
                let r = rc.release(rh).await.map(|_| LValue::Nil);
                r
            }) as FutureResult)
                .shared();

            let f2 = f.clone();

            tokio::spawn(f);

            Ok(LAsyncHandle::new(f2, tx).into())
        }) as FutureResult)
            .shared();

        let f2 = f.clone();

        tokio::spawn(f);

        return Ok(LAsyncHandle::new(f2, tx));
    };

    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref;
    let acting_manager = ctx.acting_manager.clone();

    let id: ActingProcessId = match pr {
        ProcessRef::Id(id) => {
            if acting_manager.get_kind(id).await == ActingProcessKind::Method {
                acting_manager
                    .new_acquire(
                        Label::ResourceAcquisition(acting_manager.get_number_acquire(*id).await),
                        id,
                        ProcessOrigin::Execution,
                    )
                    .await
            } else {
                panic!()
            }
        }
        ProcessRef::Relative(id, labels) => match acting_manager.get_id(pr.clone()).await {
            Some(id) => id,
            None => match labels[0] {
                Label::ResourceAcquisition(s) => {
                    acting_manager
                        .new_acquire(Label::ResourceAcquisition(s), id, ProcessOrigin::Execution)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    let log = ctx.log.clone();

    let f: LFuture = (Box::pin(async move {
        acting_manager.set_start(&id, None).await;

        let mut wait: WaitAcquire = acting_manager
            .acquire(&id, label.to_string(), quantity, priority)
            .await?;

        let priority = acting_manager
            .resource_manager
            .get_client_priority(&wait.get_resource_id(), &wait.get_client_id())
            .await;

        log.info(format!(
            "({id}) Acquiring {label}; capacity = {quantity}; priority = {priority}"
        ));

        let rh: ResourceHandler = tokio::select! {
            _ = rx.recv() => {
                log.info(format!("Acquisition of {label} cancelled."));
                resources.remove_waiter(wait).await;
                return Ok(LValue::Err(LValue::Nil.into()))
            }
            rh = wait.recv() => {
                log.info(format!("({}) Acquired unlocked", id));
                rh
            }
        };

        acting_manager.set_s_acq(&id, None).await;

        let rc = resources.clone();
        let (tx, mut rx) = new_interruption_handler();

        let log2 = log.clone();

        let f: LFuture = (Box::pin(async move {
            rx.recv().await;
            let label = rh.get_label().to_string();
            let r = rc.release(rh).await.map(|_| LValue::Nil);
            log2.info(format!("Released {}", label));
            acting_manager
                .set_end(&id, None, ProcessStatus::Success)
                .await;
            r
        }) as FutureResult)
            .shared();

        let f2 = f.clone();

        tokio::spawn(f);

        log.info(format!("{label} acquired with {quantity} capacity."));

        Ok(LAsyncHandle::new(f2, tx).into())
    }) as FutureResult)
        .shared();

    let f2 = f.clone();

    tokio::spawn(f);

    Ok(LAsyncHandle::new(f2, tx))
}

/// Release the resource
#[async_scheme_fn]
pub async fn release(mut h: LAsyncHandle) -> LResult {
    h.interrupt().await
}

#[macro_rules_attribute(dyn_async!)]
async fn check_acquire<'a>(arg: (LEnv, Vec<LValue>, usize)) -> LResult {
    let (env, args, d) = arg;
    let label = args[0].clone();
    let r = __acquire__(&env, args.as_slice()).await?;
    let h_await: LAsyncHandle = r.try_into().unwrap();
    let h: LValue = h_await.get_future().await?;
    env.get_context::<ModResource>(MOD_RESOURCE)?
        .log
        .info(format!("{} unlocked {}", d, label));
    Ok(list![label, h])
}

/// Ask to lock a resource in a list
/// Returns the resource that has been locked.
#[async_scheme_fn]
pub async fn __acquire_in_list__(
    env: &LEnv,
    args: &[LValue],
) -> Result<LAsyncHandle, LRuntimeError> {
    let (tx, mut rx) = new_interruption_handler();
    let mut resources: Vec<LValue> = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE_IN_LIST, args, 1..2))?
        .try_into()?;
    let d: usize = thread_rng().gen();
    let ctx = env.get_context::<ModResource>(MOD_RESOURCE)?;
    ctx.log
        .info(format!("Acquire element from {}, id: {} ", args[0], d));
    //let capacity = args.get(1).cloned();
    let rest = if args.len() > 1 {
        args[1..].to_vec()
    } else {
        vec![]
    };

    let acting_manager = ctx.acting_manager.clone();

    let env = env.clone();

    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref;

    let parent = acting_manager.get_id(pr.clone()).await.unwrap();

    let mut ids = vec![];
    for _ in 0..resources.len() - 1 {
        let label_id = acting_manager.get_number_acquire(parent).await;
        ids.push(
            acting_manager
                .new_acquire(
                    Label::ResourceAcquisition(label_id),
                    &parent,
                    ProcessOrigin::Execution,
                )
                .await,
        );
    }
    let receivers: Vec<(LEnv, Vec<LValue>, usize)> = resources
        .iter_mut()
        .zip(ids)
        .map(|(r, id)| {
            let mut vec = vec![r.clone()];
            vec.append(&mut rest.clone());
            let mut env = env.clone();
            let _ = def_label(&mut env, &[ACQUIRE.into(), id.into()]);
            (env.clone(), vec, d)
        })
        .collect();

    let f: LFuture = (Box::pin(async move {
        tokio::select! {
            _ = rx.recv() =>  {
                Ok(LValue::Err(LValue::Nil.into()))
            }
            r = generic_race(receivers, check_acquire) => {
                r
            }
        }
    }) as FutureResult)
        .shared();

    let f2 = f.clone();

    tokio::spawn(f2);

    Ok(LAsyncHandle::new(f, tx))
}

#[async_scheme_fn]
pub async fn is_locked(env: &LEnv, args: &[LValue]) -> LResult {
    let mode: String = env
        .get_symbol("rae-mode")
        .expect("rae-mode should be defined, default value is exec mode")
        .try_into()?;
    let ctx = env.get_context::<ModResource>(MOD_RESOURCE)?;

    match mode.as_str() {
        SYMBOL_EXEC_MODE => Ok(ctx
            .resource_manager
            .is_locked(&String::try_from(&args[0])?)
            .await?
            .into()),
        SYMBOL_SIMU_MODE => {
            let state = match env.get_symbol("state") {
                Some(lv) => lv,
                None => {
                    return Err(lruntimeerror!(
                        IS_LOCKED,
                        "state should be defined in simu mode".to_string()
                    ))
                }
            };
            get_map(env, &[state, args.into()])
        }
        _ => unreachable!(
            "{} should have either {} or {} value.",
            SYMBOL_RAE_MODE, SYMBOL_EXEC_MODE, SYMBOL_SIMU_MODE
        ),
    }
}

#[async_scheme_fn]
pub async fn resources(env: &LEnv) -> Result<Vec<String>, LRuntimeError> {
    let ctx = env.get_context::<ModResource>(MOD_RESOURCE)?;

    Ok(ctx.resource_manager.get_list_resources().await)
}
