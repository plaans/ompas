use crate::exec::*;
use ompas_language::exec::mode::*;
use ompas_language::exec::resource::*;
use ompas_middleware::logger::LogClient;
use ompas_structs::acting_manager::action_status::ProcessStatus;
use ompas_structs::execution::resource::{
    Capacity, Quantity, ResourceHandler, ResourceManager, WaitAcquire, WaiterPriority,
};
use ompas_structs::mutex::Wait;
use ompas_utils::dyn_async;
use ompas_utils::other::generic_race;
use rand::{thread_rng, Rng};
use sompas_core::modules::map::get_map;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};

pub struct ModResource {
    resource_manager: ResourceManager,
    log: LogClient,
}

impl ModResource {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            resource_manager: exec.acting_manager.resource_manager.clone(),
            log: exec.log.clone(),
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
    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref;
    let supervisor = env.get_context::<ModExec>(MOD_EXEC)?.acting_manager.clone();
    let label: String = args
        .get(0)
        .ok_or_else(|| LRuntimeError::wrong_number_of_args(ACQUIRE, args, 1..2))?
        .try_into()?;
    let id: ActingProcessId = match pr {
        ProcessRef::Id(id) => {
            if supervisor.get_kind(id).await == ProcessKind::Method {
                supervisor
                    .new_acquire(
                        Label::Acquire(supervisor.get_number_acquire(*id).await),
                        id,
                        ProcessOrigin::Execution,
                    )
                    .await
            } else {
                panic!()
            }
        }
        ProcessRef::Relative(id, labels) => match supervisor.get_id(pr.clone()).await {
            Some(id) => id,
            None => match labels[0] {
                Label::Acquire(s) => {
                    supervisor
                        .new_acquire(Label::Acquire(s), id, ProcessOrigin::Execution)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    let log = ctx.log.clone();

    let resources = ctx.resource_manager.clone();

    let (tx, mut rx) = new_interruption_handler();

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

    let f: LFuture = (Box::pin(async move {
        log.info(format!(
            "Acquiring {label}; capacity = {quantity}; priority = {priority}"
        ))
        .await;

        supervisor.set_start(&id, None).await;

        let mut wait: WaitAcquire = supervisor
            .acquire(&id, label.to_string(), quantity, priority)
            .await?;

        let rh: ResourceHandler = tokio::select! {
            _ = rx.recv() => {
                log.info(format!("Acquisition of {label} cancelled.")).await;
                resources.remove_waiter(wait).await;
                return Ok(LValue::Err(LValue::Nil.into()))
            }
            rh = wait.recv() => {
                log.info(format!("Resource {label} unlocked !!!")).await;
                rh
            }
        };

        supervisor.set_s_acq(&id, None).await;

        let rc = resources.clone();
        let (tx, mut rx) = new_interruption_handler();

        let log2 = log.clone();

        let f: LFuture = (Box::pin(async move {
            rx.recv().await;
            let label = rh.get_label().to_string();
            let r = rc.release(rh).await.map(|_| LValue::Nil);
            log2.info(format!("Released {}", label)).await;
            supervisor.set_end(&id, None, ProcessStatus::Success).await;
            r
        }) as FutureResult)
            .shared();

        let f2 = f.clone();

        tokio::spawn(f);

        log.info(format!("{label} acquired with {quantity} capacity."))
            .await;

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
async fn check_receiver<'a>(mut wait: (String, Wait)) -> String {
    wait.1.rx.recv().await;
    wait.0
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
        .info(format!("{} unlocked {}", d, label))
        .await;
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
    env.get_context::<ModResource>(MOD_RESOURCE)?
        .log
        .info(format!("Acquire element from {}, id: {} ", args[0], d))
        .await;
    //let capacity = args.get(1).cloned();
    let rest = if args.len() > 1 {
        args[1..].to_vec()
    } else {
        vec![]
    };

    let env = env.clone();

    let f: LFuture = (Box::pin(async move {
        let receivers: Vec<(LEnv, Vec<LValue>, usize)> = resources
            .iter_mut()
            .map(|r| {
                let mut vec = vec![r.clone()];
                vec.append(&mut rest.clone());
                (env.clone(), vec, d)
            })
            .collect();

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
            .is_locked(args[0].borrow().try_into()?)
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
