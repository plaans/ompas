use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::interface::rae_options::OMPASOptions;
use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId};
use crate::ompas::scheme::exec::acting_context::ModActingContext;
use crate::ompas::scheme::exec::mode::{CtxMode, RAEMode};
use crate::ompas::scheme::exec::platform::platform::Platform;
use crate::ompas::scheme::exec::platform::ModPlatform;
use crate::ompas::scheme::exec::refinement::ModRefinement;
use crate::ompas::scheme::exec::resource::ModResource;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::control::ModControl;
use crate::planning::planner::problem::PlanningDomain;
use ompas_language::exec::acting_context::MOD_ACTING_CONTEXT;
use ompas_language::exec::mode::DOC_CTX_MODE;
use ompas_language::exec::{ARBITRARY, DOC_ARBITRARY, DOC_MOD_EXEC, MOD_EXEC};
use ompas_language::process::LOG_TOPIC_OMPAS;
use ompas_middleware::logger::LogClient;
use sompas_core::eval;
use sompas_core::modules::list::car;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;

pub mod acting_context;
pub mod mode;
pub mod platform;
pub mod refinement;
pub mod resource;
pub mod state;

/*
LANGUAGE
 */

pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

///Context that will contains primitives for the RAE executive
pub struct ModExec {
    options: Arc<RwLock<OMPASOptions>>,
    acting_manager: ActingManager,
    domain: Arc<RwLock<OMPASDomain>>,
    platform: Platform,
    log: LogClient,
    _pd: Arc<RwLock<Option<PlanningDomain>>>,
}

impl ModExec {
    pub async fn new(monitor: &ModControl) -> Self {
        Self {
            options: monitor.options.clone(),
            acting_manager: monitor.acting_manager.clone(),
            domain: monitor.acting_manager.domain.clone(),
            platform: monitor.platform.clone(),
            log: LogClient::new("exec-ompas", LOG_TOPIC_OMPAS).await,
            _pd: monitor.pd.clone(),
        }
    }
}

impl From<ModExec> for LModule {
    fn from(m: ModExec) -> Self {
        let mod_platform = ModPlatform::new(&m);
        let mod_resource = ModResource::new(&m);
        let mod_state = ModState::new(&m);
        let mod_refinement = ModRefinement::new(&m);

        let mut module = LModule::new(m, MOD_EXEC, DOC_MOD_EXEC);
        module.add_subcontext(CtxMode::new(RAEMode::Exec), DOC_CTX_MODE);
        module.add_submodule(mod_platform);
        module.add_submodule(mod_resource);
        module.add_submodule(mod_state);
        module.add_submodule(ModActingContext::default());
        module.add_submodule(mod_refinement);
        module.add_async_fn(ARBITRARY, arbitrary, DOC_ARBITRARY, false);
        module
    }
}

/*//Return the labels of the methods
fn get_instantiated_methods(env: &LEnv, args: &[LValue]) -> LResult {
    if args.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            GET_INSTANTIATED_METHODS,
            args,
            1..usize::MAX,
        ));
    }
    let task_name = &args[0];
    let task_args: LValue = (&args[1..]).into();
    //log::send(format!("searching methods for {}\n", task_name));
    let task_method_map = env.get_symbol(RAE_TASK_METHODS_MAP).unwrap();
    //log::send(format!("method_map: {}\n", task_method_map));
    let methods = if let LValue::Map(map) = task_method_map {
        let methods = match map.get(task_name) {
            None => {
                return Err(lruntimeerror!(
                    RAE_GET_INSTANTIATED_METHODS,
                    format!("no methods for {}", task_name)
                ))
            }
            Some(methods) => {
                //Got here the list of the symbol of the methods
                let mut instantiated_method = vec![];
                if let LValue::List(methods) = methods {
                    for method in methods.iter() {
                        //Handle here the case where it is needed to generate all instantiation of methods where several parameters are possible.
                        instantiated_method
                            .push(cons(env, &[method.clone(), task_args.clone()]).unwrap());
                    }
                    instantiated_method.into()
                } else if let LValue::Nil = methods {
                    LValue::Nil
                } else {
                    panic!("The list of methods should be a LValue::List or Nil and nothing else")
                }
            }
        };
        methods
    } else {
        panic!("this should be a LValue::Map")
    };

    //log::send(format!("{}", methods));
    Ok(methods)
}*/

/*fn get_best_method(env: &LEnv, args: &[LValue]) -> LResult {
    let methods = get_instantiated_methods(env, args)?;
    let task_args = &args[1..];
    let best_method = if let LValue::List(methods) = methods {
        if methods.is_empty() {
            return Err(lruntimeerror!(
                RAE_GET_BEST_METHOD,
                "task has no applicable method".to_string()
            ));
        }
        methods[0].clone()
    } else {
        return Err(wrong_type!(RAE_GET_BEST_METHOD, &methods, KindLValue::List));
    };

    let method_instance = cons(env, &[best_method, task_args.into()])?;

    Ok(method_instance)
}*/

#[async_scheme_fn]
pub async fn arbitrary(env: &LEnv, args: &[LValue]) -> LResult {
    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref;
    let supervisor = &env.get_context::<ModExec>(MOD_EXEC)?.acting_manager;

    let greedy = match args.len() {
        1 => car(env, &[args[0].clone()]),
        2 => {
            eval(
                &vec![
                    args[1].clone(),
                    vec![LPrimitive::Quote.into(), args[0].clone()].into(),
                ]
                .into(),
                &mut env.clone(),
                None,
            )
            .await
        }
        _ => Err(LRuntimeError::wrong_number_of_args(ARBITRARY, args, 1..2)),
    }?;

    let set: Vec<LValue> = args[0].clone().try_into()?;

    let id: ActingProcessId = match pr {
        ProcessRef::Id(id) => {
            if supervisor.get_kind(id).await == ProcessKind::Method {
                supervisor
                    .new_arbitrary(
                        Label::Arbitrary(supervisor.get_number_arbitrary(*id).await),
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
                Label::Arbitrary(s) => {
                    supervisor
                        .new_arbitrary(Label::Arbitrary(s), id, ProcessOrigin::Execution)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    let value = supervisor.set_arbitrary_value(&id, set, greedy).await;

    Ok(value)
}
