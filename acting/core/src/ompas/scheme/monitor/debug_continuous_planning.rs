use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::control::ModControl;
use crate::ompas::scheme::monitor::model::ModModel;
use crate::ompas::scheme::monitor::ModMonitor;
use crate::planning::planner::solver::PMetric;
use ompas_language::monitor::continuous_planning::*;
use ompas_language::monitor::control::MOD_CONTROL;
use ompas_language::monitor::model::MOD_MODEL;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use tokio::sync::broadcast;

#[derive(Clone)]
pub struct ModContinuousPlanning {
    _st: RefSymTable,
    _domain: DomainManager,
}

impl ModContinuousPlanning {
    pub fn new(monitor: &ModMonitor) -> Self {
        Self {
            _st: monitor.acting_manager.st.clone(),
            _domain: monitor.acting_manager.domain.clone(),
        }
    }
}

impl From<ModContinuousPlanning> for LModule {
    fn from(m: ModContinuousPlanning) -> Self {
        let mut m = LModule::new(m, MOD_CONTINUOUS_PLANNING, DOC_MOD_CONTINUOUS_PLANNING);
        m.add_async_fn(START, start, DOC_START, false);
        m.add_async_fn(PLAN, plan, DOC_PLAN, false);
        m.add_async_fn(NEW_TASK, new_task, DOC_NEW_TASK, false);
        m.add_async_fn(NEW_EVENT, new_event, DOC_NEW_EVENT, false);
        m.add_async_fn(
            NEW_INSTANTIATION,
            new_instantiation,
            DOC_NEW_INSTANTIATION,
            false,
        );

        m
    }
}

#[async_scheme_fn]
pub async fn start(env: &LEnv, opt: bool) -> LResult {
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;

    let mut env: LEnv = ctx.get_plan_env().await;
    let state = ctx.get_plan_state().await;

    env.update_context(ModState::new_from_snapshot(state));

    acting_manager
        .start_continuous_planning(env, if opt { Some(PMetric::Makespan) } else { None })
        .await;

    Ok(LValue::Nil)
}
#[async_scheme_fn]
pub async fn plan(env: &LEnv) -> LResult {
    println!("Plan actual Acting Tree.");
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    acting_manager.plan().await;
    Ok(LValue::Nil)
}
async fn wait_on_planner(env: &LEnv) -> LResult {
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let mut recv: broadcast::Receiver<bool> =
        acting_manager.subscribe_on_plan_update().await.unwrap();
    recv.recv()
        .await
        .expect("Error while waiting on plan update.");

    Ok(LValue::Nil)
}
#[async_scheme_fn]
pub async fn new_task(env: &LEnv, args: &[LValue]) -> LResult {
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let debug = LValue::from(args).to_string();
    let args = args.iter().map(|lv| lv.as_cst().unwrap()).collect();

    let _pr = acting_manager.new_high_level_task(debug, args).await;
    wait_on_planner(env).await?;
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn new_instantiation(env: &LEnv, _args: &[LValue]) -> LResult {
    wait_on_planner(env).await?;
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn new_event(env: &LEnv, _args: &[LValue]) -> LResult {
    wait_on_planner(env).await?;
    Ok(LValue::Nil)
}

/*
async fn _plan_in_ompas(env: &LEnv, args: &[LValue], opt: bool) -> LResult {
    let task: LValue = args.into();
    println!("task to plan: {}", task);
    let acting_manager = env
        .get_context::<ModControl>(MOD_CONTROL)?
        .acting_manager
        .clone();
    let ctx = env.get_context::<ModModel>(MOD_MODEL)?;
    //let mut context: ConversionContext = ctx.get_conversion_context().await;
    let mut env: LEnv = ctx.get_plan_env().await;
    let state = ctx.get_plan_state().await;

    env.update_context(ModState::new_from_snapshot(state));

    let debug = LValue::from(args).to_string();
    let args = args.iter().map(|lv| lv.as_cst().unwrap()).collect();

    let _pr = acting_manager.new_high_level_task(debug, args).await;
    let mut recv: broadcast::Receiver<bool> =
        acting_manager.subscribe_on_plan_update().await.unwrap();
    recv.recv()
        .await
        .expect("Error while waiting on plan update.");
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn plan_in_ompas(env: &LEnv, args: &[LValue]) -> LResult {
    _plan_in_ompas(env, args, false).await
}

#[async_scheme_fn]
pub async fn plan_opt_in_ompas(env: &LEnv, args: &[LValue]) -> LResult {
    _plan_in_ompas(env, args, true).await
}*/
