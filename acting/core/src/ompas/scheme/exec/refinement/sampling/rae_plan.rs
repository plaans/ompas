#![allow(dead_code, unused_imports, unused_variables)]
use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::OMPASDomain;
use crate::ompas::interface::select_mode::RAEPlanConfig;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::refinement::sampling::cost::Cost;
use crate::ompas::scheme::exec::refinement::sampling::efficiency::Efficiency;
use crate::ompas::scheme::exec::refinement::sampling::Utility;
use crate::ompas::scheme::exec::refinement::{candidates, greedy_select};
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::exec::ModExec;
use ompas_language::exec::rae_plan::*;
use ompas_language::exec::state::MOD_STATE;
use rand::prelude::SliceRandom;
use sompas_core::{eval, parse};
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::cmp;
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

pub const DEFAULT_DEPTH: usize = 10;

pub struct ModRaePlan {
    tried: Vec<LValue>,
    efficiency: Arc<RwLock<Efficiency>>,
    config: RAEPlanConfig,
    level: Arc<AtomicU64>,
    domain: DomainManager,
}

impl From<ModRaePlan> for Context {
    fn from(m: ModRaePlan) -> Self {
        Context::new(m, MOD_RAE_PLAN)
    }
}

impl ModRaePlan {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            tried: vec![],
            efficiency: Arc::new(Default::default()),
            config: Default::default(),
            level: Arc::new(Default::default()),
            domain: exec.domain.clone(),
        }
    }

    pub fn new_from_tried(&self, tried: Vec<LValue>, level: u64) -> Self {
        Self {
            tried,
            efficiency: Arc::new(RwLock::new(Default::default())),
            config: self.config,
            level: Arc::new(AtomicU64::new(level)),
            domain: self.domain.clone(),
        }
    }

    pub async fn compose_efficiency(&self, e: Efficiency) {
        let o_e: Efficiency = *self.efficiency.read().await;
        *self.efficiency.write().await = Efficiency::compose(&o_e, &e)
    }

    pub fn increase_level(&self) {
        loop {
            let o_level = self.level.load(Ordering::Relaxed);
            if self
                .level
                .compare_exchange(o_level, o_level + 1, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                break;
            };
        }
    }

    pub async fn get_efficiency(&self) -> Efficiency {
        *self.efficiency.read().await
    }
}

impl From<ModRaePlan> for LModule {
    fn from(m: ModRaePlan) -> LModule {
        let mut module = LModule::new(m, MOD_RAE_PLAN, DOC_MOD_RAE_PLAN);

        module.add_async_fn(
            COMPOSE_EFFICIENCY,
            compose_efficiency,
            DOC_COMPOSE_EFFICIENCY,
            false,
        );
        //module.add_async_fn(RAE_PLAN, rae_plan, DOC_RAE_PLAN, false);
        module
    }
}

#[async_scheme_fn]
pub async fn compose_efficiency(env: &LEnv, cost: Cost) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModRaePlan>(MOD_RAE_PLAN)?;
    ctx.compose_efficiency(cost.into()).await;
    Ok(())
}

#[async_scheme_fn]
pub async fn rae_plan(env: &LEnv, task: &[LValue]) -> LResult {
    let state = env.get_context::<ModState>(MOD_STATE)?;
    let state = state.state_manager.get_snapshot().await;
    //let map: HashMap<LValue, u64> = Default::default();
    let mut method = LValue::Nil;
    let mut efficiency: Option<Efficiency> = None;

    let ctx = env.get_context::<ModRaePlan>(MOD_RAE_PLAN)?;
    let level = ctx.level.load(Ordering::Relaxed);

    let mut methods: Vec<LValue> = candidates(&state, &ctx.tried, task, env).await?;
    if let Some(b) = ctx.config.get_b() {
        if b < methods.len() {
            async {
                let mut rng = rand::thread_rng();
                methods.shuffle(&mut rng);
                methods = methods[0..b].to_vec();
            }
            .await;
        }
    }
    for m in &methods {
        let mut new_env = env.clone();
        println!("Computing cost for {}({})", m, level);
        new_env.update_context(ctx.new_from_tried(vec![], level + 1));
        new_env.update_context(ModState::new_from_snapshot(state.clone()));
        eval(m, &mut new_env, None).await?;
        let new_efficiency = new_env
            .get_context::<ModRaePlan>(MOD_RAE_PLAN)
            .unwrap()
            .get_efficiency()
            .await;
        println!(
            "rae_plan for {}({}): method = {} ; efficiency = {:?}",
            LValue::from(task),
            level,
            m,
            new_efficiency
        );

        match &efficiency {
            None => {
                efficiency = Some(new_efficiency);
                method = m.clone();
            }
            Some(e) => {
                if *e < new_efficiency {
                    efficiency = Some(new_efficiency);
                    method = m.clone();
                }
            }
        }
        println!("End computing cost for {}({})", m, level);
    }

    env.get_context::<ModRaePlan>(MOD_RAE_PLAN)
        .unwrap()
        .compose_efficiency(efficiency.unwrap_or(Efficiency::Inf))
        .await;

    println!(
        "rae_plan for {}({}): selected method = {} ; efficiency = {:?}",
        LValue::from(task),
        level,
        method,
        efficiency
    );

    Ok(method)
}

pub async fn rae_plan_env(mut env: LEnv, domain: &OMPASDomain) -> LEnv {
    for (label, command) in &domain.commands {
        let lv_params = command.get_parameters().get_params_as_lvalue();
        let mut params = "".to_string();
        for param in command.get_parameters().get_labels() {
            params.push_str(param.to_string().as_str());
            params.push(' ');
        }
        let model_expr = format!(
            "(lambda {} (do ({} {}) (compose_efficiency ({} {}))))",
            lv_params,
            command.get_model(&ModelKind::SimModel).unwrap(),
            params,
            command.get_model(&ModelKind::CostModel).unwrap(),
            params,
        );
        let model = eval(
            &parse(model_expr.as_str(), &mut env).await.unwrap(),
            &mut env,
            None,
        )
        .await
        .unwrap();
        env.insert(label, model);
    }
    for (label, task) in &domain.tasks {
        let lv_params = task.get_parameters().get_params_as_lvalue();
        let mut params = "".to_string();
        for param in task.get_parameters().get_labels() {
            params.push_str(param.to_string().as_str());
            params.push(' ');
        }
        let model_expr = format!("(lambda {} (rae_plan '{} {}))", lv_params, label, params);
        let model = eval(
            &parse(model_expr.as_str(), &mut env).await.unwrap(),
            &mut env,
            None,
        )
        .await
        .unwrap();
        env.insert(label, model);
    }
    env
}

pub async fn rae_plan_select(
    task: &[LValue],
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
    config: RAEPlanConfig,
) -> LResult {
    let new_env = env.clone();
    let ctx = env.get_context::<ModRaePlan>(MOD_RAE_PLAN).unwrap();

    let mut new_env: LEnv = rae_plan_env(new_env, &ctx.domain.get_inner().await).await;
    new_env.import_module(ctx.new_from_tried(vec![], 0), WithoutPrefix);
    new_env.update_context(ModState::new_from_snapshot(state.clone()));

    let method: LValue = eval(&LValue::from(task), &mut new_env, None).await?;
    Ok(method)
}
