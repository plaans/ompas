#![allow(dead_code, unused_imports, unused_variables)]
use crate::model::acting_domain::model::ModelKind;
use crate::model::acting_domain::OMPASDomain;
use crate::ompas::interface::select_mode::CChoiceConfig;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::refinement::sampling::cost::Cost;
use crate::ompas::scheme::exec::refinement::{applicable, greedy_select};
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::exec::ModExec;
use ompas_language::exec::c_choice::*;
use ompas_language::exec::state::MOD_STATE;
use rand::prelude::SliceRandom;
use sompas_core::{eval, parse};
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::cmp;
use std::fmt::{Display, Formatter};
use std::ops::Add;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

pub const DEFAULT_DEPTH: usize = 10;

pub struct ModCChoice {
    tried: Vec<LValue>,
    cost: Arc<RwLock<Cost>>,
    config: CChoiceConfig,
    level: Arc<AtomicU64>,
    domain: DomainManager,
}

impl From<ModCChoice> for Context {
    fn from(m: ModCChoice) -> Self {
        Context::new(m, MOD_C_CHOICE)
    }
}

impl ModCChoice {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            tried: vec![],
            cost: Arc::new(Default::default()),
            config: Default::default(),
            level: Arc::new(Default::default()),
            domain: exec.domain.clone(),
        }
    }

    pub fn new_from_tried(tried: Vec<LValue>, level: u64) -> Self {
        Self {
            tried,
            cost: Arc::new(RwLock::new(Cost::Some(0.0))),
            config: Default::default(),
            level: Arc::new(AtomicU64::new(level)),
            domain: Default::default(),
        }
    }
}

impl ModCChoice {
    pub async fn increase_cost(&self, c: Cost) {
        let o_c: Cost = *self.cost.read().await;
        *self.cost.write().await = o_c + c;
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

    pub async fn get_cost(&self) -> Cost {
        *self.cost.read().await
    }
}

impl From<ModCChoice> for LModule {
    fn from(m: ModCChoice) -> Self {
        let mut module = LModule::new(m, MOD_C_CHOICE, DOC_MOD_C_CHOICE);
        module.add_async_fn(INCREASE_COST, increase_cost, DOC_INCREASE_COST, false);
        module.add_async_fn(C_CHOICE, c_choice, DOC_C_CHOICE, false);
        module
    }
}

#[async_scheme_fn]
pub async fn increase_cost(env: &LEnv, cost: Cost) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<ModCChoice>(MOD_C_CHOICE)?;
    ctx.increase_cost(cost).await;
    Ok(())
}

#[async_scheme_fn]
pub async fn c_choice(env: &LEnv, task: &[LValue]) -> LResult {
    let state = env.get_context::<ModState>(MOD_STATE)?;
    let state = state.state_manager.get_snapshot().await;
    //let map: HashMap<LValue, u64> = Default::default();
    let mut method = LValue::Nil;
    let mut cost = None;

    let ctx = env.get_context::<ModCChoice>(MOD_C_CHOICE)?;
    let level = ctx.level.load(Ordering::Relaxed);

    let mut methods: Vec<LValue> = applicable(&state, task, env).await?;
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
        new_env.update_context(ModCChoice::new_from_tried(vec![], level + 1));
        new_env.update_context(ModState::new_from_snapshot(state.clone()));
        eval(m, &mut new_env, None).await?;
        let c_new = new_env
            .get_context::<ModCChoice>(MOD_C_CHOICE)
            .unwrap()
            .get_cost()
            .await;
        println!(
            "c_choice for {}({}): method = {} ; cost = {}",
            LValue::from(task),
            level,
            m,
            c_new
        );
        match &cost {
            None => {
                cost = Some(c_new);
                method = m.clone();
            }
            Some(c_old) => {
                if *c_old > c_new {
                    println!("replacing {} by {}", method, m);
                    cost = Some(c_new);
                    method = m.clone();
                }
            }
        }
        println!("End computing cost for {}({})", m, level);
    }

    env.get_context::<ModCChoice>(MOD_C_CHOICE)
        .unwrap()
        .increase_cost(cost.unwrap_or(Cost::Some(0.0)))
        .await;

    println!(
        "c_choice for {}({}): selected method = {} ; cost = {:?}",
        LValue::from(task),
        level,
        method,
        cost
    );
    Ok(method)
}

pub async fn c_choice_env(mut env: LEnv, domain: &OMPASDomain) -> LEnv {
    for (label, command) in &domain.commands {
        let lv_params = command.get_parameters().get_params_as_lvalue();
        let mut params = "".to_string();
        for param in command.get_parameters().get_labels() {
            params.push_str(param.to_string().as_str());
            params.push(' ');
        }
        let model_expr = format!(
            "(lambda {} (do ({} {}) (increase_cost ({} {}))))",
            lv_params,
            command.get_model(&ModelKind::SimModel).unwrap(),
            params,
            command.get_cost().unwrap(),
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
        let model_expr = format!("(lambda {} (c_choice '{} {}))", lv_params, label, params);
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

pub async fn c_choice_select(
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
    config: CChoiceConfig,
) -> lruntimeerror::Result<LValue> {
    let new_env = env.clone();
    let ctx = env.get_context::<ModCChoice>(MOD_C_CHOICE).unwrap();
    //
    // let mut new_env: LEnv = c_choice_env(new_env, &ctx.domain.get_inner().await).await;
    // new_env.import_module(
    //     ModCChoice::new_from_tried(greedy.tried.to_vec(), 0),
    //     WithoutPrefix,
    // );
    // new_env.update_context(ModState::new_from_snapshot(state.clone()));
    //
    // greedy.select = SelectKind::RealTime(RTSelect {
    //     refinement_type: SelectMode::Planning(Planner::CChoice(config)),
    // });
    // let method: LValue = eval(&greedy.task_value, &mut new_env, None).await?;
    //
    // let now = env
    //     .get_context::<ModExec>(MOD_EXEC)
    //     .unwrap()
    //     .acting_manager
    //     .clock_manager
    //     .now();
    // greedy.selected = method;
    // greedy.duration.set_end(now);
    //
    // Ok(greedy)

    greedy_select(candidates, state, env)
}
