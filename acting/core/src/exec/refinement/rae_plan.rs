use crate::exec::refinement::c_choice::Cost;
use crate::exec::refinement::greedy_select;
use crate::exec::state::ModState;
use crate::exec::ModExec;
use ompas_language::exec::rae_plan::*;
use ompas_language::exec::state::MOD_STATE;
use ompas_structs::acting_domain::OMPASDomain;
use ompas_structs::interface::select_mode::{Planner, RAEPlanConfig, SelectMode};
use ompas_structs::state::world_state::WorldStateSnapshot;
use ompas_structs::supervisor::interval::Timepoint;
use ompas_structs::supervisor::process::task::{RTSelect, RefinementInner, SelectTrace};
use rand::prelude::SliceRandom;
use sompas_core::{eval, parse};
use sompas_language::time::MOD_TIME;
use sompas_macros::async_scheme_fn;
use sompas_modules::time::ModTime;
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

#[derive(Copy, Clone, Debug)]
pub enum Efficiency {
    Inf,
    Some(f64),
}

impl Default for Efficiency {
    fn default() -> Self {
        Self::Inf
    }
}

impl Display for Efficiency {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Efficiency::Inf => write!(f, "{}", INF),
            Efficiency::Some(u) => write!(f, "{}", u),
        }
    }
}

impl PartialEq<Self> for Efficiency {
    fn eq(&self, _: &Self) -> bool {
        todo!()
    }
}

impl PartialOrd for Efficiency {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Inf, Self::Inf) => Some(cmp::Ordering::Equal),
            (Self::Inf, Self::Some(_)) => Some(cmp::Ordering::Greater),
            (Self::Some(_), Self::Inf) => Some(cmp::Ordering::Less),
            (Self::Some(f1), Self::Some(f2)) => {
                if f1 == f2 {
                    Some(cmp::Ordering::Equal)
                } else if f1 < f2 {
                    Some(cmp::Ordering::Less)
                } else {
                    Some(cmp::Ordering::Greater)
                }
            }
        }
    }
}

impl Efficiency {
    pub fn compose(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::Inf, Self::Inf) => Self::Inf,
            (Self::Inf, Self::Some(u)) => Self::Some(u),
            (Self::Some(u), Self::Inf) => Self::Some(u),
            (Self::Some(u1), Self::Some(u2)) => Self::Some((u1 * u2) / (u1 + u2)),
        }
    }
}

impl From<Cost> for Efficiency {
    fn from(c: Cost) -> Self {
        match c {
            Cost::Inf => Efficiency::Some(0.0),
            Cost::Some(f) => {
                if f == 0.0 {
                    Efficiency::Inf
                } else {
                    Efficiency::Some(1.0 / f)
                }
            }
        }
    }
}

impl From<Efficiency> for Cost {
    fn from(e: Efficiency) -> Self {
        match e {
            Efficiency::Inf => Cost::Some(0.0),
            Efficiency::Some(f) => {
                if f == 0.0 {
                    Cost::Inf
                } else {
                    Cost::Some(1.0 / f)
                }
            }
        }
    }
}

impl TryFrom<&LValue> for Efficiency {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(Efficiency::Some(n.into())),
            LValue::Symbol(s) => {
                if s.as_str() == INF {
                    Ok(Efficiency::Inf)
                } else {
                    Err(Default::default())
                }
            }
            _ => Err(Default::default()),
        }
    }
}

pub struct ModRaePlan {
    tried: Vec<LValue>,
    efficiency: Arc<RwLock<Efficiency>>,
    config: RAEPlanConfig,
    level: Arc<AtomicU64>,
    domain: Arc<RwLock<OMPASDomain>>,
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
        *self.efficiency.write().await = Efficiency::compose(o_e, e)
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
        module.add_async_fn(RAE_PLAN, rae_plan, DOC_RAE_PLAN, false);
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
    let state = state.state.get_snapshot().await;
    //let map: HashMap<LValue, u64> = Default::default();
    let mut method = LValue::Nil;
    let mut efficiency: Option<Efficiency> = None;

    let ctx = env.get_context::<ModRaePlan>(MOD_RAE_PLAN)?;
    let level = ctx.level.load(Ordering::Relaxed);

    let mut methods: Vec<LValue> = greedy_select(&state, &ctx.tried, task.to_vec(), env)
        .await?
        .possibilities;
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
            "c_choice for {}({}): method = {} ; efficiency = {:?}",
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
        "c_choice for {}({}): selected method = {} ; efficiency = {:?}",
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
            command.get_model(),
            params,
            command.get_cost(),
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
    state: &WorldStateSnapshot,
    mut greedy: RefinementInner,
    env: &LEnv,
    config: RAEPlanConfig,
) -> lruntimeerror::Result<RefinementInner> {
    let new_env = env.clone();
    let ctx = env.get_context::<ModRaePlan>(MOD_RAE_PLAN).unwrap();

    let mut new_env: LEnv = rae_plan_env(new_env, &ctx.domain.read().await.clone()).await;
    new_env.import_module(ctx.new_from_tried(greedy.tried.to_vec(), 0), WithoutPrefix);
    new_env.update_context(ModState::new_from_snapshot(state.clone()));

    greedy.select = SelectTrace::RealTime(RTSelect {
        refinement_type: SelectMode::Planning(Planner::RAEPlan(config)),
    });

    let method: LValue = eval(&greedy.task_value, &mut new_env, None).await?;

    greedy.method_value = method;
    greedy.interval.set_end(Timepoint::new_micros(
        env.get_context::<ModTime>(MOD_TIME)?.get_micros().await,
    ));

    Ok(greedy)
}
