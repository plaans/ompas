use crate::exec::refinement::greedy_select;
use crate::exec::state::ModState;
use crate::exec::ModExec;
use ompas_language::exec::c_choice::*;
use ompas_language::exec::state::MOD_STATE;
use ompas_structs::acting_domain::OMPASDomain;
use ompas_structs::interface::select_mode::{CChoiceConfig, Planner, SelectMode};
use ompas_structs::state::world_state::WorldStateSnapshot;
use ompas_structs::supervisor::process::task::RefinementTrace;
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
    domain: Arc<RwLock<OMPASDomain>>,
}

impl From<ModCChoice> for Context {
    fn from(m: ModCChoice) -> Self {
        Context::new(m, MOD_C_CHOICE)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Cost {
    Inf,
    Some(f64),
}

impl Default for Cost {
    fn default() -> Self {
        Self::Some(0.0)
    }
}

impl Add for Cost {
    type Output = Cost;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Cost::Some(f1), Cost::Some(f2)) => Cost::Some(f1 + f2),
            _ => Cost::Inf,
        }
    }
}

impl PartialEq<Self> for Cost {
    fn eq(&self, _: &Self) -> bool {
        todo!()
    }
}

impl PartialOrd for Cost {
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

impl Display for Cost {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Cost::Inf => write!(f, "{}", INF),
            Cost::Some(u) => write!(f, "{}", u),
        }
    }
}

impl TryFrom<&LValue> for Cost {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(Cost::Some(n.into())),
            LValue::Symbol(s) => {
                if s.as_str() == INF {
                    Ok(Cost::Inf)
                } else {
                    Err(Default::default())
                }
            }
            _ => Err(Default::default()),
        }
    }
}

impl From<Cost> for LValue {
    fn from(c: Cost) -> Self {
        match c {
            Cost::Inf => INF.into(),
            Cost::Some(u) => u.into(),
        }
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
            domain: Arc::new(Default::default()),
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
    let state = state.state.get_snapshot().await;
    //let map: HashMap<LValue, u64> = Default::default();
    let mut method = LValue::Nil;
    let mut cost = None;

    let ctx = env.get_context::<ModCChoice>(MOD_C_CHOICE)?;
    let level = ctx.level.load(Ordering::Relaxed);

    let mut methods: Vec<LValue> = greedy_select(state.clone(), &ctx.tried, task.to_vec(), env)
        .await?
        .applicable_methods;
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
    state: WorldStateSnapshot,
    tried: &Vec<LValue>,
    task: Vec<LValue>,
    env: &LEnv,
    config: CChoiceConfig,
) -> lruntimeerror::Result<RefinementTrace> {
    let new_env = env.clone();
    let ctx = env.get_context::<ModCChoice>(MOD_C_CHOICE).unwrap();

    let mut new_env: LEnv = c_choice_env(new_env, &ctx.domain.read().await.clone()).await;
    new_env.import_module(ModCChoice::new_from_tried(tried.to_vec(), 0), WithoutPrefix);
    new_env.update_context(ModState::new_from_snapshot(state.clone()));

    let mut greedy: RefinementTrace = greedy_select(state, tried, task.clone(), env).await?;
    greedy.refinement_type = SelectMode::Planning(Planner::CChoice(config));

    let method: LValue = eval(&task.into(), &mut new_env, None).await?;

    greedy.choosed = method;
    greedy
        .interval
        .set_end(env.get_context::<ModTime>(MOD_TIME)?.get_instant().await);

    Ok(greedy)
}
