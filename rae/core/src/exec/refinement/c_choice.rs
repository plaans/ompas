use crate::contexts::ctx_state::{CtxState, CTX_STATE};
use crate::exec::refinement::select::greedy_select;
use log::info;
use ompas_rae_structs::domain::RAEDomain;
use rand::prelude::SliceRandom;
use sompas_core::{eval, parse};
use sompas_macros::async_scheme_fn;
use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

pub const INCREASE_COST: &str = "increase_cost";
pub const C_CHOICE: &str = "c_choice";
pub const MOD_C_CHOICE: &str = "mod-c-choice";

pub const DEFAULT_DEPTH: usize = 10;

#[derive(Copy, Clone, Default)]
pub struct RAEPlanConfig {
    //Number of methods to compare.
    b: Option<usize>,
    //Number of simulation for commands.
    k: usize,
    max_depth: Option<usize>,
}

#[derive(Default)]
pub struct CtxCChoice {
    tried: Vec<LValue>,
    cost: Arc<AtomicU64>,
    config: RAEPlanConfig,
    level: Arc<AtomicU64>,
}

impl CtxCChoice {
    pub fn new(tried: Vec<LValue>, level: u64) -> Self {
        Self {
            tried,
            cost: Arc::new(Default::default()),
            config: Default::default(),
            level: Arc::new(AtomicU64::new(level)),
        }
    }
}

impl CtxCChoice {
    pub fn increase_cost(&self, cost: u64) {
        loop {
            let o_cost = self.cost.load(Ordering::Relaxed);
            if self
                .cost
                .compare_exchange(o_cost, o_cost + cost, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                break;
            };
        }
    }

    pub fn increase_level(&self) {
        loop {
            let o_level = self.level.load(Ordering::Relaxed);
            if self
                .cost
                .compare_exchange(o_level, o_level + 1, Ordering::Relaxed, Ordering::Relaxed)
                .is_ok()
            {
                break;
            };
        }
    }

    pub fn get_cost(&self) -> u64 {
        self.cost.load(Ordering::Relaxed)
    }
}

impl IntoModule for CtxCChoice {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_C_CHOICE.to_string(),
        };

        module.add_fn_prelude(INCREASE_COST, increase_cost);
        module.add_async_fn_prelude(C_CHOICE, c_choice);
        module
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

#[scheme_fn]
pub fn increase_cost(env: &LEnv, cost: u64) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxCChoice>(MOD_C_CHOICE)?;
    ctx.increase_cost(cost);
    Ok(())
}

#[async_scheme_fn]
pub async fn c_choice(env: &LEnv, task: &[LValue]) -> LResult {
    let state = env.get_context::<CtxState>(CTX_STATE)?;
    let state = state.state.get_snapshot().await;
    //let map: HashMap<LValue, u64> = Default::default();
    let mut method = LValue::Nil;
    let mut cost = None;

    let ctx = env.get_context::<CtxCChoice>(MOD_C_CHOICE)?;
    let level = ctx.level.load(Ordering::Relaxed);

    let mut methods: Vec<LValue> = greedy_select(state.clone(), &ctx.tried, task.to_vec(), env)
        .await?
        .applicable_methods;
    if let Some(b) = ctx.config.b {
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
        new_env.import_context(
            Context::new(CtxCChoice::new(vec![], level + 1)),
            MOD_C_CHOICE,
        );
        new_env.import_context(Context::new(CtxState::new(state.clone().into())), CTX_STATE);
        eval(m, &mut new_env, None).await?;
        let c_new = new_env
            .get_context::<CtxCChoice>(MOD_C_CHOICE)
            .unwrap()
            .get_cost();
        println!(
            "c_choice for {}({}): method = {} ; cost = {:?}",
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

    env.get_context::<CtxCChoice>(MOD_C_CHOICE)
        .unwrap()
        .increase_cost(cost.unwrap_or(0));

    println!(
        "c_choice for {}({}): selected method = {} ; cost = {:?}",
        LValue::from(task),
        level,
        method,
        cost
    );
    Ok(method)
}

pub async fn create_env(mut env: LEnv, domain: &RAEDomain) -> LEnv {
    for (label, command) in &domain.commands {
        let lv_params = command.get_parameters().get_params_as_lvalue();
        let mut params = "".to_string();
        for param in command.get_parameters().get_params() {
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
        for param in task.get_parameters().get_params() {
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
