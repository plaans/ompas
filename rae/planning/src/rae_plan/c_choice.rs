use crate::rae_plan::ctx_cost::{CtxCost, CTX_COST};
use ompas_rae_structs::contexts::ctx_domain::{CtxDomain, CTX_DOMAIN};
use ompas_rae_structs::domain::RAEDomain;
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

pub const INCREASE_COST: &str = "increase-cost";
pub const MOD_C_CHOICE: &str = "CChoice";
pub const MOD_COST: &str = "Cost";

pub struct CtxCChoice {}

impl IntoModule for CtxCChoice {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_C_CHOICE.to_string(),
        };

        module.add_fn_prelude(INCREASE_COST, increase_cost);
        module
    }

    fn documentation(&self) -> Documentation {
        todo!()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        todo!()
    }
}

#[scheme_fn]
pub fn increase_cost(env: &LEnv, cost: u64) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxCost>(MOD_COST)?;
    ctx.increase_cost(cost);
    Ok(())
}

#[async_scheme_fn]
pub async fn c_choice(env: &LEnv, task: &[LValue]) -> LResult {
    let new_env = env.clone();
    let domain = env.get_context::<CtxDomain>(CTX_DOMAIN).unwrap();
    let mut new_env: LEnv = create_env(new_env, &domain.domain).await;
    //let map: HashMap<LValue, u64> = Default::default();
    let mut method = LValue::Nil;
    let mut cost = None;

    //***//
    //Compute for all methods their costs

    let methods: Vec<LValue> = vec![];

    for m in &methods {
        new_env.import_context(Context::new(CtxCost::default()), CTX_COST);
        eval(m, &mut new_env, None).await;
        let c_new = new_env.get_context::<CtxCost>(CTX_COST).unwrap().get_cost();
        match &cost {
            None => {
                cost = Some(c_new);
                method = m.clone();
            }
            Some(mut c_old) => {
                if c_old > c_new {
                    c_old = c_new;
                    method = m.clone();
                }
            }
        }
    }

    env.get_context::<CtxCost>(CTX_COST)
        .unwrap()
        .increase_cost(cost.unwrap_or(0));

    //let methods = vec![];
    //let mut cost: Option<u64> = None;
    //for m in methods {}
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
            "(lambda {} (do ({} {}) (increase-cost ({} {}))))",
            lv_params,
            params,
            command.get_model(),
            params,
            command.get_cost()
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
