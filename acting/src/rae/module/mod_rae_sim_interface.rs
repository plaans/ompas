use crate::rae::context::rae_env::{
    RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP, RAE_TASK_METHODS_MAP,
};
use crate::rae::module::mod_rae_sim::{COMPUTE_SCORE, EVAL_PRE_CONDITIONS};
use ::macro_rules_attribute::macro_rules_attribute;
use core::mem;
use im::HashMap;
use ompas_lisp::core::{eval, ContextCollection, LEnv};
use ompas_lisp::functions::{append, cons};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::modules::utils::{enumerate, quote_list};
use ompas_lisp::structs::LCoreOperator::Quote;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::LValue::{Nil, True};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_utils::dyn_async;
use std::convert::TryInto;
use std::sync::Arc;

/*
LANGAUGE
 */

pub const CHECK_PRECONDITIONS: &str = "check-preconditions";
pub const GENERATE_APPLICABLE_INSTANCES: &str = "generate-applicable-instances";
pub const MOD_CTX_RAE_SIM_INTERFACE: &str = "rae-sim-interface";
pub const STATE: &str = "state";
pub const LAMBDA_IS_APPLICABLE: &str = "(define applicable? \
    (lambda args\
        (check_preconditions args (rae-get-state))))";

pub struct CtxRaeSimInterface {
    pub sim_env: LEnv,
    pub sim_ctxs: ContextCollection,
}

impl GetModule for CtxRaeSimInterface {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![LAMBDA_IS_APPLICABLE].into(),
            label: MOD_CTX_RAE_SIM_INTERFACE.to_string(),
        };
        module.add_async_fn_prelude(CHECK_PRECONDITIONS, check_preconditions);
        module.add_async_fn_prelude(COMPUTE_SCORE, compute_score);
        module.add_async_fn_prelude(GENERATE_APPLICABLE_INSTANCES, generate_applicable_instances);
        module
    }
}

impl CtxRaeSimInterface {
    pub fn new(sim_env: LEnv, sim_ctxs: ContextCollection) -> Self {
        Self { sim_env, sim_ctxs }
    }

    pub fn add_domain_sim(&mut self, mut domain_sim: LEnv) {
        let env = mem::take(&mut self.sim_env);
        domain_sim.set_outer(env);
        self.sim_env = domain_sim;
    }
}

/// Example:
/// (check-preconditions (<method> <params>) state)
#[macro_rules_attribute(dyn_async!)]
pub async fn check_preconditions<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a CtxRaeSimInterface,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        Err(WrongNumberOfArgument(
            CHECK_PRECONDITIONS,
            args.into(),
            args.len(),
            2..2,
        ))
    } else {
        let instantiated_method = &args[0];
        let state = &args[1];

        let mut new_env = ctx.sim_env.clone();
        let mut new_ctxs = ctx.sim_ctxs.clone();
        new_env.insert(STATE.to_string(), state.clone());
        let lv = cons(
            &[EVAL_PRE_CONDITIONS.into(), instantiated_method.clone()],
            env,
            &(),
        )?;
        eval(&lv, &mut new_env, &mut new_ctxs).await
    }
}
#[macro_rules_attribute(dyn_async!)]
pub async fn compute_score<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a CtxRaeSimInterface,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        Err(WrongNumberOfArgument(
            CHECK_PRECONDITIONS,
            args.into(),
            args.len(),
            2..2,
        ))
    } else {
        let instantiated_method = &args[0];
        let state = &args[1];

        let mut new_env = ctx.sim_env.clone();
        new_env.insert(STATE.to_string(), state.clone());
        let lv = cons(
            &[COMPUTE_SCORE.into(), instantiated_method.clone()],
            env,
            &(),
        )?;

        eval(&lv, &mut new_env, &mut ctx.sim_ctxs.clone()).await
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn generate_applicable_instances<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a CtxRaeSimInterface,
) -> Result<LValue, LError> {
    let mut list_applicable_methods: Vec<LValue> = vec![];

    let state = &args[0];
    let map_state: HashMap<LValue, LValue> = state.try_into()?;
    let task: Vec<LValue> = (&args[1]).try_into()?;
    let task_label = &task[0];
    let n_params = task.len() - 1;
    let mut params: Vec<LValue> = task[1..].into();

    let map_task_methods: HashMap<LValue, LValue> = env
        .get_symbol(RAE_TASK_METHODS_MAP)
        .expect("This entry should be in env.")
        .try_into()?;

    let map_methods_types: HashMap<LValue, LValue> = env
        .get_symbol(RAE_METHOD_TYPES_MAP)
        .expect("This entry should be in env")
        .try_into()?;

    let methods: Vec<LValue> = match map_task_methods.get(task_label) {
        None => return Ok(Nil),
        Some(m) => m.clone().try_into()?,
    };

    for m in methods {
        let types = map_methods_types.get(&m).expect("Entry should be defined");
        let vec_types: Vec<LValue> = types.try_into()?;
        if vec_types.len() > n_params {
            for t in &vec_types[n_params..] {
                let mut sym_type: String = t.try_into()?;
                sym_type.push('s');
                let instances_of_type = map_state
                    .get(&LValue::Symbol(sym_type))
                    .expect("this kind should exist in the state")
                    .clone();
                params.push(instances_of_type);
            }

            let instances_of_params: Vec<LValue> =
                enumerate(params.as_slice(), env, &())?.try_into()?;

            for element in instances_of_params {
                let method = cons(&[m.clone(), element.clone()], env, &())?;
                let lv = quote_list(&[method.clone()], env, &())?;

                match check_preconditions(&[lv.clone(), state.clone()], env, ctx).await? {
                    LValue::True => {
                        let score = compute_score(&[lv.clone(), state.clone()], env, ctx).await?;
                        list_applicable_methods.push(vec![method, score].into())
                    }
                    LValue::Nil => {}
                    _ => unreachable!(),
                }
            }
        } else {
            let method = cons(&[m.clone(), params.clone().into()], env, &())?;
            let lv = quote_list(&[method.clone()], env, &())?;
            match check_preconditions(&[lv.clone(), state.clone()], env, ctx).await? {
                LValue::True => {
                    let score = compute_score(&[lv.clone(), state.clone()], env, ctx).await?;
                    list_applicable_methods.push(vec![method, score].into())
                }
                LValue::Nil => {}
                _ => unreachable!(),
            }
        }
    }

    if list_applicable_methods.is_empty() {
        Ok(Nil)
    } else {
        Ok(list_applicable_methods.into())
    }
}

/*Functions
test_preconditions
test_effects
select
 */
