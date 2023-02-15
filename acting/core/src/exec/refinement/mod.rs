pub mod aries;
pub mod c_choice;
pub mod rae_plan;

use crate::exec::context::ModContext;
use crate::exec::refinement::aries::aries_select;
use crate::exec::refinement::c_choice::c_choice_select;
use crate::exec::refinement::rae_plan::rae_plan_select;
use crate::exec::state::{instance, ModState};
use crate::exec::ModExec;
use crate::RaeExecError;
use ompas_language::exec::context::MOD_CONTEXT;
use ompas_language::exec::refinement::*;
use ompas_middleware::logger::LogClient;
use ompas_structs::acting_domain::OMPASDomain;
use ompas_structs::agenda::Agenda;
use ompas_structs::interface::rae_options::OMPASOptions;
use ompas_structs::interface::select_mode::{Planner, SelectMode};
use ompas_structs::interval::Interval;
use ompas_structs::rae_options::OMPASOptions;
use ompas_structs::select_mode::{Planner, SelectMode};
use ompas_structs::state::action_state::{
    ActionMetaData, ActionMetaDataView, RefinementMetaData, TaskMetaData,
};
use ompas_structs::state::action_status::ActionStatus;
use ompas_structs::state::world_state::{WorldState, WorldStateSnapshot};
use ompas_structs::supervisor::task::TaskProcess;
use ompas_structs::supervisor::{ActingProcessId, Supervisor};
use rand::prelude::SliceRandom;
use sompas_core::eval;
use sompas_core::modules::list::cons;
use sompas_language::time::MOD_TIME;
use sompas_macros::async_scheme_fn;
use sompas_macros::*;
use sompas_modules::time::ModTime;
use sompas_modules::utils::enumerate;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lenv::LEnv;
use sompas_structs::lmodule::LModule;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror, wrong_type};
use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct ModRefinement {
    //pub env: LEnv,
    pub domain: Arc<RwLock<OMPASDomain>>,
    pub supervisor: Supervisor,
    pub state: WorldState,
    pub options: Arc<RwLock<OMPASOptions>>,
    pub log: LogClient,
}

impl ModRefinement {
    pub fn new(exec: &ModExec) -> Self {
        Self {
            domain: exec.domain.clone(),
            log: exec.log.clone(),
            supervisor: exec.supervisor.clone(),
            state: exec.state.clone(),
            options: exec.options.clone(),
        }
    }
}

impl From<ModRefinement> for LModule {
    fn from(m: ModRefinement) -> Self {
        let mut module = LModule::new(m, MOD_REFINEMENT, DOC_MOD_REFINEMENT);
        module.add_async_fn(REFINE, refine, DOC_REFINE, false);
        module.add_async_fn(SET_SUCCESS, set_success, DOC_SET_SUCCESS, false);
        module.add_async_fn(_RETRY, _retry, DOC__RETRY, false);
        module.add_fn(IS_SUCCESS, is_success, DOC_IS_SUCCESS, false);
        module.add_fn(IS_FAILURE, is_failure, DOC_IS_FAILURE, false);

        //Lambdas
        module.add_lambda(EXEC_TASK, LAMBDA_EXEC_TASK, DOC_EXEC_TASK);
        module.add_lambda(RETRY, LAMBDA_RETRY, DOC_RETRY);
        module.add_lambda(
            __GET_PRECONDITIONS__,
            LAMBDA___GET_PRECONDITIONS__,
            DOC___GET_PRECONDITIONS__,
        );
        module.add_lambda(__GET_SCORE__, LAMBDA___GET_SCORE__, DOC___GET_SCORE__);
        module.add_lambda(
            __GET_COMMAND_MODEL__,
            LAMBDA___GET_COMMAND_MODEL__,
            DOC___GET_COMMAND_MODEL__,
        );
        module.add_lambda(
            __EVAL_PRE_CONDITIONS__,
            LAMBDA___EVAL_PRE_CONDITIONS__,
            DOC___EVAL_PRE_CONDITIONS__,
        );
        module.add_lambda(
            __COMPUTE_SCORE__,
            LAMBDA___COMPUTE_SCORE__,
            DOC___COMPUTE_SCORE__,
        );
        module.add_lambda(IS_APPLICABLE, LAMBDA_IS_APPLICABLE, DOC_IS_APPLICABLE);
        module.add_lambda(
            __GENERATE_APPLICABLE_INSTANCES__,
            LAMBDA___GENERATE_APPLICABLE_INSTANCES__,
            DOC___GENERATE_APPLICABLE_INSTANCES__,
        );
        module.add_lambda(
            __R_GENERATE_INSTANCES__,
            LAMBDA___R_GENERATE_INSTANCES__,
            DOC___R_GENERATE_INSTANCES__,
        );
        module.add_lambda(
            __R_TEST_METHOD__,
            LAMBDA_R_TEST_METHOD,
            DOC___R_TEST_METHOD__,
        );

        module
    }
}

#[async_scheme_fn]
pub async fn refine(env: &LEnv, args: &[LValue]) -> LResult {
    let task_label: LValue = args.into();
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let log = ctx.log.clone();
    let task_id: ActingProcessId = env
        .get_context::<ModContext>(MOD_CONTEXT)
        .unwrap()
        .process_ref
        .as_id()
        .unwrap();

    let result: LValue = select(task_id, env)
        .await
        .map_err(|e: LRuntimeError| e.chain("select"))?;

    let first_m = result;

    let instant = ctx.supervisor.get_instant().await;

    let task: &mut TaskProcess = ctx
        .supervisor
        .inner
        .write()
        .await
        .get_mut(task_id)
        .unwrap()
        .try_task();

    let result: LValue = if first_m == LValue::Nil {
        log.error(format!(
            "No applicable method for task {task_label}({task_id})"
        ))
        .await;
        task.set_status(ActionStatus::Failure);
        task.set_end(instant);
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.update_status(ActionStatus::Running(None));
        first_m
    };

    Ok(result)
}

#[async_scheme_fn]
pub async fn set_success(env: &LEnv) -> LResult {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    let task_id = env
        .get_context::<ModContext>(MOD_CONTEXT)?
        .process_ref
        .as_id()
        .unwrap();

    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;

    let instant = ctx.supervisor.get_instant().await;

    let task: &mut TaskProcess = ctx
        .supervisor
        .inner
        .write()
        .await
        .get_mut(task_id)
        .unwrap()
        .try_task();

    task.set_status(ActionStatus::Success);
    task.set_end(instant);

    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn _retry(env: &LEnv) -> LResult {
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let log = ctx.log.clone();
    let mut task: TaskMetaData = ctx.agenda.get_task(&(task_id as usize)).await?;
    let task_label = task.get_label().clone();
    log.error(format!("Retrying task {task_label}({task_id})"))
        .await;
    task.add_tried_method(task.get_current_method().clone());
    task.set_current_method(LValue::Nil);
    let new_method: LValue = select(&mut task, env).await?;
    let result: LValue = if new_method == LValue::Nil {
        log.error(format!(
            "No more method for task {task_label}({task_id}). Task is a failure!",
        ))
        .await;
        task.update_status(ActionStatus::Failure);
        task.set_end_timepoint(ctx.agenda.get_instant());
        ctx.agenda.update_task(&task_id, task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.set_current_method(new_method.clone());
        ctx.agenda.update_task(&task_id, task).await;
        new_method
    };

    Ok(result)
}

/*const LAMBDA_SELECT: &str = "
(define select
  (lambda (task)
    (sim_block
    (rae-select task (generate_applicable_instances task)))))))";*/

pub async fn select(task: LValue, task_id: task_id, env: &LEnv) -> LResult {
    /*
    Each function return an ordered list of methods
     */
    let mod_refinement = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let log = mod_refinement.log.clone();
    let supervisor = &mod_refinement.supervisor;
    let state: WorldStateSnapshot = mod_refinement.state.get_snapshot().await;
    let select_mode: SelectMode = *mod_refinement.options.read().await.get_select_mode();

    let task: Vec<LValue> = task.try_into()?;
    let tried = supervisor.get_tried_method(task_id);
    let rmd: RefinementMetaData = match &select_mode {
        SelectMode::Greedy => {
            /*
            Returns all applicable methods sorted by their score
             */
            log.debug(format!("select greedy for {}", stack.get_label()))
                .await;
            greedy_select(state, tried, task, env)
                .await
                .map_err(|e| e.chain("greedy_select"))?
        }
        SelectMode::Planning(Planner::Aries(bool)) => {
            log.debug(format!("select with aries for {}", stack.get_label()))
                .await;
            aries_select(state, tried, task, env, *bool)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        SelectMode::Planning(Planner::CChoice(config)) => {
            log.debug(format!("select with c-choice for {}", stack.get_label()))
                .await;
            c_choice_select(state, tried, task, env, *config)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        SelectMode::Planning(Planner::RAEPlan(config)) => {
            log.debug(format!("select with c-choice for {}", stack.get_label()))
                .await;
            rae_plan_select(state, tried, task, env, *config)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        _ => todo!(),
    };

    log.debug(format!(
        "sorted_methods for {}({}): {}",
        stack.get_label(),
        task_id,
        LValue::from(&rmd.applicable_methods)
    ))
    .await;

    let method = rmd.choosed.clone();

    stack.set_current_method(method.clone());
    stack.add_refinement(rmd);
    Ok(method)
}

#[scheme_fn]
pub fn success(args: &[LValue]) -> LValue {
    list![LValue::from(SET_SUCCESS), args.into()]
}
#[scheme_fn]
pub fn failure(args: &[LValue]) -> LValue {
    list![LValue::from(FAILURE), args.into()]
}

#[scheme_fn]
pub fn is_failure(list: Vec<LValue>) -> Result<bool, LRuntimeError> {
    if let LValue::Symbol(s) = &list[0] {
        match s.as_str() {
            SET_SUCCESS => Ok(false),
            FAILURE => Ok(true),
            _ => Err(wrong_type!(
                IS_FAILURE,
                &list[0],
                KindLValue::Other("{success,failure}".to_string())
            )),
        }
    } else {
        Err(wrong_type!(IS_FAILURE, &list[0], KindLValue::Symbol))
    }
}

#[scheme_fn]
pub fn is_success(list: Vec<LValue>) -> Result<bool, LRuntimeError> {
    if let LValue::Symbol(s) = &list[0] {
        match s.as_str() {
            SET_SUCCESS => Ok(true),
            FAILURE => Ok(false),
            _ => Err(wrong_type!(
                IS_SUCCESS,
                &list[0],
                KindLValue::Other("{success,failure}".to_string())
            )),
        }
    } else {
        Err(wrong_type!(IS_FAILURE, &list[0], KindLValue::Symbol))
    }
}

pub async fn greedy_select(
    state: WorldStateSnapshot,
    tried: &[LValue],
    task: Vec<LValue>,
    env: &LEnv,
) -> lruntimeerror::Result<RefinementMetaData> {
    /*
    Steps:
    - Create a new entry in the agenda
    - Generate all instances of applicable methods
    - Select the best method
    - Store the stack
    - Return (best_method, task_id)
     */
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let time = env.get_context::<ModTime>(MOD_TIME)?;

    //println!("task to test in greedy: {}", LValue::from(task.clone()));
    let start = time.get_instant().await;

    let task_label = task[0].to_string();
    //let task_string = LValue::from(task.clone()).to_string();
    let params: Vec<LValue> = task[1..].iter().map(|lv| list![lv.clone()]).collect();

    let mut applicable_methods: Vec<(LValue, i64)> = vec![];

    let mut env = env.clone();

    env.update_context(ModState::new_from_snapshot(state));
    let env = &env;

    let domain: OMPASDomain = ctx.domain.read().await.clone();

    let method_templates: Vec<String> =
        domain.tasks.get(&task_label).unwrap().get_methods().clone();

    for template in &method_templates {
        let method_template = domain.methods.get(template).unwrap();
        let types: Vec<LValue> = method_template
            .parameters
            .get_types_as_lvalue()
            .try_into()?;
        /*let types: Vec<LValue> = get(
            env,
            &[
                env.get_symbol(RAE_METHOD_TYPES_MAP).unwrap(),
                template.clone(),
            ],
        )?
        .try_into()?;*/

        let score_lambda = method_template.lambda_score.clone();
        let pre_conditions_lambda = method_template.lambda_pre_conditions.clone();

        let mut instances_template = vec![template.into()];
        instances_template.append(&mut params.clone());

        for t in &types[params.len()..] {
            instances_template.push(instance(env, &[t.clone()]).await?);
        }

        let mut instances_template: Vec<LValue> =
            enumerate(env, &instances_template)?.try_into()?;

        /*println!(
            "instances for template {}: {}",
            template,
            LValue::from(instances_template.clone())
        );*/

        let iter = instances_template.drain(..);

        for i in iter {
            let i_vec: Vec<LValue> = i.borrow().try_into()?;
            let arg = cons(env, &[pre_conditions_lambda.clone(), i_vec[1..].into()])?;
            let arg_debug = arg.to_string();
            let lv: LValue = eval(
                &list!(LPrimitive::Enr.into(), list!(LPrimitive::Quote.into(), arg)),
                &mut env.clone(),
                None,
            )
            .await
            .map_err(|e: LRuntimeError| e.chain(format!("eval pre_conditions: {}", arg_debug)))?;
            if !matches!(lv, LValue::Err(_)) {
                let arg = cons(env, &[score_lambda.clone(), i_vec[1..].into()])?;
                let arg_debug = arg.to_string();
                let score: i64 = eval(
                    &list!(LPrimitive::Enr.into(), list!(LPrimitive::Quote.into(), arg)),
                    &mut env.clone(),
                    None,
                )
                .await
                .map_err(|e: LRuntimeError| e.chain(format!("eval score: {}", arg_debug)))?
                .try_into()?;
                applicable_methods.push((i, score))
            }
        }
    }

    /*
    Sort the list
     */
    let mut rng = rand::thread_rng();
    applicable_methods.shuffle(&mut rng);
    applicable_methods.sort_by_key(|a| a.1);
    applicable_methods.reverse();
    let mut methods: Vec<_> = applicable_methods.drain(..).map(|a| a.0).collect();
    methods.retain(|m| !tried.contains(m));
    let choosed = methods.get(0).cloned().unwrap_or(LValue::Nil);

    Ok(RefinementMetaData {
        refinement_type: SelectMode::Greedy,
        applicable_methods: methods,
        choosed,
        plan: None,
        interval: Interval::new(start, Some(ctx.agenda.get_instant())),
    })
}
