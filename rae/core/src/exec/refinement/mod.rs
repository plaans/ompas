pub mod c_choice;
pub mod rae_plan;
pub mod select;

use ompas_rae_structs::select_mode::{Planner, SelectMode};
use ompas_rae_structs::state::action_state::{
    ActionMetaData, ActionMetaDataView, RefinementMetaData, TaskMetaData,
};
use ompas_rae_structs::state::world_state::WorldStateSnapshot;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;

use crate::exec::task::ModTask;
use crate::RaeExecError;
use ompas_middleware::logger::LogClient;
use ompas_rae_language::exec::refinement::*;
use ompas_rae_language::exec::task::MOD_TASK;
use ompas_rae_planning::aries::structs::ConversionCollection;
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::state::action_status::ActionStatus;
use sompas_macros::async_scheme_fn;
use sompas_macros::*;
use sompas_structs::lmodule::LModule;
use sompas_structs::{list, wrong_type};
use std::convert::{TryFrom, TryInto};

pub struct ModRefinement {
    pub env: LEnv,
    pub domain: RAEDomain,
    pub cc: Option<ConversionCollection>,
    pub select_mode: SelectMode,
    pub log: LogClient,
}

impl ModRefinement {
    pub fn new(
        cc: Option<ConversionCollection>,
        domain: RAEDomain,
        env: LEnv,
        select_mode: SelectMode,
        log: LogClient,
    ) -> Self {
        Self {
            domain,
            env,
            select_mode,
            cc,
            log,
        }
    }
}

impl From<ModRefinement> for LModule {
    fn from(m: ModRefinement) -> Self {
        let mut module = LModule::new(m, MOD_REFINEMENT, DOC_MOD_REFINEMENT);
        module.add_async_fn(REFINE, refine, DOC_REFINE, false);
        module.add_async_fn(
            SET_SUCCESS_FOR_TASK,
            set_success_for_task,
            DOC_SET_SUCCESS_FOR_TASK,
            false,
        );
        module.add_async_fn(IS_SUCCESS, is_success, DOC_IS_SUCCESS, false);
        module.add_async_fn(IS_FAILURE, is_failure, DOC_IS_FAILURE, false);

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
    let log = ctx.get_log_client();
    let parent_task: Option<usize> = match env.get_context::<ModTask>(MOD_TASK) {
        Ok(ctx) => ctx.parent_id,
        Err(_) => None,
    };
    let mut task: TaskMetaData = ctx.agenda.add_task(task_label.clone(), parent_task).await;
    let task_id = *task.get_id();
    let result: LValue = select(&mut task, env)
        .await
        .map_err(|e: LRuntimeError| e.chain("select"))?;

    let first_m = result;

    let result: LValue = if first_m == LValue::Nil {
        log.error(format!(
            "No applicable method for task {task_label}({task_id})"
        ))
        .await;
        task.update_status(ActionStatus::Failure);
        task.set_end_timepoint(ctx.agenda.get_instant());
        ctx.agenda.update_task(&task_id, task).await;
        RaeExecError::NoApplicableMethod.into()
    } else {
        task.update_status(ActionStatus::Running(None));
        ctx.agenda.update_task(&task_id, task).await;
        vec![first_m, task_id.into()].into()
    };

    Ok(result)
}

#[async_scheme_fn]
pub async fn set_success_for_task(env: &LEnv, args: &[LValue]) -> LResult {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */
    let task_id: i64 = args[0].borrow().try_into()?;
    let task_id = task_id as usize;

    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let mut task: ActionMetaData = ctx.agenda.trc.get(&task_id).await;
    task.update_status(ActionStatus::Success).await;
    task.set_end_timepoint(ctx.agenda.get_instant());
    ctx.agenda.update_task(&task.get_id(), task).await;
    //ctx.agenda.remove_task(&task_id).await?;
    Ok(LValue::True)
}

#[async_scheme_fn]
pub async fn retry(env: &LEnv, task_id: usize) -> LResult {
    let ctx = env.get_context::<ModRefinement>(MOD_REFINEMENT)?;
    let log = ctx.get_log_client();
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

pub async fn select(stack: &mut TaskMetaData, env: &LEnv) -> LResult {
    /*
    Each function return an ordered list of methods
     */
    let task_id = stack.get_id();
    let ctx_planning = env.get_context::<CtxPlanning>(CTX_PLANNING)?;
    let log = env
        .get_context::<ModRefinement>(MOD_REFINEMENT)?
        .get_log_client();
    let state: WorldStateSnapshot = env
        .get_context::<CtxState>(CTX_STATE)?
        .state
        .get_snapshot()
        .await;

    let task: Vec<LValue> = stack.get_label().try_into()?;
    let tried = stack.get_tried();
    let rmd: RefinementMetaData = match &ctx_planning.select_mode {
        SelectMode::Greedy => {
            /*
            Returns all applicable methods sorted by their score
             */
            log.debug(format!("select greedy for {}", stack.get_label()))
                .await;
            select::greedy_select(state, tried, task, env)
                .await
                .map_err(|e| e.chain("greedy_select"))?
        }
        SelectMode::Planning(Planner::Aries(bool)) => {
            log.debug(format!("select with aries for {}", stack.get_label()))
                .await;
            select::aries_select(state, tried, task, env, *bool)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        SelectMode::Planning(Planner::CChoice(config)) => {
            log.debug(format!("select with c-choice for {}", stack.get_label()))
                .await;
            select::c_choice_select(state, tried, task, env, *config)
                .await
                .map_err(|e| e.chain("planning_select"))?
        }
        SelectMode::Planning(Planner::RAEPlan(config)) => {
            log.debug(format!("select with c-choice for {}", stack.get_label()))
                .await;
            select::rae_plan_select(state, tried, task, env, *config)
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
    list![LValue::from(SUCCESS), args.into()]
}
#[scheme_fn]
pub fn failure(args: &[LValue]) -> LValue {
    list![LValue::from(FAILURE), args.into()]
}

#[scheme_fn]
pub fn is_failure(list: Vec<LValue>) -> Result<bool, LRuntimeError> {
    if let LValue::Symbol(s) = &list[0] {
        match s.as_str() {
            SUCCESS => Ok(false),
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
            SUCCESS => Ok(true),
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
